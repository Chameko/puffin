use crossbeam_channel::{select, unbounded, Receiver, Sender};
use notify::{Config, Event, INotifyWatcher, RecommendedWatcher, Watcher, EventKind, event::{ModifyKind, CreateKind, RemoveKind}};
use std::thread;

use crate::path::{AbsPath, AbsPathBuf};

/// Messages for the monitor to send to the notify thread
#[derive(Debug)]
pub enum ForegroundMessage {
    /// Add a path to be monitored
    AddPath(AbsPathBuf),
    /// Remove a path from being monitored
    RemovePath(AbsPathBuf),
}

/// Messages from the monitor for the [`UpdateFn`]
#[derive(Debug)]
pub enum MonitorMessage {
    /// File was created
    CreatedFile(AbsPathBuf),
    /// File was modified
    ModifiedFile(AbsPathBuf),
    /// File was removed
    RemovedFile(AbsPathBuf),
    /// File was renamed
    RenamedFile(AbsPathBuf, RenameMode),
    /// A Folder was created
    CreatedFolder(AbsPathBuf),
    /// A folder was removed
    RemovedFolder(AbsPathBuf),
}

/// The specifics on how a file was renamed. Used to prevent the re-exporting of notify
#[derive(Debug, Clone)]
pub enum RenameMode {
    /// The catch all case, used when a specific event is unknown
    Any,
    /// An event on the file or folder resulting from a rename
    To,
    /// An event on the file or folder that was renamed
    From,
    /// Other
    Other,
}

impl TryFrom<notify::event::RenameMode> for RenameMode {
    type Error = ();

    fn try_from(value: notify::event::RenameMode) -> Result<Self, Self::Error> {
        use notify::event;
        match value {
            event::RenameMode::Any => Ok(RenameMode::Any),
            event::RenameMode::From => Ok(RenameMode::From),
            event::RenameMode::To => Ok(RenameMode::To),
            event::RenameMode::Other => Ok(RenameMode::Other),
            event::RenameMode::Both => Err(())
        }
    }
}

/// Type alias for a function that recieves a monitor message and relays it
pub type UpdateFn = Box<dyn Fn(MonitorMessage) + Send>;

/// A monitor for file changes
#[derive(Debug)]
pub struct Monitor {
    /// Join handle to the notify thread
    _handle: thread::JoinHandle<()>,
    /// Sender to send messages to the notify thread
    sender: Sender<ForegroundMessage>,
}

impl Monitor {
    /// Create a new monitor
    pub fn new(update: UpdateFn) -> notify::Result<Self> {
        let (sender, reciever) = unbounded();

        let mut notify = NotifyThread::new(reciever, update)?;

        let _handle = std::thread::Builder::new()
            .name(String::from("notify-thread"))
            .spawn(move || notify.run())
            .expect("failed to spawn notify background thread");

        Ok(Self { _handle, sender })
    }

    /// Add a file/path for the monitor to watch
    pub fn add_file(&self, path: &AbsPath) {
        let msg = ForegroundMessage::AddPath(path.to_path_buf());
        self.sender.send(msg).unwrap();
    }
}

/// Possible events for the [`NotifyThread`] to run into
#[derive(Debug)]
enum NotifyThreadEvent {
    /// Message from the foreground
    ForegroundMessage(ForegroundMessage),
    /// An event from notify
    NotifyEvent(Event),
}

/// The notify thread. It acts as the itermediate between the monitor and the watcher.
pub struct NotifyThread {
    /// The watcher that watches for changes in the file system
    watcher: INotifyWatcher,
    /// Reciever for watcher messages
    watcher_reciever: Receiver<notify::Result<Event>>,
    /// Reciever for monitor messages
    monitor_reciever: Receiver<ForegroundMessage>,
    /// The function to be called to supply an update
    update: UpdateFn,
}

impl NotifyThread {
    /// Create a new notify thread
    pub fn new(
        monitor_reciever: Receiver<ForegroundMessage>,
        update: UpdateFn,
    ) -> notify::Result<Self> {
        let (sender, reciever) = unbounded();

        let watcher = RecommendedWatcher::new(sender, Config::default())?;

        Ok(Self {
            watcher,
            monitor_reciever,
            watcher_reciever: reciever,
            update,
        })
    }

    /// Run the thread
    pub fn run(&mut self) {
        while let Some(event) = self.next_event() {
            match event {
                NotifyThreadEvent::ForegroundMessage(fmsg) => {
                    match fmsg {
                        ForegroundMessage::AddPath(path) => {
                            self.watcher.watch(path.as_ref(), notify::RecursiveMode::Recursive);
                        },
                        ForegroundMessage::RemovePath(path) => {
                            self.watcher.unwatch(path.as_ref());
                        }
                    }
                }
                NotifyThreadEvent::NotifyEvent(event) => {
                    match event {
                        Event{ kind: EventKind::Create(CreateKind::File), paths, ..} => {
                            for file in paths {
                                (self.update)(MonitorMessage::CreatedFile(AbsPathBuf::try_from(file).unwrap()));
                            }
                        }
                        Event{ kind: EventKind::Create(CreateKind::Folder), paths, ..} => {
                            for file in paths {
                                (self.update)(MonitorMessage::CreatedFile(AbsPathBuf::try_from(file).unwrap()));
                            }
                        }
                        Event{ kind: EventKind::Modify(ModifyKind::Data(_)), paths, ..} => {
                            for file in paths {
                                (self.update)(MonitorMessage::ModifiedFile(AbsPathBuf::try_from(file).unwrap()));
                            }
                        }
                        // When notify emits a both event it means that the file paths are returned in the format of (file from), (file to)
                        // as we only export single file paths, we instead map this to the relevent RenameMode
                        Event{ kind: EventKind::Modify(ModifyKind::Name(notify::event::RenameMode::Both)), paths, ..} => {
                            let mut to = false;
                            for file in paths {
                                if to {
                                    (self.update)(MonitorMessage::RenamedFile(AbsPathBuf::try_from(file).unwrap(), RenameMode::To));
                                } else {
                                    (self.update)(MonitorMessage::RenamedFile(AbsPathBuf::try_from(file).unwrap(), RenameMode::From));
                                }
                                to = !to;
                            }
                        }
                        Event{ kind: EventKind::Modify(ModifyKind::Name(r)), paths, ..} if r != notify::event::RenameMode::Both => {
                            for file in paths {
                                (self.update)(MonitorMessage::RenamedFile(AbsPathBuf::try_from(file).unwrap(), RenameMode::try_from(r).unwrap()));
                            }
                        }
                        Event{ kind: EventKind::Remove(RemoveKind::File), paths, ..} => {
                            for file in paths {
                                (self.update)(MonitorMessage::RemovedFile(AbsPathBuf::try_from(file).unwrap()));
                            }
                        },
                        Event{ kind: EventKind::Remove(RemoveKind::Folder), paths, ..} => {
                            for file in paths {
                                (self.update)(MonitorMessage::RemovedFolder(AbsPathBuf::try_from(file).unwrap()));
                            }
                        },
                        _ => {}
                    }
                },
            }
        }
    }

    /// Get the next event from either the [`Monitor`] or the watcher
    fn next_event(&self) -> Option<NotifyThreadEvent> {
        select! {
            recv(self.monitor_reciever) -> it => it.ok().map(|fmsg| NotifyThreadEvent::ForegroundMessage(fmsg)),
            recv(self.watcher_reciever) -> it => it.ok().map(|res| NotifyThreadEvent::NotifyEvent(res.unwrap()))
        }
    }
}

