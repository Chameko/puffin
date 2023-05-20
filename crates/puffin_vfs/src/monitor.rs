use crossbeam_channel::{select, unbounded, Receiver, Sender};
use notify::{Config, Event, INotifyWatcher, RecommendedWatcher, Watcher};
use std::{str::FromStr, thread};

use crate::{
    path::{AbsPath, AbsPathBuf},
    VFSError,
};

#[derive(Debug)]
pub enum ForegroundMessage {
    AddPath(AbsPathBuf),
}

pub enum MonitorMessage {
    CreatedFile(AbsPathBuf),
    ModifiedFile(AbsPathBuf),
    RemovedFile(AbsPathBuf),
    RenamedFile(AbsPathBuf),
}

pub type UpdateFn = Box<dyn Fn(MonitorMessage) + Send>;

/// A monitor for file changes
#[derive(Debug)]
pub struct Monitor {
    _handle: thread::JoinHandle<()>,
    sender: Sender<ForegroundMessage>,
}

impl Monitor {
    /// Create a new monitor
    fn new(update: UpdateFn) -> notify::Result<Self> {
        let (sender, reciever) = unbounded();

        let mut notify = NotifyThread::new(reciever, update)?;

        let _handle = std::thread::Builder::new()
            .name(String::from("notify-thread"))
            .spawn(move || notify.run())
            .expect("failed to spawn notify background thread");

        Ok(Self { _handle, sender })
    }

    fn add_file(&self, path: &AbsPath) {
        let msg = ForegroundMessage::AddPath(path.into());
        self.sender.send(msg).unwrap();
    }
}

#[derive(Debug)]
enum NotifyThreadEvent {
    ForegroundMessage(ForegroundMessage),
    NotifyEvent(Event),
}

pub struct NotifyThread {
    watcher: INotifyWatcher,
    watcher_reciever: Receiver<notify::Result<Event>>,
    monitor_reciever: Receiver<ForegroundMessage>,
    update: UpdateFn,
    watch: Vec<AbsPathBuf>,
}

impl NotifyThread {
    pub fn new(
        monitor_reciever: Receiver<ForegroundMessage>,
        update: UpdateFn,
    ) -> notify::Result<Self> {
        let (sender, reciever) = unbounded();

        let mut watcher = RecommendedWatcher::new(sender, Config::default())?;

        Ok(Self {
            watcher,
            monitor_reciever,
            watcher_reciever: reciever,
            update,
            watch: vec![],
        })
    }

    pub fn run(&mut self) {
        while let Some(event) = self.next_event() {
            match event {
                NotifyThreadEvent::ForegroundMessage(fmsg) => {
                    println!("Foreground message: {:?}", fmsg);
                    if let ForegroundMessage::AddPath(path) = fmsg {
                        self.watcher.watch(path.as_ref(), notify::RecursiveMode::Recursive);
                    }
                }
                NotifyThreadEvent::NotifyEvent(event) => println!("Event: {:?}", event),
            }
        }
    }

    fn next_event(&self) -> Option<NotifyThreadEvent> {
        select! {
            recv(self.monitor_reciever) -> it => it.ok().map(NotifyThreadEvent::ForegroundMessage),
            recv(self.watcher_reciever) -> it => it.ok().map(|res| NotifyThreadEvent::NotifyEvent(res.unwrap()))
        }
    }
}

#[cfg(test)]
mod tests {
    use std::str::FromStr;

    use crate::path::AbsPath;

    use super::{Monitor, MonitorMessage};

    #[test]
    fn basic() {
        let update = Box::new(|monitor: MonitorMessage| println!("Amazing"));
        let monitor = Monitor::new(update).expect("Failed to create monitor");

        let path = std::path::PathBuf::from_str("/home/chameko/projects/puffin/crates/puffin_vfs/foo.txt").unwrap();
        monitor.add_file(AbsPath::new(&path).unwrap());

        monitor._handle.join().expect("Failed to join");
    }
}
