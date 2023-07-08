use puffin_ast::ast::{Root, stmt::*, expr::*, prelude::* };
use puffin_vm::{vm::VM, instruction::Opcode, value::Value};

/// Used to pass around the data that will be added to the VM
type VMData = (Vec<u8>, Vec<Value>);

/// Generates the bytecode from an AST
pub fn generate_bytecode(ast: Root) -> VM {
    let mut vm_data = (vec![], vec![]);
    for stmt in ast.contents {
        match stmt {
            Stmt::ExprStmt(expr) => expr_bytecode_gen(expr, &mut vm_data),
            _ => todo!()
        }
    }
    vm_data.0.push(Opcode::HLT as u8);
    VM::with_data(vm_data)
}

/// Gnerates the bytecode for an expression
fn expr_bytecode_gen(expr: Expr, vm_data: &mut VMData) {
    match expr {
        Expr::Binary(bin) => {
            match *bin {
                BinaryExpr::Add(expr) => {
                    expr_bytecode_gen(expr.a, vm_data);
                    expr_bytecode_gen(expr.b, vm_data);
                    vm_data.0.push(Opcode::ADD as u8)
                },
                BinaryExpr::Subtract(expr) => {
                    expr_bytecode_gen(expr.a, vm_data);
                    expr_bytecode_gen(expr.b, vm_data);
                    vm_data.0.push(Opcode::SUB as u8)
                },
                BinaryExpr::Multiply(expr) => {
                    expr_bytecode_gen(expr.a, vm_data);
                    expr_bytecode_gen(expr.b, vm_data);
                    vm_data.0.push(Opcode::MUL as u8)
                },
                BinaryExpr::Divide(expr) => {
                    expr_bytecode_gen(expr.a, vm_data);
                    expr_bytecode_gen(expr.b, vm_data);
                    vm_data.0.push(Opcode::DIV as u8)
                },
                _ => todo!()
            }
        },
        Expr::Lit(lit) => {
            match lit {
                Literal::Int(i) => {
                    vm_data.1.push(Value::Number(i.int));
                    vm_data.0.push(Opcode::LOAD as u8);
                    vm_data.0.push((vm_data.1.len() - 1) as u8)
                },
                Literal::Float(f) => {
                    vm_data.1.push(Value::Decimal(f.float));
                    vm_data.0.push(Opcode::LOAD as u8);
                    vm_data.0.push((vm_data.1.len() - 1) as u8)
                },
                _ => todo!()
            }
        }
        _ => todo!()
    }
}

#[cfg(test)]
mod codegen_test {
    use puffin_ast::ast::lit::{literal_expr, IntLiteral};

    use super::*;

    #[test]
    fn simple_maths() {
        let mut ast = Root::new();
        ast.contents = vec![
            binary_expr_stmt(AddBinaryExpr::test_node(literal_expr(IntLiteral::test_node(1)), literal_expr(IntLiteral::test_node(2))))
        ];
        let mut vm = generate_bytecode(ast);
        vm.run_with_stack_trace();
        assert_eq!(vm.stack[0], Value::Number(3))
    }
}
