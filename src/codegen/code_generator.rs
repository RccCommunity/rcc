use std::collections::HashMap;

use llvm::prelude::{LLVMBuilderRef, LLVMContextRef, LLVMValueRef};

use crate::parser::expr::{Expr, Func};

pub struct CodeGenerator;
/*
When you write out instructions in LLVM, you get back `LLVMValueRef`s. You
can then use these references in other instructions.
*/
impl CodeGenerator {
    /// # Safety
    ///
    /// This function should not be called before the horsemen are ready.
    unsafe fn codegen_expr(
        _context: LLVMContextRef,
        _builder: LLVMBuilderRef,
        _names: &mut HashMap<String, LLVMValueRef>,
        expr: Expr,
    ) -> LLVMValueRef {
        match expr {
            Expr::Error => todo!(),
            Expr::Literal(_) => todo!(),
            Expr::List(_) => todo!(),
            Expr::Local(_) => todo!(),
            Expr::Let(_, _, _) => todo!(),
            Expr::Then(_, _) => todo!(),
            Expr::Call(_, _) => todo!(),
            Expr::Block(_) => todo!(),
            Expr::If(_, _, _) => todo!(),
            Expr::WhileBlock(_, _) => todo!(),
            Expr::Operator(_) => todo!(),
        }
        // match expr {
        //     Expr::Literal(int_literal) => {
        //         let int_type = llvm::core::LLVMInt64TypeInContext(context);
        //         llvm::core::LLVMConstInt(int_type, int_literal.parse().unwrap(), 0)
        //     }

        //     Expr::Add(lhs, rhs) => {
        //         let lhs = Self::codegen_expr(context, builder, names, *lhs);
        //         let rhs = Self::codegen_expr(context, builder, names, *rhs);

        //         let name = CString::new("addtmp").unwrap();
        //         llvm::core::LLVMBuildAdd(builder, lhs, rhs, name.as_ptr())
        //     }

        //     Expr::Sub(lhs, rhs) => {
        //         let lhs = Self::codegen_expr(context, builder, names, *lhs);
        //         let rhs = Self::codegen_expr(context, builder, names, *rhs);

        //         let name = CString::new("subtmp").unwrap();
        //         llvm::core::LLVMBuildSub(builder, lhs, rhs, name.as_ptr())
        //     }

        //     Expr::Mul(lhs, rhs) => {
        //         let lhs = Self::codegen_expr(context, builder, names, *lhs);
        //         let rhs = Self::codegen_expr(context, builder, names, *rhs);

        //         let name = CString::new("multmp").unwrap();
        //         llvm::core::LLVMBuildMul(builder, lhs, rhs, name.as_ptr())
        //     }

        //     Expr::Div(lhs, rhs) => {
        //         let lhs = Self::codegen_expr(context, builder, names, *lhs);
        //         let rhs = Self::codegen_expr(context, builder, names, *rhs);

        //         let name = CString::new("divtmp").unwrap();
        //         llvm::core::LLVMBuildUDiv(builder, lhs, rhs, name.as_ptr())
        //     }
        //     Expr::Ref(name) => *names.get(&name).unwrap(),

        //     Expr::Assign(name, expr) => {
        //         let new_value = Self::codegen_expr(context, builder, names, *expr);
        //         names.insert(name, new_value);
        //         new_value
        //     }
        // }
    }
    /// # Safety
    ///
    /// This function should not be called before the horsemen are ready.
    pub unsafe fn codegen(_input: &HashMap<&str, Func>) {
        // let context = llvm::core::LLVMContextCreate();
        // let module = llvm::core::LLVMModuleCreateWithName(b"example_module\0".as_ptr() as *const _);
        // let builder = llvm::core::LLVMCreateBuilderInContext(context);

        // // In LLVM, you get your types from functions.
        // let int_type = llvm::core::LLVMInt64TypeInContext(context);
        // let function_type = llvm::core::LLVMFunctionType(int_type, ptr::null_mut(), 0, 0);
        // let function =
        //     llvm::core::LLVMAddFunction(module, b"main\0".as_ptr() as *const _, function_type);

        // let entry_name = CString::new("entry").unwrap();
        // let bb = llvm::core::LLVMAppendBasicBlockInContext(context, function, entry_name.as_ptr());
        // llvm::core::LLVMPositionBuilderAtEnd(builder, bb);

        // // The juicy part: construct a `LLVMValue` from a Rust value:
        // let zero = llvm::core::LLVMConstInt(int_type, 0, 0);

        // let mut names = HashMap::new();
        // let mut return_value = zero; // return value on empty program
        // for expr in input {
        //     return_value = Self::codegen_expr(context, builder, &mut names, expr);
        // }
        // llvm::core::LLVMBuildRet(builder, return_value);
        // // Instead of dumping to stdout, let's write out the IR to `out.ll`
        // let out_file = CString::new("out.ll").unwrap();
        // llvm::core::LLVMPrintModuleToFile(module, out_file.as_ptr(), ptr::null_mut());

        // llvm::core::LLVMDisposeBuilder(builder);
        // llvm::core::LLVMDisposeModule(module);
        // llvm::core::LLVMContextDispose(context);
    }
}
