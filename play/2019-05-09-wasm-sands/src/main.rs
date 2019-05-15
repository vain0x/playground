pub mod binary;

use std::fs::File;
use std::io::*;

fn create_binary_ast() -> binary::Root {
    use binary::*;

    Root {
        ty_section: Some(TySection {
            func_tys: vec![FuncTy {
                param_tys: vec![],
                result_tys: vec![ValTy::I32],
            }],
        }),
        func_section: Some(FuncSection {
            func_ty_indexes: vec![0],
        }),
        export_section: Some(ExportSection {
            exports: vec![("f".to_string(), ExportDesc::Func(0))],
        }),
        code_section: Some(CodeSection {
            codes: vec![Code {
                locals: vec![],
                body: vec![Instr::I32Const(2), Instr::I32Const(3), Instr::I32Add],
            }],
        }),
    }
}

fn main() {
    let mut buf: Vec<u8> = vec![];

    let root = create_binary_ast();
    binary::write_module(&mut buf, &root);

    let mut f = File::create("out/main.wasm").unwrap();
    f.write_all(&buf).unwrap();
    f.flush().unwrap();

    // For debugging, output hex-encoded wasm.
    let mut f = File::create("out/main.txt").unwrap();
    let mut i = 0;
    while i < buf.len() {
        for _ in 0..16 {
            if i >= buf.len() {
                break;
            }
            write!(f, "{:02X} ", buf[i]).unwrap();
            i += 1;
        }
        write!(f, "\n").unwrap();
    }
    f.flush().unwrap();
}
