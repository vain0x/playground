// Representation.

use std::io::Write;

type TyIndex = usize;

type FuncIndex = usize;

type LocalIndex = usize;

#[derive(Clone, Copy, Debug)]
pub(crate) enum Instr {
    Return,
    I32Const(i32),
    I32Add,
}

#[derive(Clone, Copy, Debug)]
pub(crate) enum ValTy {
    I32,
    I64,
    U32,
    U64,
}

#[derive(Clone, Debug)]
pub(crate) struct FuncTy {
    pub param_tys: Vec<ValTy>,
    pub result_tys: Vec<ValTy>,
}

#[derive(Clone, Debug)]
pub(crate) struct Local {
    pub count: usize,
    pub ty: ValTy,
}

/// a.k.a. Code
#[derive(Clone, Debug)]
pub(crate) struct Code {
    pub locals: Vec<Local>,
    pub body: Vec<Instr>,
}

#[derive(Clone, Debug)]
pub(crate) struct TySection {
    pub func_tys: Vec<FuncTy>,
}

#[derive(Clone, Debug)]
pub(crate) struct FuncSection {
    pub func_ty_indexes: Vec<TyIndex>,
}

#[derive(Clone, Debug)]
pub(crate) enum ExportDesc {
    Func(FuncIndex),
}

#[derive(Clone, Debug)]
pub(crate) struct ExportSection {
    pub exports: Vec<(String, ExportDesc)>,
}

#[derive(Clone, Debug)]
pub(crate) struct CodeSection {
    pub codes: Vec<Code>,
}

#[derive(Clone, Debug)]
pub(crate) struct Root {
    pub ty_section: Option<TySection>,
    pub func_section: Option<FuncSection>,
    pub export_section: Option<ExportSection>,
    pub code_section: Option<CodeSection>,
}

pub(crate) struct Assembler<W: Write> {
    out: W,
}

fn write_leb128(buf: &mut Vec<u8>, value: i64) {
    let mut x = value;
    loop {
        let v = x >> 7;
        let b = x & 0x7f;

        if v == 0 && b & 0x40 == 0 || v == -1 && b & 0x40 != 0 {
            buf.push(b as u8);
            return;
        }

        buf.push(b as u8 | 0x80);
        x = v;
    }
}

fn write_i32(buf: &mut Vec<u8>, value: i32) {
    write_leb128(buf, value as i64);
}

fn write_u32(buf: &mut Vec<u8>, value: u32) {
    write_leb128(buf, value as i64);
}

fn write_index(buf: &mut Vec<u8>, value: usize) {
    write_u32(buf, value as u32);
}

fn write_name(buf: &mut Vec<u8>, value: &str) {
    buf.push(value.len() as u8);
    for b in value.bytes() {
        buf.push(b);
    }
}

fn val_ty_to_byte(val_ty: ValTy) -> u8 {
    match val_ty {
        ValTy::I32 => 0x7F,
        ValTy::I64 => 0x7E,
        ValTy::U32 => 0x7D,
        ValTy::U64 => 0x7C,
    }
}

fn write_val_ty(buf: &mut Vec<u8>, val_ty: ValTy) {
    buf.push(val_ty_to_byte(val_ty));
}

fn write_vec_val_ty(buf: &mut Vec<u8>, val_tys: &[ValTy]) {
    buf.push(val_tys.len() as u8);
    for &val_ty in val_tys {
        write_val_ty(buf, val_ty);
    }
}

fn write_func_ty(buf: &mut Vec<u8>, ty: &FuncTy) {
    buf.push(0x60);
    write_vec_val_ty(buf, &ty.param_tys);
    write_vec_val_ty(buf, &ty.result_tys);
}

fn write_instr(buf: &mut Vec<u8>, instr: Instr) {
    match instr {
        Instr::Return => buf.push(0x0F),
        Instr::I32Const(value) => {
            buf.push(0x41);
            write_i32(buf, value);
        }
        Instr::I32Add => buf.push(0x6A),
    }
}

fn write_expr(buf: &mut Vec<u8>, expr: &[Instr]) {
    for &instr in expr {
        write_instr(buf, instr);
    }
    // end
    buf.push(0x0B);
}

fn write_section(buf: &mut Vec<u8>, n: usize, write: impl Fn(&mut Vec<u8>)) {
    let mut content_buf = vec![];
    write(&mut content_buf);

    buf.push(n as u8);
    write_u32(buf, content_buf.len() as u32);
    buf.extend(content_buf.drain(..));
}

pub(crate) fn write_module(buf: &mut Vec<u8>, root: &Root) {
    // magic, version
    buf.extend(&[0x00, 0x61, 0x73, 0x6D, 0x01, 0x00, 0x00, 0x00]);

    if let Some(ty_section) = &root.ty_section {
        write_section(buf, 1, |buf| {
            // vec(func-type)
            buf.push(ty_section.func_tys.len() as u8);
            for func_ty in &ty_section.func_tys {
                write_func_ty(buf, func_ty);
            }
        });
    }

    if let Some(func_section) = &root.func_section {
        write_section(buf, 3, |buf| {
            // vec(type-index)
            buf.push(func_section.func_ty_indexes.len() as u8);
            for &func_ty_index in &func_section.func_ty_indexes {
                write_index(buf, func_ty_index);
            }
        });
    }

    if let Some(export_section) = &root.export_section {
        write_section(buf, 7, |buf| {
            // vec(export)
            buf.push(export_section.exports.len() as u8);
            for (name, desc) in &export_section.exports {
                write_name(buf, &name);

                match desc {
                    &ExportDesc::Func(func_index) => {
                        buf.push(0x00);
                        write_index(buf, func_index);
                    }
                }
            }
        });
    }

    if let Some(code_section) = &root.code_section {
        write_section(buf, 10, |buf| {
            let mut content_buf = vec![];

            // vec(code)
            buf.push(code_section.codes.len() as u8);
            for code in &code_section.codes {
                // write to temporary buffer to calculate content size
                debug_assert!(content_buf.is_empty());

                // vec(locals)
                content_buf.push(code.locals.len() as u8);
                for local in &code.locals {
                    write_u32(&mut content_buf, local.count as u32);
                    write_val_ty(&mut content_buf, local.ty);
                }

                write_expr(&mut content_buf, &code.body);

                buf.push(content_buf.len() as u8);
                buf.extend(content_buf.drain(..));
            }
        });
    }
}
