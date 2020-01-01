module RaiiLang.Cir

open RaiiLang.Helpers

type CUni =
  // `*x`
  | CDerefUni

  // `&x`
  | CRefUni

type CBin =
  | CEqBin
  | CAddBin
  | CAssignBin

type CTy =
  | CVoidTy
  | CIntTy
  | CPtrTy
    of CTy
  | CFunTy
    of CTy list * CTy

[<Struct>]
type CParam =
  | CParam
    of name:string * CTy

type CTerm =
  | CInt
    of string

  | CName
    of string

  | CUni
    of CUni * CTerm

  | CCall
    of CTerm * CTerm list

  | CBin
    of CBin * CTerm * CTerm

type CStmt =
  | CTermStmt
    of CTerm

  | CLocalStmt
    of string * CTy * CTerm

  | CReturn
    of CTerm option

type CDecl =
  | CFnDecl
    of string
      * CParam list
      * CTy
      * CStmt list
