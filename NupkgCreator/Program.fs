module NupkgWriter

open Paket
open Paket.Rop
open System.IO

let fs = File.Open("paket.pack", FileMode.Open)

let metaData = PackagingConfig.Parse fs |> returnOrFail

NupkgWriter.writeZip
    metaData
    (Some @"C:\rip\Algebra.Boolean\Algebra.Boolean\bin\Debug")
    None
    "."