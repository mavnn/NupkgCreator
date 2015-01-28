module Paket.PackageFile.Test

open System.IO
open Paket
open Paket.Rop
open FsUnit
open NUnit.Framework

[<Literal>]
let FileBasedShortDesc = """type file
id My.Thing
version 1.0
authors Bob McBob
description A short description
"""

[<Literal>]
let FileBasedLongDesc = """type file
id My.Thing
version 1.0
authors Bob McBob
description
    A longer description
    on two lines.
"""

[<Literal>]
let FileBasedLongDesc2 = """type file
id My.Thing
authors Bob McBob
description
    A longer description
    on two lines.
version 1.0
"""

[<Literal>]
let FileBasedLongDesc3 = """type file
id My.Thing
authors Bob McBob
description
    A longer description
    on two lines.
version
    1.0
"""

let v1 = Paket.SemVer.Parse "1.0"

let strToStream (str : string) =
    let mem = new MemoryStream()
    let writer = new StreamWriter(mem)
    writer.Write(str)
    writer.Flush()
    mem.Seek(0L, SeekOrigin.Begin) |> ignore
    mem

[<TestCase(FileBasedShortDesc, "A short description")>]
[<TestCase(FileBasedLongDesc, "A longer description\non two lines.")>]
[<TestCase(FileBasedLongDesc2, "A longer description\non two lines.")>]
[<TestCase(FileBasedLongDesc3, "A longer description\non two lines.")>]
let ``Parsing minimal file based packages works`` (fileContent, desc) =
    let result =
        Paket.PackagingConfig.Parse (strToStream fileContent)
        |> returnOrFail
    result.Id |> should equal "My.Thing"
    result.Version |> should equal v1
    result.Authors |> should equal ["Bob McBob"]
    result.Description |> should equal desc

[<Literal>]
let Invalid1 = """type fil
id My.Thing
version 1.0
authors Bob McBob
description A short description
"""

[<Literal>]
let Invalid2 = """type file
id My.Thing
authors Bob McBob
description A short description
"""

[<Literal>]
let Invalid3 = """type file
id My.Thing
version 1.0
description A short description
"""

[<TestCase(Invalid1)>]
[<TestCase(Invalid2)>]
[<TestCase(Invalid3)>]
let ``Invalid file input recognised as invalid`` (fileContent : string) =
    fileContent |> strToStream |> PackagingConfig.Parse |> (function | Failure _ -> true | Success _ -> false)
    |> should be True

[<Literal>]
let Dependency1 = """type file
id My.Thing
authors Bob McBob
description
    A longer description
    on two lines.
version
    1.0
dependencies
     FSharp.Core 4.3.1
     My.OtherThing
"""

[<TestCase(Dependency1)>]
let ``Detect dependencies correctly`` fileContent =
    let sut =
        fileContent |> strToStream |> PackagingConfig.Parse
        |> returnOrFail
    match sut.Dependencies with
    | Some [name1,range1;name2,range2] ->
        name1 |> should equal "FSharp.Core"
        range1.Range |> should equal (Specific (SemVer.Parse "4.3.1"))
        name2 |> should equal "My.OtherThing"
        range2.Range |> should equal (Minimum (SemVer.Parse "0"))
    | Some _
    | None ->
        Assert.Fail()