module NupkgWriter

let dll = "This is a dll!"
open Ionic.Zip
open System.IO
open System.Xml.Linq

let nuspecId = "nuspec"
let corePropsId = "coreProp"

type NuspecMetaData =
    {
        Id : string
        Version : string
        Authors : string
        Description : string
    }

let contentTypePath = "/[Content_Types].xml"

let contentTypeDoc fileList =
    let declaration = XDeclaration("1.0", "UTF-8", "yes")
    let ns = XNamespace.Get "http://schemas.openxmlformats.org/package/2006/content-types"
    let root = XElement(ns + "Types")

    let defaultNode extension contentType =
        let def = XElement(ns + "Default")
        def.SetAttributeValue (XName.Get "Extension", extension)
        def.SetAttributeValue (XName.Get "ContentType", contentType)
        def

    let knownExtensions =
        Map.ofList [
            "rels", "application/vnd.openxmlformats-package.relationships+xml"
            "psmdcp", "application/vnd.openxmlformats-package.core-properties+xml"
        ]

    let ext path = Path.GetExtension(path).TrimStart([|'.'|]).ToLowerInvariant()

    let fType ext = 
        knownExtensions |> Map.tryFind ext
        |> function | Some ft -> ft | None -> "application/octet"

    let contentTypes =
        fileList
        |> Seq.map (fun f ->
                        let e = ext f
                        e, fType e)
        |> Seq.distinct
        |> Seq.iter (fun (ex, ct) -> defaultNode ex ct |> root.Add)

    XDocument(declaration, box root)

let nuspecPath metadata =
    sprintf "/%s.nuspec" metadata.Id

let nuspecDoc metadata =
    let declaration = XDeclaration("1.0", "UTF-8", "yes")
    let ns = XNamespace.Get "http://schemas.microsoft.com/packaging/2011/10/nuspec.xsd"
    let root = XElement(ns + "package")

    let addChildNode (parent : XElement) name value =
        let node = XElement(ns + name)
        node.SetValue value
        parent.Add node

    let metadataNode = XElement(ns + "metadata")
    root.Add metadataNode
    
    let (!!) =
        addChildNode metadataNode

    !! "id" metadata.Id
    !! "version" metadata.Version
    !! "authors" metadata.Authors
    !! "description" metadata.Description

    XDocument(declaration, box root)

let corePropsPath =
    sprintf "/package/services/metadata/core-properties/%s.psmdcp" corePropsId

let corePropsDoc metadata =
    let declaration = XDeclaration("1.0", "UTF-8", "yes")
    let ns = XNamespace.Get "http://schemas.openxmlformats.org/package/2006/metadata/core-properties"
    let dc = XNamespace.Get "http://purl.org/dc/elements/1.1/"
    let dcterms = XNamespace.Get "http://purl.org/dc/terms/"
    let xsi = XNamespace.Get "http://www.w3.org/2001/XMLSchema-instance"
    let root = XElement(ns + "Relationships",
                XAttribute(XName.Get "xmlns", ns.NamespaceName),
                XAttribute(XNamespace.Xmlns + "dc", dc.NamespaceName),
                XAttribute(XNamespace.Xmlns + "dcterms", dcterms.NamespaceName),
                XAttribute(XNamespace.Xmlns + "xsi", xsi.NamespaceName))
    let (!!) (ns : XNamespace) name value =
        let node = XElement(ns + name)
        node.SetValue value
        root.Add node
    !! dc "creator" metadata.Authors
    !! dc "description" metadata.Description
    !! dc "identifier" metadata.Id
    !! ns "version" metadata.Version
    XElement(ns + "keywords") |> root.Add
    !! dc "title" metadata.Id
    !! ns "lastModifiedBy" "paket"

    XDocument(declaration, box root)

let relsPath = "/_rels/.rels"

let relsDoc metadata =
    let declaration = XDeclaration("1.0", "UTF-8", "yes")
    let ns = XNamespace.Get "http://schemas.openxmlformats.org/package/2006/relationships"
    let root = XElement(ns + "Relationships")

    let r type' target id' =
        let rel = XElement(ns + "Relationship")
        rel.SetAttributeValue(XName.Get "Type", type')
        rel.SetAttributeValue(XName.Get "Target", target)
        rel.SetAttributeValue(XName.Get "Id", id')
        root.Add rel

    r
        "http://schemas.microsoft.com/packaging/2010/07/manifest"
        (nuspecPath metadata)
        nuspecId

    r
        "http://schemas.openxmlformats.org/package/2006/relationships/metadata/core-properties"
        corePropsPath
        corePropsId

    XDocument(declaration, box root)

let xDocWriter (xDoc : XDocument) (stream : System.IO.Stream) =
    let xmlWriter = new System.Xml.XmlTextWriter(stream, System.Text.Encoding.UTF8)
    xDoc.WriteTo xmlWriter
    xmlWriter.Flush()

let writeNupkg metadata =
    [
        nuspecPath metadata, nuspecDoc metadata |> xDocWriter
        corePropsPath, corePropsDoc metadata |> xDocWriter
        relsPath, relsDoc metadata |> xDocWriter
    ]

if System.IO.File.Exists "My.Thing.nupkg" then
    System.IO.File.Delete "My.Thing.nupkg"

let writeZip libDir contentDir =
    use zipFile = new ZipFile("My.Thing.nupkg")
    let addEntry (zipFile : ZipFile) path writer =
        let writeDel _ stream =
            writer stream
        zipFile.AddEntry(path, WriteDelegate(writeDel))

    zipFile.AddDirectory(libDir, "lib") |> ignore
    zipFile.AddDirectory(contentDir, "Content") |> ignore

    writeNupkg { Id = "My.Thing"; Version = "1.0"; Authors = "Me"; Description = "A description" }
    |> List.iter (fun (path, writer) -> addEntry zipFile path writer |> ignore)

    let fileList =
        zipFile.Entries
        |> Seq.filter (fun e -> not e.IsDirectory)
        |> Seq.map (fun e -> e.FileName)

    let contentTypesDoc =
        addEntry zipFile contentTypePath (contentTypeDoc fileList |> xDocWriter)

    zipFile.Save()

writeZip @"C:\rip\Algebra.Boolean\Algebra.Boolean\bin\Debug" @"C:\rip\Build.Tools\stylesheets" 
printfn "Done"
System.Console.ReadLine() |> ignore