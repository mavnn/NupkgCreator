module Paket.NupkgWriter

open System.IO
open System.Xml.Linq
open Ionic.Zip
open Paket

let nuspecId = "nuspec"
let corePropsId = "coreProp"

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

    let (!!?) nodeName strOpt =
        match strOpt with
        | Some s ->
            addChildNode metadataNode nodeName s
        | None ->
            ()

    let buildDependencyNode (Id, (VersionRequirement (range, _))) =
        let dep = XElement(ns + "dependency")
        dep.SetAttributeValue(XName.Get "id", Id)
        let versionStr =
            match range with
            | Minimum v ->
                v.ToString()
            | GreaterThan v ->
                sprintf "(%A,)" v
            | Maximum v ->
                sprintf "(,%A]" v
            | LessThan v ->
                sprintf "(,%A)" v
            | OverrideAll v
            | Specific v ->
                sprintf "[%A]" v
            | Range (fromB, from, to', toB) ->
                let opening =
                    match fromB with
                    | VersionRangeBound.Excluding -> "("
                    | VersionRangeBound.Including -> "["
                let closing =
                    match toB with
                    | VersionRangeBound.Excluding -> ")"
                    | VersionRangeBound.Including -> "]"
                sprintf "%s%A, %A%s" opening from to' closing
        match versionStr with
        | "0" -> ()
        | _ ->
            dep.SetAttributeValue(XName.Get "version", versionStr)
        dep

    let buildDependenciesNode dependencyList =
        let d = XElement(ns + "dependencies")
        dependencyList
        |> List.iter (buildDependencyNode >> d.Add)
        metadataNode.Add d

    !! "id" metadata.Id
    !! "version" <| metadata.Version.ToString()
    !!? "title" metadata.Title
    !! "authors" (metadata.Authors |> String.concat ", ")
    !!? "owners" (metadata.Owners |> Option.map (fun o -> o |> String.concat ", "))
    !!? "licenseUrl" metadata.LicenseUrl
    !!? "projectUrl" metadata.ProjectUrl
    !!? "iconUrl" metadata.IconUrl
    !!? "requireLicenseAcceptance" (metadata.RequireLicenseAcceptance |> Option.map (fun b -> b.ToString()))
    !! "description" metadata.Description
    !!? "summary" metadata.Summary
    !!? "releaseNotes" metadata.ReleaseNotes
    !!? "copyright" metadata.Copyright
    !!? "language" metadata.Language
    !!? "tags" (metadata.Tags |> Option.map (fun t -> t |> String.concat " "))
    !!? "developmentDependency" (metadata.Tags |> Option.map (fun b -> b.ToString()))
    metadata.Dependencies |> Option.iter buildDependenciesNode

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

let writeZip metaData libDir contentDir outputDir =
    let outputPath = Path.Combine(outputDir, metaData.Id + ".nupkg")
    if File.Exists outputPath then
        File.Delete outputPath
    use zipFile = new ZipFile(outputPath)
    let addEntry (zipFile : ZipFile) path writer =
        let writeDel _ stream =
            writer stream
        zipFile.AddEntry(path, WriteDelegate(writeDel))

    libDir
    |> Option.iter (fun lib -> zipFile.AddDirectory(lib, "lib") |> ignore)

    contentDir
    |> Option.iter (fun lib -> zipFile.AddDirectory(lib, "Content") |> ignore)

    writeNupkg metaData
    |> List.iter (fun (path, writer) -> addEntry zipFile path writer |> ignore)

    let fileList =
        zipFile.Entries
        |> Seq.filter (fun e -> not e.IsDirectory)
        |> Seq.map (fun e -> e.FileName)

    let contentTypesDoc =
        addEntry zipFile contentTypePath (contentTypeDoc fileList |> xDocWriter)

    zipFile.Save()

