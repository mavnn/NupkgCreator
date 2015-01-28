namespace Paket
open Paket

type PackagingConfig =
    {
        Id : string
        Version : SemVerInfo
        Authors : string list
        Description : string
        Title : string option
        Owners : string list option
        ReleaseNotes : string option
        Summary : string option
        Language : string option
        ProjectUrl : string option
        IconUrl : string option
        LicenseUrl : string option
        Copyright : string option
        RequireLicenseAcceptance : string option
        Tags : string list option
        DevelopmentDependency : bool option
        Dependencies : (string * VersionRequirement) list option
    }

[<CompilationRepresentationAttribute(CompilationRepresentationFlags.ModuleSuffix)>]
module PackagingConfig =
    open System
    open System.IO
    open System.Text.RegularExpressions
    open Paket.Rop
    open Paket.Domain

    let private basicOperators = ["~>";"==";"<=";">=";"=";">";"<"]
    let private operators = basicOperators @ (basicOperators |> List.map (fun o -> "!" + o))

    let twiddle(minimum:string) =                    
        let promote index (values:string array) =
            let parsed, number = Int32.TryParse values.[index]
            if parsed then values.[index] <- (number + 1).ToString()
            if values.Length > 1 then values.[values.Length - 1] <- "0"
            values

        let parts = minimum.Split '.'
        let penultimateItem = Math.Max(parts.Length - 2, 0)
        let promoted = parts |> promote penultimateItem
        String.Join(".", promoted)

    let parseVersionRequirement (text : string) : VersionRequirement =
        let parsePrerelease(texts:string seq) =
            let texts = texts |> Seq.filter ((<>) "")
            if Seq.isEmpty texts then PreReleaseStatus.No else
            if Seq.head(texts).ToLower() = "prerelease" then PreReleaseStatus.All else
            PreReleaseStatus.Concrete(texts |> Seq.toList)

        if text = "" || text = null then VersionRequirement(VersionRange.AtLeast("0"),PreReleaseStatus.No) else

        match text.Split(' ') |> Array.toList with
        |  ">=" :: v1 :: "<" :: v2 :: rest -> VersionRequirement(VersionRange.Range(VersionRangeBound.Including,SemVer.Parse v1,SemVer.Parse v2,VersionRangeBound.Excluding),parsePrerelease rest)
        |  ">=" :: v1 :: "<=" :: v2 :: rest -> VersionRequirement(VersionRange.Range(VersionRangeBound.Including,SemVer.Parse v1,SemVer.Parse v2,VersionRangeBound.Including),parsePrerelease rest)
        |  "~>" :: v1 :: ">=" :: v2 :: rest -> VersionRequirement(VersionRange.Range(VersionRangeBound.Including,SemVer.Parse v2,SemVer.Parse(twiddle v1),VersionRangeBound.Excluding),parsePrerelease rest)
        |  "~>" :: v1 :: ">" :: v2 :: rest -> VersionRequirement(VersionRange.Range(VersionRangeBound.Excluding,SemVer.Parse v2,SemVer.Parse(twiddle v1),VersionRangeBound.Excluding),parsePrerelease rest)
        |  ">" :: v1 :: "<" :: v2 :: rest -> VersionRequirement(VersionRange.Range(VersionRangeBound.Excluding,SemVer.Parse v1,SemVer.Parse v2,VersionRangeBound.Excluding),parsePrerelease rest)
        |  ">" :: v1 :: "<=" :: v2 :: rest -> VersionRequirement(VersionRange.Range(VersionRangeBound.Excluding,SemVer.Parse v1,SemVer.Parse v2,VersionRangeBound.Including),parsePrerelease rest)
        | _ -> 
            let splitVersion (text:string) =            
                match basicOperators |> List.tryFind(text.StartsWith) with
                | Some token -> token, text.Replace(token + " ", "").Split(' ') |> Array.toList
                | None -> "=", text.Split(' ') |> Array.toList

            try
                match splitVersion text with
                | "==", version :: rest -> VersionRequirement(VersionRange.OverrideAll(SemVer.Parse version),parsePrerelease rest)
                | ">=", version :: rest -> VersionRequirement(VersionRange.AtLeast(version),parsePrerelease rest)
                | ">", version :: rest -> VersionRequirement(VersionRange.GreaterThan(SemVer.Parse version),parsePrerelease rest)
                | "<", version :: rest -> VersionRequirement(VersionRange.LessThan(SemVer.Parse version),parsePrerelease rest)
                | "<=", version :: rest -> VersionRequirement(VersionRange.Maximum(SemVer.Parse version),parsePrerelease rest)
                | "~>", minimum :: rest -> VersionRequirement(VersionRange.Between(minimum,twiddle minimum),parsePrerelease rest)
                | _, version :: rest -> VersionRequirement(VersionRange.Exactly(version),parsePrerelease rest)
                | _ -> failwithf "could not parse version range \"%s\"" text
            with
            | _ -> failwithf "could not parse version range \"%s\"" text

    let private (!<) prefix lines =
        let singleLine str =
            let regex = sprintf "%s (?<%s>.*)" prefix prefix
            let reg = Regex(regex, RegexOptions.Compiled ||| RegexOptions.CultureInvariant ||| RegexOptions.IgnoreCase)
            if reg.IsMatch str then
                Some <| (reg.Match str).Groups.[prefix].Value
            else None
        let multiLine lines =
            let rec findBody acc (lines : string list) =
                match lines with
                | h::t when h.StartsWith "    " -> findBody (h.Trim()::acc) t
                | _ -> Some (acc |> List.rev |> String.concat "\n")
            let rec findStart lines =
                match lines with
                | h::t when h = prefix ->
                    findBody [] t
                | h::t ->
                    findStart t
                | [] ->
                    None
            findStart lines
        [
            lines |> List.tryPick singleLine
            multiLine lines
        ]
        |> List.tryPick id
        

    let private failP str =
        fail <| PackagingConfigParseError str

    type PackageConfigType =
        | FileType
        | ProjectType

    let private parsePackageConfigType contents =
        match contents with
        | firstLine::_ ->
            let t' = !< "type" [firstLine]
            t'
            |> function
               | Some s ->
                    match s with
                    | "file" -> succeed FileType
                    | "project" -> succeed ProjectType
                    | s -> failP (sprintf "Unknown package config type.")
               | None ->
                    failP (sprintf "First line of paket.package file had no 'type' declaration.")
        | [] ->
            failP "Empty paket.packaging file."

    let private getId lines =
        !< "id" lines
        |> function
           | Some m -> succeed <| m
           | None -> failP "No id line in paket.packaging file."

    let private getVersion lines =
        !< "version" lines
        |> function
           | Some m ->
                let versionString = m
                succeed <| SemVer.Parse versionString
           | None ->
                failP "No version line in paket.packaging file."

    let private getAuthors lines =
        !< "authors" lines
        |> function
           | Some m ->
                m.Split ','
                |> Array.map (fun s -> s.Trim())
                |> List.ofArray
                |> succeed
           | None ->
                failP "No authors line in paket.packaging file."

    let private getDescription lines =
        !< "description" lines
        |> function
           | Some m ->
                succeed m
           | None ->
                failP "No description line in paket.packaging file."

    let private getDependencies lines =
        !< "dependencies" lines
        |> Option.map (fun d -> d.Split '\n')
        |> Option.map
            (Array.map
                (fun d ->
                    let reg = Regex(@"(?<id>\S+)(?<version>.*)").Match d
                    let id' =
                        reg.Groups.["id"].Value
                    let versionRequirement =
                        reg.Groups.["version"].Value.Trim()
                        |> parseVersionRequirement
                    id', versionRequirement))
        |> Option.map Array.toList

    let Parse (contentStream : Stream) =
        rop {
            let configLines =
                use sr = new StreamReader(contentStream, System.Text.Encoding.UTF8)
                let rec inner (s : StreamReader) =
                    seq {
                        let line = s.ReadLine()
                        if line <> null then
                            yield line
                            yield! inner s
                    }
                inner sr |> Seq.toList
            let! type' =
                parsePackageConfigType configLines                

            match type' with
            | ProjectType ->
                return! failP "Project type packages are not yet supported."
            | FileType ->
                let! id' = getId configLines
                let! version = getVersion configLines
                let! authors = getAuthors configLines
                let! description = getDescription configLines

                let title = !< "title" configLines
                let owners =
                    !< "owners" configLines
                    |> Option.map (fun o -> 
                        o.Split(',')
                        |> Array.map (fun o -> o.Trim())
                        |> Array.toList)
                let releaseNotes =
                    !< "releaseNotes" configLines
                let summary =
                    !< "summary" configLines
                let language =
                    !< "language" configLines
                let projectUrl =
                    !< "projectUrl" configLines
                let iconUrl =
                    !< "iconUrl" configLines
                let licenseUrl =
                    !< "licenseUrl" configLines
                let copyright =
                    !< "copyright" configLines
                let requireLicenseAcceptance =
                    !< "requireLicenseAcceptance" configLines
                let tags =
                    !< "tags" configLines
                    |> Option.map (fun t ->
                                    t.Split ' '
                                    |> Array.map (fun t -> t.Trim())
                                    |> Array.toList)
                let developmentDependency =
                    !< "developmentDependency" configLines
                    |> Option.map Boolean.Parse
                let dependencies =
                    getDependencies configLines
                

                return {
                    Id = id'
                    Version = version
                    Authors = authors
                    Description = description
                    Title = title
                    Owners = owners
                    ReleaseNotes = releaseNotes
                    Summary = summary
                    Language = language
                    ProjectUrl = projectUrl
                    IconUrl = iconUrl
                    LicenseUrl = licenseUrl
                    Copyright = copyright
                    RequireLicenseAcceptance = requireLicenseAcceptance
                    Tags = tags
                    DevelopmentDependency = developmentDependency
                    Dependencies = dependencies
                }
        }