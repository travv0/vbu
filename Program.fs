module Program

open Argu
open DotNet.Globbing
open FSharpx.Reader
open System
open System.IO
open System.Threading

open Args
open Types
open Util.FileSystem
open Util.Terminal

let warnMissingGroups (groups: list<string>) =
    reader {
        let! config = ask

        let warningPrinted =
            List.fold
                (fun warningPrinted group ->
                    if not (Array.exists (fun g -> g.Name = group) config.Groups) then
                        warn $"No group named `%s{group}'"
                        true
                    else
                        warningPrinted)
                false
                groups

        if warningPrinted then printfn ""
    }

let cleanupBackups (backupPath: string) verbose =
    reader {
        let! config = ask

        if config.NumToKeep > 0 then
            let backupDir = Path.GetDirectoryName(backupPath)
            let backupFileName = Path.GetFileName(backupPath)

            let glob =
                Glob.Parse(
                    backupDir
                    +/ "*.bak.[0-9][0-9][0-9][0-9]_[0-9][0-9]_[0-9][0-9]_[0-9][0-9]_[0-9][0-9]_[0-9][0-9]"
                )

            let allFiles = Directory.EnumerateFiles(backupDir)

            let files =
                allFiles
                |> Seq.filter (fun f ->
                    Path.GetFileName(f).StartsWith(backupFileName)
                    && glob.IsMatch(f))
                |> Seq.append (seq { backupPath })
                |> Seq.toList

            if (List.length files > config.NumToKeep) then
                let sortedFiles =
                    List.sortByDescending File.GetLastWriteTimeUtc files

                let filesToDelete = List.skip config.NumToKeep sortedFiles

                for file in filesToDelete do
                    note verbose $"Deleting %s{file}"
                    File.Delete(file)
    }

let rec backupFile group basePath glob fromPath toPath verbose (force: bool) =
    reader {
        try
            let globMatches () =
                let glob =
                    Glob.Parse(
                        basePath
                        +/ Option.defaultValue Group.defaultGlob glob
                    )

                glob.IsMatch(fromPath: string)

            let copyAndCleanup () =
                reader {
                    Directory.CreateDirectory(Path.GetDirectoryName(toPath: string))
                    |> ignore

                    printfn $"%s{fromPath} ==>\n\t%s{toPath}"
                    File.Copy(fromPath, toPath, force)
                    do! cleanupBackups toPath verbose
                    return (1, [])
                }

            let backupFile' () =
                reader {
                    let fromInfo = FileInfo(fromPath)

                    let fromIsReparsePoint =
                        fromInfo.Attributes.HasFlag(FileAttributes.ReparsePoint)

                    if fromIsReparsePoint then
                        note
                            verbose
                            $"%s{fromPath} appears to be a link to somewhere else in the filesystem. Skipping..."

                        return (0, [])
                    else
                        let fromModTime = fromInfo.LastWriteTimeUtc

                        let toModTime =
                            if File.Exists(toPath) then
                                Some(File.GetLastWriteTimeUtc(toPath))
                            else
                                None

                        match toModTime with
                        | Some toModTime ->
                            if fromModTime <> toModTime then
                                File.Move(
                                    toPath,
                                    toPath
                                    + ".bak."
                                    + toModTime.ToString("yyyy_MM_dd_HH_mm_ss"),
                                    force
                                )

                                return! copyAndCleanup ()
                            else
                                return (0, [])
                        | None -> return! copyAndCleanup ()
                }

            if Directory.Exists(fromPath) then
                return! backupFiles group basePath glob fromPath toPath verbose force
            else if globMatches () then
                return! backupFile' ()
            else
                return (0, [])
        with
        | e ->
            let warning =
                $"Unable to backup file %s{toPath} for group %s{group}:\n\
                %s{e.Message}\n"

            warn warning
            return (1, [ warning ])
    }

and backupFiles group basePath glob fromPath toPath verbose (force: bool) =
    Directory.EnumerateFileSystemEntries(fromPath)
    |> foldM
        (fun (c, es) path ->
            reader {
                let file = Path.GetFileName(path)

                let! newCount, newErrs =
                    backupFile group basePath glob (fromPath +/ file) (toPath +/ file) verbose force

                return (c + newCount, es @ newErrs)
            })
        (0, [])

let backupGroup groupName verbose (force: bool) =
    reader {
        let! config = ask
        let startTime = DateTime.Now

        let group =
            Array.tryFind (fun g -> g.Name = groupName) config.Groups

        match group with
        | Some group ->
            if Directory.Exists group.Path then
                let! backedUpCount, warnings =
                    backupFiles group.Name group.Path group.Glob group.Path (config.Path +/ groupName) verbose force

                if (backedUpCount > 0) then
                    let now = DateTime.Now
                    let warningCount = List.length warnings

                    printfn
                        "\nFinished backing up %d file%s%s for %s in %fs on %s at %s\n"
                        backedUpCount
                        (if backedUpCount = 1 then "" else "s")
                        (if warningCount > 0 then
                             sprintf " with %d warning%s" warningCount (if warningCount = 1 then "" else "s")
                         else
                             "")
                        groupName
                        (now - startTime).TotalSeconds
                        (now.ToLongDateString())
                        (now.ToLongTimeString())

                return warnings
            else
                warn $"Path set for %s{groupName} doesn't exist: %s{group.Path}"
                return []
        | None ->
            do! warnMissingGroups [ groupName ]
            return []
    }

let rec backup (groupNames: option<list<string>>) (loop: bool) (verbose: bool) (force: bool) =
    reader {
        let! config = ask

        let groupNames' =
            match groupNames with
            | None -> Array.map (fun g -> g.Name) config.Groups
            | Some gns -> gns |> List.toArray

        let! warnings =
            groupNames'
            |> foldM
                (fun acc group ->
                    reader {
                        try
                            let! warnings = backupGroup group verbose force
                            return acc @ warnings
                        with
                        | e ->
                            err $"Error backing up %s{group}: %s{e.Message}"
                            return acc
                    })
                []

        let warningCount = List.length warnings

        if warningCount > 0 then
            withColor ConsoleColor.Yellow (fun () ->
                printf "\n%d warning%s occurred:" warningCount (if warningCount = 1 then "" else "s")

                if verbose then
                    printfn "\n"
                    List.iter (printfn "%s") warnings
                else
                    printfn "\nPass --verbose flag to print all warnings after backup completes\n")

        if loop then
            Thread.Sleep(TimeSpan.FromMinutes(float config.Frequency))
            return! backup groupNames loop verbose force
        else
            return None
    }

let add (group: string) (path: string) (glob: option<string>) =
    reader {
        let! config = ask

        if Array.exists (fun g -> g.Name = group) config.Groups then
            err $"Group with the name %s{group} already exists"
            return None
        elif not (Group.isValidName group) then
            err
                $"Invalid characters in name `%s{group}': only alphanumeric characters, underscores, and hyphens are allowed"

            return None
        else
            let newGroup =
                { Name = group
                  Path = absolutePath path
                  Glob = glob }

            let newGroups =
                Array.append config.Groups [| newGroup |]
                |> Array.sortBy (fun g -> g.Name)

            printfn "Group added successfully:\n"
            Group.print newGroup
            return Some { config with Groups = newGroups }
    }

let list =
    reader {
        let! config = ask
        Array.iter (fun g -> printfn $"%s{g.Name}") config.Groups
        return None
    }

let info (groupNames: option<list<string>>) =
    reader {
        let! config = ask

        match groupNames with
        | Some gns -> do! warnMissingGroups gns
        | None -> ()

        let groups =
            match groupNames with
            | None -> config.Groups
            | Some gs -> Array.filter (fun g -> List.contains g.Name gs) config.Groups

        Array.iter Group.print groups
        return None
    }

let remove (groups: list<string>) (yes: bool) =
    reader {
        let! config = ask
        do! warnMissingGroups groups

        let newGroups =
            [| for group in config.Groups do
                   if List.contains group.Name groups
                      && (yes
                          || promptYorN (
                              "Are you sure you want to remove "
                              + group.Name
                              + "?"
                          )) then
                       printfn $"Removed %s{group.Name}"
                   else
                       yield group |]

        return Some { config with Groups = newGroups }
    }

let edit (groupName: string) (name: option<string>) (path: option<string>) (glob: option<string>) =
    reader {
        let! config = ask

        match (name, path, glob) with
        | None, None, None ->
            err "One or more of --name, --path, or --glob must be provided."
            return None
        | _ ->
            let splitList =
                Array.tryFindIndex (fun g -> g.Name = groupName) config.Groups
                |> Option.map (fun i -> config.Groups |> Array.toList |> List.splitAt i)

            match splitList with
            | None ->
                do! warnMissingGroups [ groupName ]
                return None
            | Some (_, []) ->
                err "Couldn't find group in list"
                return None
            | Some (front, group :: back) ->
                let newName = Option.defaultValue group.Name name

                let newGlobForSave =
                    match glob with
                    | Some ""
                    | Some "none" -> None
                    | glob -> glob

                let newGlobForPrint =
                    Option.map
                        (function
                        | "none" -> ""
                        | glob -> glob)
                        glob

                let newPath =
                    Option.defaultValue group.Path path
                    |> absolutePath

                let editedGroup =
                    { Name = newName
                      Path = newPath
                      Glob = newGlobForSave }

                if not (Group.isValidName newName) then
                    err
                        $"Invalid characters in name `%s{newName}': only alphanumeric characters, underscores, and hyphens are allowed"

                    return None
                else
                    Group.printUpdated group (Some newName) (Some newPath) newGlobForPrint

                    let backupDirExists =
                        Directory.Exists(config.Path +/ groupName)

                    if (Option.isSome name && backupDirExists) then
                        warn "Group name changed, renaming backup directory..."
                        Directory.Move(config.Path +/ groupName, config.Path +/ newName)

                    return Some { config with Groups = front @ editedGroup :: back |> List.toArray }
    }

let editConfig (backupDir: option<string>) (backupFreq: option<int>) (backupsToKeep: option<int>) =
    reader {
        let! config = ask

        let newBackupDir =
            Option.defaultValue config.Path backupDir
            |> absolutePath

        let newBackupFreq =
            Option.defaultValue config.Frequency backupFreq

        let newBackupsToKeep =
            Option.defaultValue config.NumToKeep backupsToKeep

        Config.printUpdated config (Some newBackupDir) (Some newBackupFreq) (Some newBackupsToKeep)

        match (backupDir, backupFreq, backupsToKeep) with
        | None, None, None -> return None
        | _ ->
            return
                Some
                    { config with
                        Path = newBackupDir
                        Frequency = newBackupFreq
                        NumToKeep = newBackupsToKeep }
    }

let app (parseResults: ParseResults<_>) =
    let command = parseResults.GetSubCommand()

    match command with
    | Backup sp ->
        backup (sp.TryGetResult BackupArgs.Groups) (sp.Contains Loop) (sp.Contains Verbose) (sp.Contains Force)
    | Add sp -> add (sp.GetResult AddArgs.Group) (sp.GetResult AddArgs.Path) (sp.TryGetResult AddArgs.Glob)
    | List _ -> list
    | Info sp -> info (sp.TryGetResult InfoArgs.Groups)
    | Remove sp -> remove (sp.GetResult Groups) (sp.Contains Yes)
    | Edit sp -> edit (sp.GetResult Group) (sp.TryGetResult Name) (sp.TryGetResult EditArgs.Path) (sp.TryGetResult Glob)
    | Config sp -> editConfig (sp.TryGetResult Path) (sp.TryGetResult Frequency) (sp.TryGetResult Keep)
    | ConfigPath _
    | Version -> failwithf $"non-command matched as command: %A{command}"

[<EntryPoint>]
let main argv =
    try
        let parser =
            ArgumentParser.Create<vbuArgs>(programName = AppDomain.CurrentDomain.FriendlyName)

        let parseResults =
            parser.ParseCommandLine(inputs = argv, raiseOnUsage = true)

        if parseResults.Contains Version then
            printfn "vbu v1.3.2"
        else
            let configPath =
                parseResults.TryGetResult ConfigPath
                |> Option.defaultValue defaultConfigPath

            let config = Config.load configPath
            let newConfig = app parseResults config

            match newConfig with
            | None -> ()
            | Some c ->
                match Path.GetDirectoryName(configPath).Trim() with
                | "" -> ()
                | configDir -> Directory.CreateDirectory(configDir) |> ignore

                Config.save configPath c
    with
    | :? ArguParseException as e -> printfn $"%s{e.Message}"
    | e -> err e.Message

    0
