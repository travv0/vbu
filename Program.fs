module Program

open Argu
open DotNet.Globbing
open System
open System.IO
open System.Text.Json
open System.Threading

open Args

type Game =
    { Name: string
      Path: string
      Glob: string option }

type Config =
    { Path: string
      Frequency: int
      NumToKeep: int
      Games: Game [] }

let defaultGlob = "**/*"

let withColor color f =
    Console.ForegroundColor <- color
    f ()
    Console.ResetColor()

let printWithColor color s =
    withColor color (fun () -> printfn "%s" s)

let note s =
    "Note: " + s |> printWithColor ConsoleColor.Blue

let warn s =
    "Warning: " + s
    |> printWithColor ConsoleColor.Yellow

let err s =
    "Error: " + s |> printWithColor ConsoleColor.Red

let warnMissingGames games config =
    let warningPrinted =
        Seq.fold
            (fun warningPrinted game ->
                if not (Seq.exists (fun g -> g.Name = game) config.Games) then
                    warn <| sprintf "No game named `%s'" game
                    true
                else
                    warningPrinted)
            false
            games

    if warningPrinted then printfn ""

let printConfigRow label value newValue =
    printfn "%s: %s%s" label value
    <| match newValue with
       | Some nv when value = nv -> ""
       | Some nv -> sprintf " -> %s" nv
       | None -> ""

let printGame game newName newPath newGlob =
    printConfigRow "Name" game.Name newName
    printConfigRow "Save path" game.Path newPath

    match (game.Glob, newGlob) with
    | (Some _, _)
    | (None, Some _) ->
        printConfigRow "Save glob" (Option.defaultValue "" game.Glob) (Some(Option.defaultValue "" newGlob))
    | _ -> ()

    printfn ""

let cleanupBackups (backupPath: string) verbose config =
    if config.NumToKeep > 0 then
        let glob =
            Glob.Parse(
                backupPath
                + ".bak.[0-9][0-9][0-9][0-9]_[0-9][0-9]_[0-9][0-9]_[0-9][0-9]_[0-9][0-9]_[0-9][0-9]"
            )

        let allFiles =
            Directory.EnumerateFiles(Path.GetDirectoryName(backupPath))

        let files =
            Seq.filter (fun f -> glob.IsMatch(f: string)) allFiles
            |> Seq.append (Seq.singleton backupPath)

        if (Seq.length files > config.NumToKeep) then
            let sortedFiles =
                Seq.sortByDescending File.GetLastWriteTimeUtc files

            let filesToDelete = Seq.skip config.NumToKeep sortedFiles

            for file in filesToDelete do
                if verbose then
                    note <| sprintf "Deleting %s" file

                File.Delete(file)

let rec backupFile game basePath glob fromPath toPath verbose config =
    try
        let globMatches () =
            let glob =
                Glob.Parse(Path.Join(basePath, Option.defaultValue defaultGlob glob))

            glob.IsMatch(fromPath: string)

        let copyAndCleanup () =
            Directory.CreateDirectory(Path.GetDirectoryName(toPath: string))
            |> ignore

            printfn "%s ==>\n\t%s" fromPath toPath
            File.Copy(fromPath, toPath)
            cleanupBackups toPath verbose config
            (1, Seq.empty)

        let backupFile' () =
            let fromInfo = FileInfo(fromPath)

            let fromIsReparsePoint =
                fromInfo.Attributes.HasFlag(FileAttributes.ReparsePoint)

            if fromIsReparsePoint then
                if verbose then
                    note
                    <| sprintf "%s appears to be a link to somewhere else in the filesystem. Skipping..." fromPath

                (0, Seq.empty)
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
                            + toModTime.ToString("yyyy_MM_dd_HH_mm_ss")
                        )

                        copyAndCleanup ()
                    else
                        (0, Seq.empty)
                | None -> copyAndCleanup ()

        if Directory.Exists(fromPath) then
            backupFiles game basePath glob fromPath toPath verbose config
        else if globMatches () then
            backupFile' ()
        else
            (0, Seq.empty)
    with e ->
        let warning =
            sprintf "Unable to backup file %s for game %s:\n%s\n" toPath game e.Message

        warn warning
        (1, Seq.singleton warning)

and backupFiles game basePath glob fromPath toPath verbose config =
    Directory.EnumerateFileSystemEntries(fromPath)
    |> Seq.fold
        (fun (c, es) path ->
            let file = Path.GetFileName(path)

            let (newCount, newErrs) =
                backupFile game basePath glob (Path.Join(fromPath, file)) (Path.Join(toPath, file)) verbose config

            (c + newCount, Seq.append es newErrs))
        (0, Seq.empty)

let backupGame gameName verbose config =
    let startTime = DateTime.Now

    let game =
        Seq.tryFind (fun g -> g.Name = gameName) config.Games

    match game with
    | Some game ->
        if Directory.Exists game.Path then
            let (backedUpCount, warnings) =
                backupFiles game.Name game.Path game.Glob game.Path (Path.Join(config.Path, gameName)) verbose config

            if (backedUpCount > 0) then
                let now = DateTime.Now
                let warningCount = Seq.length warnings

                printfn
                    "\nFinished backing up %d file%s%s for %s in %fs on %s at %s\n"
                    backedUpCount
                    (if backedUpCount = 1 then "" else "s")
                    (if warningCount > 0 then
                         sprintf " with %d warning%s" warningCount (if warningCount = 1 then "" else "s")
                     else
                         "")
                    gameName
                    (now - startTime).TotalSeconds
                    (now.ToLongDateString())
                    (now.ToLongTimeString())

            warnings
        else
            warn
            <| sprintf "Path set for %s doesn't exist: %s" gameName game.Path

            Seq.empty
    | None ->
        warnMissingGames [ gameName ] config
        Seq.empty

let rec backup (gameNames: string list option) (loop: bool) (verbose: bool) config =
    let gameNames' =
        match gameNames with
        | None -> Array.map (fun g -> g.Name) config.Games
        | Some gns -> Array.ofSeq gns

    let warnings =
        gameNames'
        |> Seq.fold
            (fun acc game ->
                try
                    let warnings = backupGame game verbose config
                    Seq.append acc warnings
                with e ->
                    err
                    <| sprintf "Error backing up %s: %s" game e.Message

                    acc)
            Seq.empty

    let warningCount = Seq.length warnings

    if warningCount > 0 then
        withColor
            ConsoleColor.Yellow
            (fun () ->
                printf "\n%d warning%s occurred:" warningCount (if warningCount = 1 then "" else "s")

                if verbose then
                    printfn "\n"
                    Seq.iter (printfn "%s") warnings
                else
                    printfn "\nPass --verbose flag to print all warnings after backup completes\n")

    if loop then
        Thread.Sleep(TimeSpan.FromMinutes(float config.Frequency))
        backup gameNames loop verbose config
    else
        None

let validGameNameChars: Set<char> =
    Seq.concat [ [ 'A' .. 'Z' ]
                 [ 'a' .. 'z' ]
                 [ '0' .. '9' ]
                 [ '-'; '_' ] ]
    |> Set.ofSeq

let isValidGameName (name: string) =
    Seq.forall (fun c -> Set.contains c validGameNameChars) name

let absolutePath (path: string) =
    if path.Length > 0 && path.[0] = '~' then
        let home =
            Environment.GetFolderPath(Environment.SpecialFolder.UserProfile)

        Path.Join(home, path.[1..]) |> Path.GetFullPath
    else
        Path.GetFullPath path

let add (game: string) (path: string) (glob: string option) config =
    if Seq.exists (fun g -> g.Name = game) config.Games then
        err
        <| sprintf "Game with the name %s already exists" game

        None
    elif not (isValidGameName game) then
        err
        <| sprintf
            "Invalid characters in name `%s': only alphanumeric characters, underscores, and hyphens are allowed"
            game

        None
    else
        let newGame =
            { Name = game
              Path = absolutePath path
              Glob = glob }

        let newGames =
            Seq.singleton newGame
            |> Seq.append config.Games
            |> Seq.sortBy (fun g -> g.Name)
            |> Array.ofSeq

        printfn "Successfully added %s\n" game
        printGame newGame None None None

        Some { config with Games = newGames }

let list () config =
    Seq.iter (fun g -> printfn "%s" g.Name) config.Games
    None

let info (gameNames: string list option) config =
    match gameNames with
    | Some gns -> warnMissingGames gns config
    | None -> ()

    let games =
        match gameNames with
        | None -> config.Games
        | Some gs -> Array.filter (fun g -> Seq.exists ((=) g.Name) gs) config.Games

    Seq.iter (fun g -> printGame g None None None) games
    None

let rec promptYorN prompt =
    stdout.Flush()
    printf "\n%s (y/n) " prompt

    match Console.ReadLine().Trim().ToLower() with
    | "y" -> true
    | "n" -> false
    | _ -> promptYorN prompt

let remove (games: string list) (yes: bool) config =
    warnMissingGames games config

    let newGames =
        [| for game in config.Games do
               if Seq.exists ((=) game.Name) games
                  && (yes
                      || promptYorN (
                          "Are you sure you want to remove "
                          + game.Name
                          + "?"
                      )) then
                   printfn "Removed %s" game.Name
               else
                   yield game |]

    Some { config with Games = newGames }

let edit (gameName: string) (newName: string option) (newPath: string option) (newGlob: string option) config =
    match (newName, newPath, newGlob) with
    | (None, None, None) ->
        err "One or more of --name, --path, or --glob must be provided."
        None
    | _ ->
        let splitList =
            Seq.tryFindIndex (fun g -> g.Name = gameName) config.Games
            |> Option.map (fun i -> List.ofSeq config.Games |> List.splitAt i)

        match splitList with
        | None ->
            warnMissingGames [ gameName ] config
            None
        | Some (_, []) ->
            err "Couldn't find game in list"
            None
        | Some (front, game :: back) ->
            let newName' = Option.defaultValue game.Name newName

            let newGlob' =
                match newGlob with
                | Some "none" -> None
                | Some "" -> None
                | glob -> glob

            let newPath' =
                Option.defaultValue game.Path newPath
                |> absolutePath

            let editedGame =
                { game with
                      Name = newName'
                      Path = newPath'
                      Glob = newGlob' }

            if not (isValidGameName newName') then
                err
                <| sprintf
                    "Invalid characters in name `%s': only alphanumeric characters, underscores, and hyphens are allowed"
                    newName'

                None
            else
                printGame game (Some newName') (Some newPath') newGlob'

                let backupDirExists =
                    Directory.Exists(Path.Join(config.Path, gameName))

                if (Option.isSome newName && backupDirExists) then
                    warn "Game name changed, renaming backup directory..."
                    Directory.Move(Path.Join(config.Path, gameName), Path.Join(config.Path, newName'))

                Some
                    { config with
                          Games =
                              Seq.concat [ front
                                           List.singleton editedGame
                                           back ]
                              |> Array.ofSeq }

let printConfig newBackupDir newBackupFreq newBackupsToKeep config =
    printConfigRow "Backup path" config.Path newBackupDir

    printConfigRow
        "Backup frequency (in minutes)"
        (config.Frequency.ToString())
        (Option.map (fun freq -> freq.ToString()) newBackupFreq)

    printConfigRow
        "Number of backups to keep"
        (config.NumToKeep.ToString())
        (Option.map (fun toKeep -> toKeep.ToString()) newBackupsToKeep)

    printfn ""

let editConfig backupDir backupFreq backupsToKeep config =
    let newBackupDir =
        Option.defaultValue config.Path backupDir
        |> absolutePath

    let newBackupFreq =
        Option.defaultValue config.Frequency backupFreq

    let newBackupsToKeep =
        Option.defaultValue config.NumToKeep backupsToKeep

    printConfig (Some newBackupDir) (Some newBackupFreq) (Some newBackupsToKeep) config

    match (backupDir, backupFreq, backupsToKeep) with
    | (None, None, None) -> None
    | _ ->
        Some
            { config with
                  Path = newBackupDir
                  Frequency = newBackupFreq
                  NumToKeep = newBackupsToKeep }

let defaultConfigPath =
    Path.Join(Environment.GetFolderPath(Environment.SpecialFolder.UserProfile), ".sbufs", "config.json")

let defaultConfig =
    { Path = Path.Join(Environment.GetFolderPath(Environment.SpecialFolder.UserProfile), ".sbufs-backups")
      Frequency = 15
      NumToKeep = 20
      Games = Array.empty }

let saveDefaultConfig path =
    match Path.GetDirectoryName(path: string) with
    | "" -> ()
    | path -> Directory.CreateDirectory(path) |> ignore

    File.WriteAllText(path, JsonSerializer.Serialize(defaultConfig))

let run (parseResults: ParseResults<_>) =
    let command = parseResults.GetSubCommand()

    match command with
    | Backup sp -> backup (sp.TryGetResult BackupArgs.Games) (sp.Contains Loop) (sp.Contains Verbose)
    | Add sp -> add (sp.GetResult AddArgs.Game) (sp.GetResult AddArgs.Path) (sp.TryGetResult AddArgs.Glob)
    | List _ -> list ()
    | Info sp -> info (sp.TryGetResult InfoArgs.Games)
    | Remove sp -> remove (sp.GetResult Games) (sp.Contains Yes)
    | Edit sp -> edit (sp.GetResult Game) (sp.TryGetResult Name) (sp.TryGetResult EditArgs.Path) (sp.TryGetResult Glob)
    | Config sp -> editConfig (sp.TryGetResult Path) (sp.TryGetResult Frequency) (sp.TryGetResult Keep)
    | Config_Path _
    | Version -> failwithf "non-command matched as command: %s" (command.ToString())

let loadConfig configPath =
    if not (File.Exists(configPath)) then
        saveDefaultConfig configPath

    try
        configPath
        |> File.ReadAllText
        |> JsonSerializer.Deserialize<Config>
    with e ->
        warn
        <| sprintf
            "Couldn't load config: %s\nAttempting to save default config \
                    to '%s' after backing up existing config.\n"
            e.Message
            configPath

        if File.Exists(configPath) then
            File.Copy(configPath, configPath + ".bak", true)

        saveDefaultConfig configPath
        defaultConfig

[<EntryPoint>]
let main argv =
    try
        let parser =
            ArgumentParser.Create<SbuArgs>(programName = AppDomain.CurrentDomain.FriendlyName)

        let parseResults =
            parser.ParseCommandLine(inputs = argv, raiseOnUsage = true)

        if parseResults.Contains Version then
            printfn "sbu v1.2.0"
        else
            let configPath =
                parseResults.TryGetResult Config_Path
                |> Option.defaultValue defaultConfigPath

            let config = loadConfig configPath
            let newConfig = run parseResults config

            match newConfig with
            | None -> ()
            | Some c ->
                Directory.CreateDirectory(Path.GetDirectoryName(configPath))
                |> ignore

                File.WriteAllText(configPath, JsonSerializer.Serialize(c))
    with
    | :? Argu.ArguParseException as e -> printfn "%s" e.Message
    | e -> err e.Message

    0
