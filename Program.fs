open Argu
open DotNet.Globbing
open System.Text.Json
open System
open System.IO
open System.Threading

type BackupArgs =
    | [<MainCommand>] Games of games: string list
    | [<AltCommandLine("-l")>] Loop
    | [<AltCommandLine("-v")>] Verbose

    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | Games _ -> "List of games to back up.  If not provided, will back up all games"
            | Loop -> "Keep running, backing up games at the interval specified in your config file"
            | Verbose -> "Print verbose output"

type AddArgs =
    | [<MainCommand; Mandatory; Unique>] Game of game: string
    | [<AltCommandLine("-p")>] Path of save_path: string
    | [<AltCommandLine("-g")>] Glob of save_glob: string

    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | Game _ -> "Name of game to add"
            | Path _ -> "Path to added game's save files"
            | Glob _ ->
                "Save file glob for added game's save files. Only files \
                matching this pattern will be backed up. The default is \
                **/* which will recursively back up all saves in SAVE_PATH"

type InfoArgs =
    | [<MainCommand>] Games of games: string list
    | [<AltCommandLine("-b")>] Brief

    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | Games _ ->
                "List of games to display info for. If not \
                provided, will display info for all games"
            | Brief -> "Output only game names"

type RemoveArgs =
    | [<MainCommand; Mandatory>] Games of games: string list
    | [<AltCommandLine("-y")>] Yes

    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | Games _ -> "List of games to remove"
            | Yes -> "Remove all without confirmation prompts"

type EditArgs =
    | [<MainCommand; Unique; Mandatory>] Game of game: string
    | [<AltCommandLine("-n")>] Name of new_name: string
    | [<AltCommandLine("-p")>] Path of new_save_path: string
    | [<AltCommandLine("-g")>] Glob of new_save_glob: string

    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | Game _ -> "Name of game to edit"
            | Name _ ->
                "Set game name to NEW_NAME. This will also update the \
                directory name in your backup directory"
            | Path _ -> "Set game's save path to NEW_SAVE_PATH"
            | Glob _ ->
                "Set game's save file glob to NEW_SAVE_FILE_GLOB. \
                Setting this to an empty string or \"none\" implies the \
                glob **/* which will recursively back up all files"

type ConfigArgs =
    | [<AltCommandLine("-p")>] Path of backup_path: string
    | [<AltCommandLine("-f")>] Frequency of backup_frequency: int
    | [<AltCommandLine("-k")>] Keep of backups_to_keep: int

    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | Path _ -> "Set path to directory in which to back up saves"
            | Frequency _ -> "Set frequency in minutes to backup saves when looping"
            | Keep _ -> "Set how many copies of each backed-up file to keep"

type SbuArgs =
    | [<CliPrefix(CliPrefix.None)>] Backup of ParseResults<BackupArgs>
    | [<CliPrefix(CliPrefix.None)>] Add of ParseResults<AddArgs>
    | [<CliPrefix(CliPrefix.None)>] Info of ParseResults<InfoArgs>
    | [<CliPrefix(CliPrefix.None)>] Remove of ParseResults<RemoveArgs>
    | [<CliPrefix(CliPrefix.None)>] Edit of ParseResults<EditArgs>
    | [<CliPrefix(CliPrefix.None)>] Config of ParseResults<ConfigArgs>
    | Config_Path of config_path: string
    | Version

    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | Backup _ -> "Backup your game saves"
            | Add _ -> "Add games to backup"
            | Info _ -> "List info for games"
            | Remove _ -> "Remove games from backup"
            | Edit _ -> "Edit game info"
            | Config _ -> "Manage sbu configuration"
            | Config_Path _ -> "Path to configuration file"
            | Version -> "Print version information"

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

let warnMissingGames games config =
    List.iter
        (fun gn ->
            if not (Array.exists (fun g -> g.Name = gn) config.Games) then
                printfn "Warning: No game named `%s'" gn)
        games

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

let rec backupFile game basePath glob fromPath toPath =
    try
        let fromModTime = File.GetLastWriteTimeUtc(fromPath)

        let globMatches () =
            let glob =
                Glob.Parse(Path.Join(basePath, Option.defaultValue defaultGlob glob))

            glob.IsMatch(fromPath)

        let copyAndCleanup () =
            Directory.CreateDirectory(Directory.GetParent(toPath).FullName)
            |> ignore

            printfn "%s ==>\n\t%s" fromPath toPath
            File.Copy(fromPath, toPath)
            File.SetLastWriteTimeUtc(toPath, fromModTime)
            // cleanupBackups toPath
            (1, [||])

        let backupFile' () =
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
                    (0, [||])
            | None -> copyAndCleanup ()

        if Directory.Exists(fromPath) then
            backupFiles game basePath glob fromPath toPath
        else if globMatches () then
            backupFile' ()
        else
            (0, [||])
    with e ->
        let warning =
            sprintf "Unable to backup file %s for game %s:\n%s\n" toPath game e.Message

        printfn "Warning: %s" warning
        (1, [| warning |])

and backupFiles game basePath glob fromPath toPath =
    Directory.GetFileSystemEntries(fromPath)
    |> Array.fold
        (fun (c, es) path ->
            let file = Path.GetFileName(path)

            let (newCount, newErrs) =
                backupFile game basePath glob (Path.Join(fromPath, file)) (Path.Join(toPath, file))

            (c + newCount, Array.append es newErrs))
        (0, [||])

let backupGame gameName config =
    let startTime = DateTime.Now

    let game =
        Array.tryFind (fun g -> g.Name = gameName) config.Games

    match game with
    | Some game ->
        if Directory.Exists game.Path then
            let (backedUpCount, warnings) =
                backupFiles game.Name game.Path game.Glob game.Path (Path.Join(config.Path, gameName))

            if (backedUpCount > 0) then
                let now = DateTime.Now

                printfn
                    "\nFinished backing up %d file%s%s for %s in %fs on %s at %s\n"
                    backedUpCount
                    (if backedUpCount = 1 then "" else "s")
                    (if warnings.Length > 0 then
                         sprintf " with %d warning%s" warnings.Length (if warnings.Length = 1 then "" else "s")
                     else
                         "")
                    gameName
                    (now - startTime).TotalSeconds
                    (now.ToLongDateString())
                    (now.ToLongTimeString())

            warnings
        else
            printfn "Warning: Path set for %s doesn't exist: %s" gameName game.Path
            [||]
    | None ->
        warnMissingGames [ gameName ] config
        [||]

let rec backup (gameNames: string list option) (loop: bool) (verbose: bool) config =
    let gameNames' =
        match gameNames with
        | None -> Array.map (fun g -> g.Name) config.Games
        | Some gns -> List.toArray gns

    let warnings =
        Array.fold
            (fun acc game ->
                let warnings =
                    try
                        backupGame game config
                    with e ->
                        printfn "Error backing up %s: %s" game e.Message
                        [||]

                Array.append acc warnings)
            [||]
            gameNames'

    if not (Array.isEmpty warnings) then
        printf "\n%d warning%s occurred:" warnings.Length (if warnings.Length = 1 then "" else "s")

        if verbose then
            printfn "\n"
            Array.iter (printfn "%s") warnings
        else
            printfn "\nPass --verbose flag to print all warnings after backup completes\n"

    if loop then
        Thread.Sleep(TimeSpan.FromMinutes(float config.Frequency))
        backup gameNames loop verbose config
    else
        None

let validGameNameChars =
    [ [| for c in 'A' .. 'z' -> c |]
      [| for c in '0' .. '9' -> c |]
      [| '-'; '_' |] ]
    |> Array.concat

let isValidGameName (name: string) =
    Array.TrueForAll(Array.ofSeq name, (fun c -> Array.contains c validGameNameChars))

let add (game: string) (path: string) (glob: string option) config =
    if Array.exists (fun g -> g.Name = game) config.Games then
        printfn "Error: Game with the name %s already exists" game
        None
    elif not (isValidGameName game) then
        printfn
            "Error: Invalid characters in name `%s': only alphanumeric characters, underscores, and hyphens are allowed"
            game

        None
    else
        let newGames =
            Array.singleton
                { Name = game
                  Path = path
                  Glob = glob }
            |> Array.append config.Games
            |> Array.sortBy (fun g -> g.Name)

        printfn "Successfully added %s" game

        Some { config with Games = newGames }

let info (gameNames: string list option) (brief: bool) config =
    let games =
        match gameNames with
        | None -> config.Games
        | Some gs -> Array.filter (fun g -> List.contains g.Name gs) config.Games

    if brief then
        Seq.iter (fun g -> printfn "%s" g.Name) games
    else
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
        Array.filter
            (fun g ->
                if List.contains g.Name games
                   && (yes
                       || promptYorN ("Are you sure you want to remove " + g.Name + "?")) then
                    printfn "Removed %s" g.Name
                    false
                else
                    true)
            config.Games

    Some { config with Games = newGames }

let edit (gameName: string) (newName: string option) (newPath: string option) (newGlob: string option) config =
    match (newName, newPath, newGlob) with
    | (None, None, None) ->
        printfn "Error: One or more of --name, --path, or --glob must be provided."
        None
    | _ ->
        let mSplitList =
            Array.tryFindIndex (fun g -> g.Name = gameName) config.Games
            |> Option.map (fun i -> Array.toList config.Games |> List.splitAt i)

        match mSplitList with
        | None ->
            warnMissingGames [ gameName ] config
            None
        | Some (_, []) ->
            printfn "Error: Couldn't find game in list"
            None
        | Some (front, game :: back) ->
            let newName' = Option.defaultValue game.Name newName

            let newGlob' =
                match newGlob with
                | Some "none" -> None
                | Some "" -> None
                | glob -> glob

            let newPath' = Option.defaultValue game.Path newPath

            let editedGame =
                { game with
                      Name = newName'
                      Path = newPath'
                      Glob = newGlob' }

            if not (isValidGameName newName') then
                printfn
                    "Error: Invalid characters in name `%s': only alphanumeric characters, underscores, and hyphens are allowed"
                    newName'

                None
            else
                printGame game (Some newName') (Some newPath') newGlob'

                let backupDirExists =
                    Directory.Exists(Path.Join(config.Path, gameName))

                if (Option.isSome newName && backupDirExists) then
                    printfn "Warning: Game name changed, renaming backup directory..."
                    Directory.Move(Path.Join(config.Path, gameName), Path.Join(config.Path, newName'))

                Some
                    { config with
                          Games =
                              List.concat [ front
                                            List.singleton editedGame
                                            back ]
                              |> List.toArray }

let printConfig config newBackupDir newBackupFreq newBackupsToKeep =
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

    let newBackupFreq =
        Option.defaultValue config.Frequency backupFreq

    let newBackupsToKeep =
        Option.defaultValue config.NumToKeep backupsToKeep

    printConfig config (Some newBackupDir) (Some newBackupFreq) (Some newBackupsToKeep)

    match (backupDir, backupFreq, backupsToKeep) with
    | (None, None, None) -> None
    | _ ->
        Some
            { config with
                  Path = newBackupDir
                  Frequency = newBackupFreq
                  NumToKeep = newBackupsToKeep }

let parser =
    ArgumentParser.Create<SbuArgs>(programName = "sbu")

let defaultConfigPath =
    Path.Join(Environment.GetFolderPath(Environment.SpecialFolder.UserProfile), ".sbufs", "config.json")

let defaultConfig =
    { Path = Path.Join(Environment.GetFolderPath(Environment.SpecialFolder.UserProfile), ".sbufs-backups")
      Frequency = 15
      NumToKeep = 20
      Games = [||] }

[<EntryPoint>]
let main argv =
    try
        let result =
            parser.ParseCommandLine(inputs = argv, raiseOnUsage = true)

        if result.Contains Version then
            printfn "sbu v0.0.1"
        else
            let configPath =
                result.TryGetResult Config_Path
                |> Option.defaultValue defaultConfigPath

            let config =
                try
                    configPath
                    |> File.ReadAllText
                    |> JsonSerializer.Deserialize<Config>
                with e ->
                    printfn "Warning: %s Using default configuration." e.Message
                    defaultConfig

            let command =
                match result.GetSubCommand() with
                | Backup sp -> backup (sp.TryGetResult BackupArgs.Games) (sp.Contains Loop) (sp.Contains Verbose)
                | Add sp -> add (sp.GetResult AddArgs.Game) (sp.GetResult AddArgs.Path) (sp.TryGetResult AddArgs.Glob)
                | Info sp -> info (sp.TryGetResult InfoArgs.Games) (sp.Contains Brief)
                | Remove sp -> remove (sp.GetResult Games) (sp.Contains Yes)
                | Edit sp ->
                    edit
                        (sp.GetResult Game)
                        (sp.TryGetResult Name)
                        (sp.TryGetResult EditArgs.Path)
                        (sp.TryGetResult Glob)
                | Config sp -> editConfig (sp.TryGetResult Path) (sp.TryGetResult Frequency) (sp.TryGetResult Keep)
                | Config_Path _
                | Version -> Some

            let newConfig = command config

            match newConfig with
            | None -> ()
            | Some c ->
                Directory.CreateDirectory(Directory.GetParent(configPath).FullName)
                |> ignore

                File.WriteAllText(configPath, JsonSerializer.Serialize(c))

    with e -> printfn "Error: %s" e.Message

    0
