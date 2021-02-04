open Argu
open DotNet.Globbing
open System
open System.IO
open System.Text.Json
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

[<HelpDescription("Show this help text"); HelpFlags("--help", "-h"); NoAppSettings>]
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

let withColor color f =
    Console.ForegroundColor <- color
    f ()
    Console.ResetColor()

let printWithColor color s =
    withColor color (fun () -> printfn "%s" s)

let warn (s: string) =
    "Warning: " + s
    |> printWithColor ConsoleColor.Yellow

let err (s: string) =
    "Error: " + s |> printWithColor ConsoleColor.Red

let warnMissingGames games config =
    let mutable warningPrinted = false

    for game in games do
        if not (Seq.exists (fun g -> g.Name = game) config.Games) then
            warn $"No game named `{game}'"
            warningPrinted <- true

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
                Seq.sortWith (fun f1 f2 -> compare (File.GetLastWriteTimeUtc f2) (File.GetLastWriteTimeUtc f1)) files

            let filesToDelete = Seq.skip config.NumToKeep sortedFiles

            for file in filesToDelete do
                if verbose then warn $"Deleting {file}"
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
            let fromModTime = File.GetLastWriteTimeUtc(fromPath)

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

                printfn
                    "\nFinished backing up %d file%s%s for %s in %fs on %s at %s\n"
                    backedUpCount
                    (if backedUpCount = 1 then "" else "s")
                    (if Seq.length warnings > 0 then
                         sprintf
                             " with %d warning%s"
                             (Seq.length warnings)
                             (if Seq.length warnings = 1 then
                                  ""
                              else
                                  "s")
                     else
                         "")
                    gameName
                    (now - startTime).TotalSeconds
                    (now.ToLongDateString())
                    (now.ToLongTimeString())

            warnings
        else
            warn $"Path set for {gameName} doesn't exist: {game.Path}"
            Seq.empty
    | None ->
        warnMissingGames [ gameName ] config
        Seq.empty

let rec backup (gameNames: string list option) (loop: bool) (verbose: bool) config =
    let gameNames' =
        match gameNames with
        | None -> Seq.map (fun g -> g.Name) config.Games
        | Some gns -> List.toSeq gns

    let warnings =
        Seq.fold
            (fun acc game ->
                let warnings =
                    try
                        backupGame game verbose config
                    with e ->
                        err $"Error backing up {game}: {e.Message}"
                        Seq.empty

                Seq.append acc warnings)
            Seq.empty
            gameNames'

    if not (Seq.isEmpty warnings) then
        withColor
            ConsoleColor.Yellow
            (fun () ->
                printf
                    "\n%d warning%s occurred:"
                    (Seq.length warnings)
                    (if Seq.length warnings = 1 then
                         ""
                     else
                         "s")

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

let validGameNameChars =
    [ [| for c in 'A' .. 'z' -> c |]
      [| for c in '0' .. '9' -> c |]
      [| '-'; '_' |] ]
    |> Array.concat

let isValidGameName (name: string) =
    Array.TrueForAll(Array.ofSeq name, (fun c -> Array.contains c validGameNameChars))

let absolutePath (path: string) =
    if path.Length > 0 && path.[0] = '~' then
        let home =
            Environment.GetFolderPath(Environment.SpecialFolder.UserProfile)

        Path.Join(home, path.[1..]) |> Path.GetFullPath
    else
        Path.GetFullPath path

let add (game: string) (path: string) (glob: string option) config =
    if Seq.exists (fun g -> g.Name = game) config.Games then
        err $"Error: Game with the name {game} already exists"
        None
    elif not (isValidGameName game) then
        err "Invalid characters in name `{game}': only alphanumeric characters, underscores, and hyphens are allowed"

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
            |> Seq.toArray

        printfn "Successfully added %s\n" game
        printGame newGame None None None

        Some { config with Games = newGames }

let info (gameNames: string list option) (brief: bool) config =
    match gameNames with
    | Some gns -> warnMissingGames gns config
    | None -> ()

    let games =
        match gameNames with
        | None -> Array.toSeq config.Games
        | Some gs -> Seq.filter (fun g -> Seq.contains g.Name gs) config.Games

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
        [| for game in config.Games do
            if Seq.contains game.Name games
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
            |> Option.map (fun i -> Seq.toList config.Games |> List.splitAt i)

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

            let newPath' = Option.defaultValue game.Path newPath

            let editedGame =
                { game with
                      Name = newName'
                      Path = absolutePath newPath'
                      Glob = newGlob' }

            if not (isValidGameName newName') then
                err
                    $"Invalid characters in name `{newName'}': only alphanumeric characters, underscores, and hyphens are allowed"

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
                              |> Seq.toArray }

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
                  Path = absolutePath newBackupDir
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
            printfn "sbu v0.0.4"
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
                    warn $"{e.Message} Using default configuration."
                    defaultConfig

            let command = result.GetSubCommand()

            let run =
                match command with
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
                | Version -> failwithf "non-command matched as command: %s" (command.ToString())

            let newConfig = run config

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
