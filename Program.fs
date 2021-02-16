module Program

open Argu
open DotNet.Globbing
open FSharpPlus
open FSharpPlus.Data
open System
open System.IO
open System.Text.Json
open System.Threading

type Game =
    { Name: string
      Path: string
      Glob: string option }

type Config =
    { Path: string
      Frequency: int
      NumToKeep: int
      Games: Game [] }

type App<'a> = Reader<Config, 'a>

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

type ListArgs =
    | [<Hidden>] Hidden

    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | Hidden -> ""

type InfoArgs =
    | [<MainCommand>] Games of games: string list

    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | Games _ ->
                "List of games to display info for. If not \
                provided, will display info for all games"

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
    | [<CliPrefix(CliPrefix.None)>] List of ParseResults<ListArgs>
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
            | List _ -> "List names of games that can be backed up"
            | Info _ -> "List info for games"
            | Remove _ -> "Remove games from backup"
            | Edit _ -> "Edit game info"
            | Config _ -> "Manage sbu configuration"
            | Config_Path _ -> "Path to configuration file"
            | Version -> "Print version information"

let defaultGlob = "**/*"

let withColor color f =
    Console.ForegroundColor <- color
    f ()
    Console.ResetColor()

let printWithColor color s =
    withColor color (fun () -> printfn "%s" s)

let warn s =
    "Warning: " + s
    |> printWithColor ConsoleColor.Yellow

let err s =
    "Error: " + s |> printWithColor ConsoleColor.Red

let inline warnMissingGames games: App<unit> =
    monad {
        let! config = ask

        let warningPrinted =
            fold
                (fun warningPrinted game ->
                    if not (exists (fun g -> g.Name = game) config.Games) then
                        warn $"No game named `{game}'"
                        true
                    else
                        warningPrinted)
                false
                games

        if warningPrinted then printfn ""
    }

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

let cleanupBackups (backupPath: string) verbose: App<unit> =
    monad {
        let! config = ask

        if config.NumToKeep > 0 then
            let glob =
                Glob.Parse(
                    backupPath
                    + ".bak.[0-9][0-9][0-9][0-9]_[0-9][0-9]_[0-9][0-9]_[0-9][0-9]_[0-9][0-9]_[0-9][0-9]"
                )

            let allFiles =
                Directory.EnumerateFiles(Path.GetDirectoryName(backupPath))

            let files =
                filter (fun f -> glob.IsMatch(f: string)) allFiles
                |> plus (result backupPath)

            if (length files > config.NumToKeep) then
                let sortedFiles =
                    sortByDescending File.GetLastWriteTimeUtc files

                let filesToDelete = skip config.NumToKeep sortedFiles

                for file in filesToDelete do
                    if verbose then warn $"Deleting {file}"
                    File.Delete(file)
    }

let rec backupFile game basePath glob fromPath toPath verbose: App<int * string seq> =
    monad {
        try
            let globMatches () =
                let glob =
                    Glob.Parse(Path.Join(basePath, Option.defaultValue defaultGlob glob))

                glob.IsMatch(fromPath: string)

            let copyAndCleanup () =
                monad {
                    Directory.CreateDirectory(Path.GetDirectoryName(toPath: string))
                    |> ignore

                    printfn "%s ==>\n\t%s" fromPath toPath
                    File.Copy(fromPath, toPath)
                    do! cleanupBackups toPath verbose
                    return (1, empty)
                }

            let backupFile' () =
                monad {
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

                            return! copyAndCleanup ()
                        else
                            return (0, empty)
                    | None -> return! copyAndCleanup ()
                }

            if Directory.Exists(fromPath) then
                return! backupFiles game basePath glob fromPath toPath verbose
            else if globMatches () then
                return! backupFile' ()
            else
                return (0, empty)
        with e ->
            let warning =
                sprintf "Unable to backup file %s for game %s:\n%s\n" toPath game e.Message

            warn warning
            return (1, result warning)
    }

and backupFiles game basePath glob fromPath toPath verbose: App<int * string seq> =
    monad {
        return!
            Directory.EnumerateFileSystemEntries(fromPath)
            |> toList
            |> List.foldM
                (fun (c, es) path ->
                    monad {
                        let file = Path.GetFileName(path)

                        let! (newCount, newErrs) =
                            backupFile game basePath glob (Path.Join(fromPath, file)) (Path.Join(toPath, file)) verbose

                        return (c + newCount, es ++ newErrs)
                    })
                (0, empty)
    }

let backupGame gameName verbose: App<string seq> =
    monad {
        let! config = ask
        let startTime = DateTime.Now

        let game =
            tryFind (fun g -> g.Name = gameName) config.Games

        match game with
        | Some game ->
            if Directory.Exists game.Path then
                let! (backedUpCount, warnings) =
                    backupFiles game.Name game.Path game.Glob game.Path (Path.Join(config.Path, gameName)) verbose

                if (backedUpCount > 0) then
                    let now = DateTime.Now

                    printfn
                        "\nFinished backing up %d file%s%s for %s in %fs on %s at %s\n"
                        backedUpCount
                        (if backedUpCount = 1 then "" else "s")
                        (if length warnings > 0 then
                             sprintf " with %d warning%s" (length warnings) (if length warnings = 1 then "" else "s")
                         else
                             "")
                        gameName
                        (now - startTime).TotalSeconds
                        (now.ToLongDateString())
                        (now.ToLongTimeString())

                return warnings
            else
                warn $"Path set for {gameName} doesn't exist: {game.Path}"
                return empty
        | None ->
            do! warnMissingGames [ gameName ]
            return empty
    }

let rec backup (gameNames: string list option) (loop: bool) (verbose: bool): App<Config option> =
    monad {
        let! config = ask

        let gameNames' =
            match gameNames with
            | None -> map (fun g -> g.Name) config.Games
            | Some gns -> ofList gns

        let! warnings =
            gameNames'
            |> toList
            |> List.foldM
                (fun acc game ->
                    monad {
                        try
                            let! warnings = backupGame game verbose
                            return acc ++ warnings
                        with e ->
                            err $"Error backing up {game}: {e.Message}"
                            return acc
                    })
                empty

        if length warnings > 0 then
            withColor
                ConsoleColor.Yellow
                (fun () ->
                    printf "\n%d warning%s occurred:" (length warnings) (if length warnings = 1 then "" else "s")

                    if verbose then
                        printfn "\n"
                        iter (printfn "%s") warnings
                    else
                        printfn "\nPass --verbose flag to print all warnings after backup completes\n")

        if loop then
            Thread.Sleep(TimeSpan.FromMinutes(float config.Frequency))
            return! backup gameNames loop verbose
        else
            return None
    }

let validGameNameChars =
    [| 'A' .. 'Z' |]
    ++ [| 'a' .. 'z' |]
    ++ [| '0' .. '9' |]
    ++ [| '-'; '_' |]

let isValidGameName (name: string) =
    forall (fun c -> exists ((=) c) validGameNameChars) name

let absolutePath (path: string) =
    if path.Length > 0 && path.[0] = '~' then
        let home =
            Environment.GetFolderPath(Environment.SpecialFolder.UserProfile)

        Path.Join(home, path.[1..]) |> Path.GetFullPath
    else
        Path.GetFullPath path

let add (game: string) (path: string) (glob: string option): App<Config option> =
    monad {
        let! config = ask

        if exists (fun g -> g.Name = game) config.Games then
            err $"Error: Game with the name {game} already exists"
            return None
        elif not (isValidGameName game) then
            err
                "Invalid characters in name `{game}': only alphanumeric characters, underscores, and hyphens are allowed"

            return None
        else
            let newGame =
                { Name = game
                  Path = absolutePath path
                  Glob = glob }

            let newGames =
                result newGame
                |> plus config.Games
                |> sortBy (fun g -> g.Name)
                |> toArray

            printfn "Successfully added %s\n" game
            printGame newGame None None None

            return Some { config with Games = newGames }
    }

let list (): App<Config option> =
    monad {
        let! config = ask
        iter (fun g -> printfn "%s" g.Name) config.Games
        return None
    }

let info (gameNames: string list option): App<Config option> =
    monad {
        let! config = ask

        match gameNames with
        | Some gns -> do! warnMissingGames gns
        | None -> ()

        let games =
            match gameNames with
            | None -> config.Games
            | Some gs -> filter (fun g -> exists ((=) g.Name) gs) config.Games

        iter (fun g -> printGame g None None None) games
        return None
    }

let rec promptYorN prompt =
    stdout.Flush()
    printf "\n%s (y/n) " prompt

    match Console.ReadLine().Trim().ToLower() with
    | "y" -> true
    | "n" -> false
    | _ -> promptYorN prompt

let remove (games: string NonEmptyList) (yes: bool): App<Config option> =
    monad {
        let! config = ask
        do! warnMissingGames games

        let newGames =
            [| for game in config.Games do
                   if exists ((=) game.Name) games
                      && (yes
                          || promptYorN (
                              "Are you sure you want to remove "
                              + game.Name
                              + "?"
                          )) then
                       printfn "Removed %s" game.Name
                   else
                       yield game |]

        return Some { config with Games = newGames }
    }

let edit
    (gameName: string)
    (newName: string option)
    (newPath: string option)
    (newGlob: string option)
    : App<Config option> =
    monad {
        let! config = ask

        match (newName, newPath, newGlob) with
        | (None, None, None) ->
            err "One or more of --name, --path, or --glob must be provided."
            return None
        | _ ->
            let splitList =
                tryFindIndex (fun g -> g.Name = gameName) config.Games
                |> map (fun i -> toList config.Games |> List.splitAt i)

            match splitList with
            | None ->
                do! warnMissingGames [ gameName ]
                return None
            | Some (_, []) ->
                err "Couldn't find game in list"
                return None
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
                        $"Invalid characters in name `{newName'}': only alphanumeric characters, underscores, and hyphens are allowed"

                    return None
                else
                    printGame game (Some newName') (Some newPath') newGlob'

                    let backupDirExists =
                        Directory.Exists(Path.Join(config.Path, gameName))

                    if (Option.isSome newName && backupDirExists) then
                        warn "Game name changed, renaming backup directory..."
                        Directory.Move(Path.Join(config.Path, gameName), Path.Join(config.Path, newName'))

                    return
                        Some
                            { config with
                                  Games = front ++ result editedGame ++ back |> toArray }
    }

let printConfig newBackupDir newBackupFreq newBackupsToKeep: App<unit> =
    monad {
        let! config = ask
        printConfigRow "Backup path" config.Path newBackupDir

        printConfigRow
            "Backup frequency (in minutes)"
            (config.Frequency.ToString())
            (map (fun freq -> freq.ToString()) newBackupFreq)

        printConfigRow
            "Number of backups to keep"
            (config.NumToKeep.ToString())
            (map (fun toKeep -> toKeep.ToString()) newBackupsToKeep)

        printfn ""
    }

let editConfig backupDir backupFreq backupsToKeep: App<Config option> =
    monad {
        let! config = ask

        let newBackupDir =
            Option.defaultValue config.Path backupDir
            |> absolutePath

        let newBackupFreq =
            Option.defaultValue config.Frequency backupFreq

        let newBackupsToKeep =
            Option.defaultValue config.NumToKeep backupsToKeep

        do! printConfig (Some newBackupDir) (Some newBackupFreq) (Some newBackupsToKeep)

        match (backupDir, backupFreq, backupsToKeep) with
        | (None, None, None) -> return None
        | _ ->
            return
                Some
                    { config with
                          Path = newBackupDir
                          Frequency = newBackupFreq
                          NumToKeep = newBackupsToKeep }
    }

let parser =
    ArgumentParser.Create<SbuArgs>(programName = "sbu")

let defaultConfigPath =
    Path.Join(Environment.GetFolderPath(Environment.SpecialFolder.UserProfile), ".sbufs", "config.json")

let defaultConfig =
    { Path = Path.Join(Environment.GetFolderPath(Environment.SpecialFolder.UserProfile), ".sbufs-backups")
      Frequency = 15
      NumToKeep = 20
      Games = empty }

let saveDefaultConfig path =
    match Path.GetDirectoryName(path: string) with
    | "" -> ()
    | path -> Directory.CreateDirectory(path) |> ignore

    File.WriteAllText(path, JsonSerializer.Serialize(defaultConfig))

let app (parseResults: ParseResults<_>): App<Config option> =
    monad {
        let command = parseResults.GetSubCommand()

        return!
            match command with
            | Backup sp -> backup (sp.TryGetResult BackupArgs.Games) (sp.Contains Loop) (sp.Contains Verbose)
            | Add sp -> add (sp.GetResult AddArgs.Game) (sp.GetResult AddArgs.Path) (sp.TryGetResult AddArgs.Glob)
            | List _ -> list ()
            | Info sp -> info (sp.TryGetResult InfoArgs.Games)
            | Remove sp -> remove (NonEmptyList.ofList (sp.GetResult Games)) (sp.Contains Yes)
            | Edit sp ->
                edit (sp.GetResult Game) (sp.TryGetResult Name) (sp.TryGetResult EditArgs.Path) (sp.TryGetResult Glob)
            | Config sp -> editConfig (sp.TryGetResult Path) (sp.TryGetResult Frequency) (sp.TryGetResult Keep)
            | Config_Path _
            | Version -> failwithf "non-command matched as command: %s" (command.ToString())
    }

let loadConfig configPath =
    if not (File.Exists(configPath)) then
        saveDefaultConfig configPath

    try
        configPath
        |> File.ReadAllText
        |> JsonSerializer.Deserialize<Config>
    with e ->
        sprintf
            "Couldn't load config: %s\nAttempting to save default config \
                    to '%s' after backing up existing config.\n"
            e.Message
            configPath
        |> warn

        if File.Exists(configPath) then
            File.Copy(configPath, configPath + ".bak", true)

        saveDefaultConfig configPath
        defaultConfig

let runApp = Reader.run << app

[<EntryPoint>]
let main argv =
    try
        let parseResults =
            parser.ParseCommandLine(inputs = argv, raiseOnUsage = true)

        if parseResults.Contains Version then
            printfn "sbu v1.0.0"
        else
            let configPath =
                parseResults.TryGetResult Config_Path
                |> Option.defaultValue defaultConfigPath

            let config = loadConfig configPath
            let newConfig = runApp parseResults config

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
