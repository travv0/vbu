open Argu
open System.Text.Json
open System
open System.IO

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

let mutable config =
    { Path = Path.Join(Environment.GetFolderPath(Environment.SpecialFolder.UserProfile), ".sbufs-backups")
      Frequency = 15
      NumToKeep = 20
      Games = [||] }

let backup (games: string list option) (loop: bool) (verbose: bool) =
    printfn $"{games} {loop} {verbose}"
    None

let validGameNameChars =
    [ [| for c in 'A' .. 'z' -> c |]
      [| for c in '0' .. '9' -> c |]
      [| '-'; '_' |] ]
    |> Array.concat

let isValidGameName (name: string) =
    Array.TrueForAll(Array.ofSeq name, (fun c -> Array.contains c validGameNameChars))

let add (game: string) (path: string) (glob: string option) =
    if Array.exists (fun g -> g.Name = game) config.Games then
        printfn "Game with the name %s already exists" game
        None
    else if not (isValidGameName game) then
        printfn
            "Invalid characters in name `%s': only alphanumeric characters, underscores, and hyphens are allowed"
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

let info (gameNames: string list option) (brief: bool) =
    let games =
        match gameNames with
        | None -> config.Games
        | Some gs -> Array.filter (fun g -> List.contains g.Name gs) config.Games

    if brief then
        Seq.iter (fun g -> printfn "%s" g.Name) games
    else
        Seq.iter
            (fun g ->
                printfn "Name: %s\nSave path: %s\n" g.Name g.Path

                match g.Glob with
                | None -> ()
                | Some glob -> printfn "Save glob: %s\n\n" glob)
            games

    None

let rec promptYorN prompt =
    stdout.Flush()
    printf "\n%s (y/n) " prompt

    match Console.ReadLine().Trim().ToLower() with
    | "y" -> true
    | "n" -> false
    | _ -> promptYorN prompt

let remove (games: string list) (yes: bool) =
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

let edit (game: string) (newName: string option) (newPath: string option) (newGlob: string option) =
    printfn $"{game} {newName} {newPath} {newGlob}"
    Some config

let editConfig (path: string option) (frequency: int option) (numToKeep: int option) =
    printfn $"{path} {frequency} {numToKeep}"
    Some config

let parser =
    ArgumentParser.Create<SbuArgs>(programName = "sbu")

let defaultConfigPath =
    Path.Join(Environment.GetFolderPath(Environment.SpecialFolder.UserProfile), ".sbufs", "config")

[<EntryPoint>]
let main argv =
    try
        let result =
            parser.ParseCommandLine(inputs = argv, raiseOnUsage = true)

        let configPath =
            result.TryGetResult Config_Path
            |> Option.defaultValue defaultConfigPath

        config <-
            try
                configPath
                |> File.ReadAllText
                |> JsonSerializer.Deserialize<Config>
            with e ->
                printfn "Warning: %s Using default configuration." e.Message
                config

        let newConfig =
            match result.GetSubCommand() with
            | Backup sp -> backup (sp.TryGetResult BackupArgs.Games) (sp.Contains Loop) (sp.Contains Verbose)
            | Add sp -> add (sp.GetResult AddArgs.Game) (sp.GetResult AddArgs.Path) (sp.TryGetResult AddArgs.Glob)
            | Info sp -> info (sp.TryGetResult InfoArgs.Games) (sp.Contains Brief)
            | Remove sp -> remove (sp.GetResult Games) (sp.Contains Yes)
            | Edit sp ->
                edit (sp.GetResult Game) (sp.TryGetResult Name) (sp.TryGetResult EditArgs.Path) (sp.TryGetResult Glob)
            | Config sp -> editConfig (sp.TryGetResult Path) (sp.TryGetResult Frequency) (sp.TryGetResult Keep)
            | Config_Path _
            | Version -> Some config

        match newConfig with
        | None -> ()
        | Some c ->
            Directory.CreateDirectory(Directory.GetParent(configPath).FullName)
            |> ignore

            File.WriteAllText(configPath, JsonSerializer.Serialize(c))

    with e -> printfn "Error: %s" e.Message

    0
