open System
open Argu

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

    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | Games _ ->
                "List of games to display info for. If not \
                provided, will display info for all games"

type ListArgs =
    | Verbose

    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | Verbose -> "Show verbose output"

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
            | List _ -> "List games that can be backed up"
            | Info _ -> "List info for games"
            | Remove _ -> "Remove games from backup"
            | Edit _ -> "Edit game info"
            | Config _ -> "Manage sbu configuration"
            | Config_Path _ -> "Path to configuration file"
            | Version -> "Print version information"

let backup (games: string list option) (loop: bool) (verbose: bool) = printfn $"{games} {loop} {verbose}"
let add (game: string) (path: string) (glob: string option) = printfn $"{game} {path} {glob}"
let list (verbose: bool) = printfn $"{verbose}"
let info (games: string list option) = printfn $"{games}"
let remove (games: string list) (yes: bool) = printfn $"{games} {yes}"

let edit (game: string) (newName: string option) (newPath: string option) (newGlob: string option) =
    printfn $"{game} {newName} {newPath} {newGlob}"

let config (path: string option) (frequency: int option) (numToKeep: int option) =
    printfn $"{path} {frequency} {numToKeep}"

let parser =
    ArgumentParser.Create<SbuArgs>(programName = "sbu")

[<EntryPoint>]
let main argv =
    try
        let result =
            parser.ParseCommandLine(inputs = argv, raiseOnUsage = true)

        match result.GetSubCommand() with
        | Backup sp -> backup (sp.TryGetResult BackupArgs.Games) (sp.Contains Loop) (sp.Contains BackupArgs.Verbose)
        | Add sp -> add (sp.GetResult AddArgs.Game) (sp.GetResult AddArgs.Path) (sp.TryGetResult AddArgs.Glob)
        | List sp -> list (sp.Contains Verbose)
        | Info sp -> info (sp.TryGetResult InfoArgs.Games)
        | Remove sp -> remove (sp.GetResult Games) (sp.Contains Yes)
        | Edit sp ->
            edit (sp.GetResult Game) (sp.TryGetResult Name) (sp.TryGetResult EditArgs.Path) (sp.TryGetResult Glob)
        | Config sp -> config (sp.TryGetResult Path) (sp.TryGetResult Frequency) (sp.TryGetResult Keep)
        | _ -> ()
    with e -> printfn "%s" e.Message

    0
