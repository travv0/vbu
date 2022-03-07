module Args

open Argu
open Types

type BackupArgs =
    | [<MainCommand>] Groups of groups: list<string>
    | [<AltCommandLine("-l")>] Loop
    | [<AltCommandLine("-v")>] Verbose
    | [<AltCommandLine("-f")>] Force

    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | Groups _ ->
                "List of groups to back up.  If not provided, will back up all groups"
            | Loop ->
                "Keep running, backing up groups at the interval specified in your config file"
            | Verbose -> "Print verbose output"
            | Force ->
                "Force all file backups to overwrite any conflicting files"

type AddArgs =
    | [<MainCommand; Mandatory; Unique>] Group of group: string
    | [<AltCommandLine("-p"); Mandatory>] Path of path: string
    | [<AltCommandLine("-g")>] Glob of glob: string

    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | Group _ -> "Name of group to add"
            | Path _ -> "Path to added group's files"
            | Glob _ ->
                $"File glob for added group's files. Only files \
                matching this pattern will be backed up. The default is \
                %s{Group.defaultGlob} which will recursively back up all files in PATH"

type ListArgs =
    | [<Hidden>] Hidden

    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | Hidden -> ""

type InfoArgs =
    | [<MainCommand>] Groups of groups: list<string>

    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | Groups _ ->
                "List of groups to display info for. If not \
                provided, will display info for all groups"

type RemoveArgs =
    | [<MainCommand; Mandatory>] Groups of groups: list<string>
    | [<AltCommandLine("-y")>] Yes

    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | Groups _ -> "List of groups to remove"
            | Yes -> "Remove all without confirmation prompts"

type EditArgs =
    | [<MainCommand; Unique; Mandatory>] Group of group: string
    | [<AltCommandLine("-n")>] Name of new_name: string
    | [<AltCommandLine("-p")>] Path of new_path: string
    | [<AltCommandLine("-g")>] Glob of new_glob: string

    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | Group _ -> "Name of group to edit"
            | Name _ ->
                "Set group name to NEW_NAME. This will also update the \
                directory name in your backup directory"
            | Path _ -> "Set group's path to NEW_PATH"
            | Glob _ ->
                $"Set group's file glob to NEW_FILE_GLOB. \
                Setting this to an empty string or \"none\" implies the \
                glob %s{Group.defaultGlob} which will recursively back up all files"

type ConfigArgs =
    | [<AltCommandLine("-p")>] Path of backup_path: string
    | [<AltCommandLine("-f")>] Frequency of backup_frequency: int
    | [<AltCommandLine("-k")>] Keep of backups_to_keep: int

    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | Path _ -> "Set path to directory in which to back up files"
            | Frequency _ ->
                "Set frequency in minutes to backup files when looping"
            | Keep _ -> "Set how many copies of each backed-up file to keep"

[<HelpDescription("Show this help text");
  HelpFlags("--help", "-h");
  NoAppSettings>]
type vbuArgs =
    | [<CliPrefix(CliPrefix.None)>] Backup of ParseResults<BackupArgs>
    | [<CliPrefix(CliPrefix.None)>] Add of ParseResults<AddArgs>
    | [<CliPrefix(CliPrefix.None)>] List of ParseResults<ListArgs>
    | [<CliPrefix(CliPrefix.None)>] Info of ParseResults<InfoArgs>
    | [<CliPrefix(CliPrefix.None)>] Remove of ParseResults<RemoveArgs>
    | [<CliPrefix(CliPrefix.None)>] Edit of ParseResults<EditArgs>
    | [<CliPrefix(CliPrefix.None)>] Config of ParseResults<ConfigArgs>
    | [<CustomCommandLine("--config"); AltCommandLine("-c")>] ConfigPath of
        config_path: string
    | [<AltCommandLine("-V")>] Version

    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | Backup _ -> "Backup your files"
            | Add _ -> "Add groups to backup"
            | List _ -> "List names of groups that can be backed up"
            | Info _ -> "List info for groups"
            | Remove _ -> "Remove groups from backup"
            | Edit _ -> "Edit group info"
            | Config _ -> "Manage vbu configuration"
            | ConfigPath _ -> "Path to configuration file"
            | Version -> "Print version information"
