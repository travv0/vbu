// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

open System
open Argu

type BackupArgs =
    | [<MainCommand; Mandatory>] Games of games: string
    | [<AltCommandLine("-l")>] Loop
    | [<AltCommandLine("-v")>] Verbose

    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | Games _ -> "List of games to back up.  If not provided, will back up all games"
            | Loop -> "Keep running, backing up games at the interval specified in your config file"
            | Verbose -> "Print verbose output"

type SbuArgs =
    | [<CliPrefix(CliPrefix.None)>] Backup of ParseResults<BackupArgs>
    | Config of config_file: string
    | Version

    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | Backup _ -> "Backup your game saves"
            | Config _ -> "Path to configuration file"
            | Version -> "Print version information"

let backup games loop verbose = printfn $"{games} {loop} {verbose}"

let parser =
    ArgumentParser.Create<SbuArgs>(programName = "sbu")

[<EntryPoint>]
let main argv =
    try
        let result =
            parser.ParseCommandLine(inputs = argv, raiseOnUsage = true)

        match result.GetSubCommand() with
        | Backup sp -> backup (sp.GetResults Games) (sp.Contains Loop) (sp.Contains Verbose)
        | _ -> ()
    with e -> printfn "%s" e.Message

    0
