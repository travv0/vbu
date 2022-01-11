module Types

open System
open System.IO
open System.Text.Json
open Util.FileSystem

let printConfigRow label value newValue =
    printfn "%s: %s%s" label value
    <| match newValue with
       | Some nv when value = nv -> ""
       | Some nv -> $" -> %s{nv}"
       | None -> ""

type Group =
    { Name: string
      Path: string
      Glob: option<string> }

module Group =
    let defaultGlob = "**/*"

    let printUpdated group newName newPath newGlob =
        printConfigRow "Name" group.Name newName
        printConfigRow "Path" group.Path newPath

        match (group.Glob, newGlob) with
        | Some _, _
        | None, Some _ -> printConfigRow "Glob" (Option.defaultValue "" group.Glob) newGlob
        | _ -> ()

        printfn ""

    let print group = printUpdated group None None None

    let private validNameChars: Set<char> =
        [ 'A' .. 'Z' ]
        @ [ 'a' .. 'z' ] @ [ '0' .. '9' ] @ [ '-'; '_' ]
        |> Set.ofList

    let isValidName (name: string) =
        String.forall (fun c -> Set.contains c validNameChars) name


type Config =
    { Path: string
      Frequency: int
      NumToKeep: int
      Groups: Group [] }

module Config =
    open Util.Terminal

    let def =
        { Path =
            Environment.GetFolderPath(Environment.SpecialFolder.UserProfile)
            +/ ".vbu-backups"
          Frequency = 15
          NumToKeep = 20
          Groups = [||] }

    let saveDefault path =
        printfn
            $"Creating new config file at `%s{path}'.\n\
            Use the `config' command to update default values, which are:\n\
            \n\
            Backup path: %s{def.Path}\n\
            Backup frequency (in minutes): %d{def.Frequency}\n\
            Number of backups to keep: %d{def.NumToKeep}\n"

        match Path.GetDirectoryName(path: string) with
        | "" -> ()
        | path -> Directory.CreateDirectory(path) |> ignore

        File.WriteAllText(path, JsonSerializer.Serialize(def))

    let printUpdated config newBackupDir newBackupFreq newBackupsToKeep =
        printConfigRow "Backup path" config.Path newBackupDir
        printConfigRow "Backup frequency (in minutes)" (string config.Frequency) (Option.map string newBackupFreq)
        printConfigRow "Number of backups to keep" (string config.NumToKeep) (Option.map string newBackupsToKeep)
        printfn ""

    let print config = printUpdated config None None None

    let load path =
        if not (File.Exists(path)) then
            saveDefault path

        try
            path
            |> File.ReadAllText
            |> JsonSerializer.Deserialize<Config>
        with
        | e ->
            warn
                $"Couldn't load config: %s{e.Message}\nAttempting to save default config \
                to '%s{path}' after backing up existing config.\n"

            if File.Exists(path) then
                File.Copy(path, path + ".bak", true)

            saveDefault path
            def

    let save path (config: Config) =
        File.WriteAllText(path, JsonSerializer.Serialize(config))
