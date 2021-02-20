module Types

open FSharpPlus.Data

type Game =
    { Name: string
      Path: string
      Glob: string option }

type Config =
    { Path: string
      Frequency: int
      NumToKeep: int
      Games: Game [] }

type RunConfig = { Config: Config; Verbose: bool }

type App<'a> = Reader<RunConfig, 'a>
