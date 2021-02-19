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

type App<'a> = Reader<Config, 'a>
