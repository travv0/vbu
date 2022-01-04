module Types

type Game =
    { Name: string
      Path: string
      Glob: option<string> }

type Config =
    { Path: string
      Frequency: int
      NumToKeep: int
      Games: Game [] }
