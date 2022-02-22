module Util

open System
open System.IO

[<AutoOpen; RequireQualifiedAccess>]
module Terminal =
    let withColor color f =
        Console.ForegroundColor <- color
        f ()
        Console.ResetColor()

    let printWithColor color s =
        withColor color (fun () -> printfn "%s" s)

    let note verbose s =
        if verbose then
            "Note: " + s |> printWithColor ConsoleColor.Blue

    let warn s =
        "Warning: " + s
        |> printWithColor ConsoleColor.Yellow

    let err s =
        "Error: " + s |> printWithColor ConsoleColor.Red

    let rec promptYorN prompt =
        stdout.Flush()
        printf $"%s{prompt} (y/N) "

        match Console.ReadLine().Trim().ToLower() with
        | "y" -> true
        | "n"
        | "" -> false
        | _ -> promptYorN prompt

[<AutoOpen; RequireQualifiedAccess>]
module FileSystem =
    let (+/) (path1: string) path2 = Path.Join(path1, path2)

    let absolutePath (path: string) =
        if path.Length > 0 && path.[0] = '~' then
            let home =
                Environment.GetFolderPath(Environment.SpecialFolder.UserProfile)

            home +/ path.[1..] |> Path.GetFullPath
        else
            Path.GetFullPath path

    let defaultConfigPath =
        Environment.GetFolderPath(Environment.SpecialFolder.UserProfile)
        +/ ".vbu"
        +/ "config.json"

[<AutoOpen>]
module Operators =

    [<RequiresExplicitTypeArguments>]
    /// <summary>
    /// Ignore the passed value. This is often used to throw away results of a computation.
    /// </summary>
    /// <param name="value">The value to ignore</param>
    /// <typeparam name="'a">the type of value to expect, and ignore</typeparam>
    /// <returns>unit</returns>
    let ignore<'a> (value: 'a) = ignore<'a> value
