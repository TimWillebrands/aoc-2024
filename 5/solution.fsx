open System.IO
open System

let fileName = if fsi.CommandLineArgs.Length > 1 then fsi.CommandLineArgs[1] else "5/example"
let input = fileName |> File.ReadAllText |> _.Split("\r\n\r\n")
let rules =
    input[0]
    |> _.Split("\r\n")
    |> Seq.map _.Split("|")
    |> Seq.map (fun x -> (int x[0], int x[1]))
let updates  =
    input[1]
    |> _.Split("\r\n")
    |> Seq.map (fun x -> x.Split(",") |> Seq.map int)

    
printfn "Rules: %A" rules
printfn "Update : %A" updates
