open System.IO
open System

let len = 10
let fileName = if fsi.CommandLineArgs.Length > 1 then fsi.CommandLineArgs[1] else "4/example"
let input =
    fileName
    |> File.ReadAllText
    |> _.ReplaceLineEndings("")
    |> (fun i -> Array2D.init len len (fun y x -> i[x + y * len]))

let lookup =
    [|
        [|0;1;2;3|]    
        [|0;-1;-2;-3|]    
    |]
    
let tryGet (array: 'T[,]) y x =
    if x >= 0 && x < Array2D.length1 array && y >= 0 && y < Array2D.length2 array then
        Some array[y, x]
    else
        None

let checkPosition y x=
    [|0;1;2;3|]
    |> Seq.map (fun xTrans -> tryGet input y (x + xTrans))
    |> String.Concat
    // |> (fun s -> s = "XMAS")

let day1 =
    Array2D.init len len checkPosition
    
printfn "Input: %A" input
printfn "Day1: %A" day1
