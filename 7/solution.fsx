open System.Diagnostics
open System.IO

let input =
    fsi.CommandLineArgs[1]
    |> File.ReadLines
    |> Seq.map _.Split(":")
    |> Seq.map (fun x ->
        (int64 x[0], x[1].Split(" ")
        |> Seq.filter (fun x -> x.Length > 0)
        |> Seq.map int64
        |> List.ofSeq))

let reduce (target, numbers) doConcat =
    let rec calc operands =
        let rec evaluate operands currentValue =
            match operands with
            | [] -> currentValue = target
            | next :: rest ->
                let add = currentValue + next
                let addPath = add <= target && evaluate rest add
                
                let mult = currentValue * next
                let multiplyPath = mult <= target && evaluate rest mult
                
                if doConcat then
                    let concat = int64 ((string currentValue) + (string next))
                    let concatPath = concat <= target && evaluate rest concat
                    addPath || multiplyPath || concatPath
                else
                    addPath || multiplyPath

        match operands with
        | [] -> false
        | initial :: rest -> evaluate rest initial || evaluate rest initial

    calc numbers

let sw = Stopwatch.StartNew()
let day1 =
    input
    |> Seq.map (fun i -> (i, reduce i false))
    |> Seq.filter snd
    |> Seq.sumBy (fun (i, _) -> (fst i))
let day1ms = sw.ElapsedMilliseconds
sw.Restart()
let day2 =
    input
    |> Seq.map (fun i -> (i, reduce i true))
    |> Seq.filter snd
    |> Seq.sumBy (fun (i, _) -> (fst i))
let day2ms = sw.ElapsedMilliseconds

printfn "Day1: %A, calc in %i" day1 day1ms
printfn "Day2: %A, calc in %i" day2 day2ms