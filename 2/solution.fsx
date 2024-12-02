open System.IO

let inputFile = fsi.CommandLineArgs[1]
let input = File.ReadAllText(inputFile)

let removeAt idx (s: seq<'T>) =
    Seq.append (Seq.take idx s) (Seq.skip (idx + 1) s)

let isValidRow row =
    row
    |> Seq.pairwise 
    |> Seq.map(fun (a, b) -> (a > b, abs (a - b)))
    |> Seq.fold
        (fun (isConsistent, prevDir, isInBounds) (currInc, distance) ->  
            let consistent = isConsistent && (prevDir = None || prevDir = Some currInc)
            let inBounds = isInBounds && distance >= 1 && distance <= 3

            (consistent, Some currInc, inBounds))
        (true, None, true)
    |> fun (a, _, c) -> a && c

let lists =
    input.Split [|'\n'|]
    |> Seq.filter(fun line -> line.Length > 0)
    |> Seq.map(fun line -> 
        line.Split(' ', System.StringSplitOptions.RemoveEmptyEntries) 
        |> Seq.map int32)

let day1 = 
    lists
    |> Seq.map isValidRow
    |> Seq.filter (fun a -> a)

let day2 = 
    lists
    |> Seq.filter (fun row ->
        if isValidRow row then true
        else
            row
            |> Seq.mapi (fun idx _ -> removeAt idx row)
            |> Seq.exists isValidRow)

printfn "Day1: %i" (day1 |> Seq.length)
printfn "Day2: %i" (day2 |> Seq.length)
