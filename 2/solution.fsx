open System.IO

let inputFile = fsi.CommandLineArgs[1]
let input = File.ReadAllText(inputFile)

let lists =
    input.Split [|'\n'|]
    |> Seq.filter(fun line -> line.Length > 0)
    |> Seq.map(fun line -> 
        line.Split(' ', System.StringSplitOptions.RemoveEmptyEntries) 
        |> Seq.map int32)

let day1 = 
    lists
    |> Seq.map(fun row -> 
        row
        |> Seq.pairwise 
        |> Seq.map(fun (a, b) -> (a > b, abs (a - b)))
        |> Seq.fold
            (fun (isConsistent, prevDir, isInBounds) (currInc, distance) ->  
                let consistent = isConsistent && (prevDir = None || prevDir = Some currInc)
                let inBounds = isInBounds && distance >= 1 && distance <= 3

                (consistent, Some currInc, inBounds))
            (true, None, true))
    |> Seq.filter (fun (a, _, c) -> a && c)

printfn "Lists: %A" day1
printfn "Day1: %i" (day1 |> Seq.length)
