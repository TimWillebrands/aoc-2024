open System.IO

let inputFile = fsi.CommandLineArgs[1]
let input = File.ReadAllText(inputFile)

let lists =
    input.Split [|'\n'|]
    |> Seq.filter(fun line -> line.Length > 0)
    |> Seq.map(fun line -> 
        line.Split([|"   "|], System.StringSplitOptions.RemoveEmptyEntries) 
        |> Seq.map int32)
    |> Seq.transpose

let occurances =
    lists 
    |> Seq.last
    |> Seq.groupBy id
    |> Seq.map (fun (num, occ) -> num, Seq.length occ)
    |> Map.ofSeq

let getFreq key =
    Map.tryFind key occurances
    |> Option.defaultValue 0 

let day1 = 
    lists
    |> Seq.map(Seq.sort)
    |> Seq.transpose
    |> Seq.map(fun pair -> 
        let maxElement = Seq.max pair
        let minElement = Seq.min pair
        maxElement - minElement)
    |> Seq.sum

let day2 =
    lists
    |> Seq.head 
    |> Seq.map(fun num -> num * getFreq num)
    |> Seq.sum


printfn "Lists: %A" lists
printfn "occurances: %A" occurances
printfn "Day 1: %i" day1
printfn "Day 2: %i" day2
