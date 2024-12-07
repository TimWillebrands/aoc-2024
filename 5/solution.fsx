open System.IO

let input = fsi.CommandLineArgs[1] |> File.ReadAllText |> _.Split("\n\n")
let rules =
    input[0]
    |> _.Split("\n")
    |> Seq.map _.Split("|")
    |> Seq.map (fun x -> (int x[0], int x[1]))
    |> Seq.groupBy fst
    |> Seq.map (fun (key, group) -> key, Seq.map snd group |> Set.ofSeq)
    |> Map.ofSeq
let updates  =
    input[1]
    |> _.Split("\n")
    |> Seq.filter (fun x -> x.Length > 0)
    |> Seq.map (fun x -> x.Split(",") |> Seq.map int)

let checkUpdate (update: int seq) =
    let mid = Seq.item (Seq.length update / 2)  update
    
    let stuff =
        (update, (Set.empty<int>, true))
        ||> Seq.foldBack
            (fun page (dependencies, isValid) ->
                let newDeps =
                    match Map.tryFind page rules with
                    | None -> dependencies
                    | Some rule -> Set.union rule dependencies
                (newDeps, isValid && not (Seq.contains page newDeps)))
    (snd stuff, mid)
    
let fixUpdate (update: int seq) =
    let updateList = Seq.toList update
    let rec adjust lst =
        match lst with
        | [] -> []
        | x :: rest ->
            let fixedRest = adjust rest
            let deps =
                match Map.tryFind x rules with
                | None -> Set.empty
                | Some rule -> rule
            let (toMove, toKeep) = List.partition (fun y -> Set.contains y deps) fixedRest
            toMove @ (x :: toKeep) 
    adjust updateList
    
let checkedUpdates = updates |> Seq.map (fun updates -> (checkUpdate updates, updates))

let part1 = checkedUpdates |> Seq.map fst |> Seq.filter fst |> Seq.sumBy snd
let part2 =
    checkedUpdates
    |> Seq.filter (fun ((isValid, _), _) -> not isValid) 
    |> Seq.map (fun (_, update) ->
        let fixedUpdate = fixUpdate update
        let mid = Seq.item (Seq.length fixedUpdate / 2) fixedUpdate
        mid)
    |> Seq.sum
    
printfn "Part1: %A" part1
printfn "Part2: %A" part2