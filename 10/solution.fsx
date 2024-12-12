open System
open System.Diagnostics
open System.IO

let input =
    fsi.CommandLineArgs[1]
    |> File.ReadLines
    |> Seq.map (fun x -> x.ToCharArray() |> Seq.map string |> Seq.map int |> Seq.toArray)
    |> Array.ofSeq

let len = Seq.length input
let grid = Array2D.init len len (fun x y -> input[x][y])

let zeroes =
    input
    |> Seq.collect id
    |> Seq.mapi (fun i height -> (height, i))
    |> Seq.fold
        (fun list (height, i) -> if height = 0 then ((i%len,i/len) :: list) else list)
        List.Empty

let tryGet  (x, y) =
    if x >= 0 && x < Array2D.length1 grid && y >= 0 && y < Array2D.length2 grid then
        Some grid[y, x]
    else
        None

let findNeighbours (x, y) =
    [(1,0); (-1,0); (0,1); (0,-1)]
    |> Seq.map (fun (dx,dy) -> (x+dx, y+dy)) 
    |> Seq.map (fun pos -> (pos, tryGet pos))
    |> Seq.choose (fun (pos, v) -> 
    match v with
    | Some value -> Some (pos, value)
    | None -> None)
    
let findEnds trailHead  =
    let rec findNext position nextValue trailEnds =
        if nextValue > 9 then
            trailEnds @ [position]
        else 
            let goodNeighbours =
                position
                |> findNeighbours
                |> Seq.filter (fun (pos, height) -> height = nextValue)
                |> Seq.toList
            match goodNeighbours with
            | [] -> trailEnds
            | neighbours ->
                neighbours
                |> Seq.map (fun (pos, height) -> findNext pos (height+1) trailEnds)
                |> Seq.concat
                |> Seq.toList
    findNext trailHead 1 List.empty

let uniqueEnds = findEnds >> Set.ofList >> Set.count 
let allEnds = findEnds >> List.length
let part1 = zeroes |> Seq.map uniqueEnds |> Seq.sum
let part2 = zeroes |> Seq.map allEnds |> Seq.sum

printfn $"Part1:%A{part1}"
printfn $"Part2:%A{part2}"
