open System
open System.Diagnostics
open System.IO

let input =
    fsi.CommandLineArgs[1]
    |> File.ReadLines
    |> Seq.map _.ToCharArray()
    |> Array.ofSeq

let len = Seq.length input
let grid = Array2D.init len len (fun x y -> input[x][y])
let antennas =
    input
    |> Seq.collect id
    |> Seq.mapi (fun i freq -> (freq, i))
    |> Seq.fold
        (fun map (freq, i) ->
            if freq <> '.' then
                match Map.tryFind freq map with
                | Some positions -> Map.add freq ((i%len,i/len) :: positions) map 
                | None -> Map.add freq [(i%len,i/len)] map 
            else
                map)
        Map.empty<char, (int*int) list>
        
let tryGet y x =
    if x >= 0 && x < Array2D.length1 grid && y >= 0 && y < Array2D.length2 grid then
        Some grid[y, x]
    else
        None

let partOneMethod ((x1, y1):int*int) ((x2, y2):int*int) =
    let dx, dy = x2 - x1, y2 - y1
    Set.ofList [ (x1 - dx, y1 - dy); (x2 + dx, y2 + dy) ]

let partTwoMethod ((x1, y1):int*int) ((x2, y2):int*int) =
    let dx, dy = x2 - x1, y2 - y1
    let rec add x y dx dy (points: (int*int) list) =
        match tryGet x y with
        | None -> points
        | Some _ -> add (x+dx) (y+dy) dx dy ( ((x+dx), (y+dy)) :: points ) 
    let dir1 = add x1 y1 dx dy List.empty<int*int>
    let dir2 = add x1 y1 -dx -dy List.empty<int*int>
    Set.ofList (dir1 @ dir2)

let foldAntennas plotFn  agg freq positions =
    let stuff =
        positions
        |> Seq.map (fun x -> x, List.filter ((<>) x) positions)
        |> Seq.map (fun (antenna, others) ->
            others
            |> Seq.map (plotFn antenna)
            |> Seq.reduce Set.union)
       |> Seq.reduce Set.union
    Set.union agg stuff

let part1 =
    antennas
    |> Map.fold (foldAntennas partOneMethod) Set.empty<int*int> 
    |> Seq.filter (fun (x, y) -> (tryGet x y) <> None)
    |> Seq.length

let part2 =
    antennas
    |> Map.fold (foldAntennas partTwoMethod) Set.empty<int*int> 
    |> Seq.filter (fun (x, y) -> (tryGet x y) <> None)
    |> Seq.length

printfn $"Day1: %A{part1}"
printfn $"Day2: %A{part2}"
