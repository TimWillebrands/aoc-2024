open System.IO

let len = 140
let fileName = if fsi.CommandLineArgs.Length > 1 then fsi.CommandLineArgs[1] else "4/example"
let input =
    fileName
    |> File.ReadAllText
    |> _.ReplaceLineEndings("")
    |> (fun i -> Array2D.init len len (fun y x -> i[x + y * len]))

let directions =
    [| (0,1);(0,-1);(1,0);(-1,0);(1,1);(1,-1);(-1,-1);(-1,1); |]
    
let tryGet y x =
    if x >= 0 && x < Array2D.length1 input && y >= 0 && y < Array2D.length2 input then
        Some input[y, x]
    else
        None

//            start  dir      acc
let rec check (y, x) (dx, dy) (path:string) = 
    let dirX = dx * path.Length
    let dirY = dy * path.Length
    match tryGet (y+dirY) (x+dirX) with
    | None -> false
    | Some c -> 
        let p = path + string c
        if p = "XMAS" then
            true 
        else if "XMAS".StartsWith(p) then
            check (y, x) (dx, dy) p
        else if p.Length > 4 then
            false
        else
            false

let checkPosition y x =
    directions 
    |> Seq.map (fun dir -> check (y, x) dir "")
    |> Seq.filter (fun x -> x)
    |> Seq.length

let day1 =
    Array2D.init len len checkPosition
    |> Seq.cast<int>
    |> Seq.sum
    
printfn "Input: %A" input
printfn "Day1: %A" day1
