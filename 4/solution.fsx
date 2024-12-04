open System.IO

let fileName = if fsi.CommandLineArgs.Length > 1 then fsi.CommandLineArgs[1] else "4/example"
let len = if fsi.CommandLineArgs.Length > 2 then int fsi.CommandLineArgs[2] else 140
let input =
    fileName
    |> File.ReadAllText
    |> _.ReplaceLineEndings("")
    |> (fun i -> Array2D.init len len (fun y x -> i[x + y * len]))

let diagonals = [| (1,1);(1,-1);(-1,-1);(-1,1); |]
let directions =
    Array.concat [ [| (0,1);(0,-1);(1,0);(-1,0); |] ; diagonals ]
    
let tryGet y x =
    if x >= 0 && x < Array2D.length1 input && y >= 0 && y < Array2D.length2 input then
        Some input[y, x]
    else
        None

//            start  dir      acc
let rec xmas (y, x) (dx, dy) (path:string) = 
    let dirX = dx * path.Length
    let dirY = dy * path.Length
    match tryGet (y+dirY) (x+dirX) with
    | None -> false
    | Some c -> 
        let p = path + string c
        if p = "XMAS" then
            true 
        else if "XMAS".StartsWith(p) then
            xmas (y, x) (dx, dy) p
        else if p.Length > 4 then
            false
        else
            false

let day1 =
    let check y x = 
        directions 
        |> Seq.map (fun dir -> xmas (y, x) dir "")
        |> Seq.filter (fun x -> x)
        |> Seq.length
    Array2D.init len len check
    |> Seq.cast<int>
    |> Seq.sum

let day2 =
    let mas y x =
        let mid = tryGet y x
        let ring = 
            diagonals
            |> Seq.map (fun (dx, dy) -> tryGet (y+dy) (x+dx))
            |> Seq.choose id
            |> Seq.map string
            |> String.concat ""
        match ring with 
        | "SSMM" | "SMMS" | "MMSS" | "MSSM" when mid = Some 'A' -> true
        | _ -> false
    Array2D.init len len mas
    |> Seq.cast<bool>
    |> Seq.filter (fun x -> x)
    |> Seq.length

// printfn "Input: %A" input
printfn "Day1: %A" day1
printfn "Day2: %A" day2
