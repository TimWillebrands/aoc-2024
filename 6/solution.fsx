open System.IO

let input =
    fsi.CommandLineArgs[1]
    |> File.ReadLines
    |> Seq.map _.ToCharArray()
    |> Array.ofSeq

let len = Seq.length input
let grid = Array2D.init len len (fun x y -> input[x][y])
let guard =
    grid
    |> Seq.cast<char>
    |> Seq.findIndex (fun cell -> cell = '^')
    |> (fun i -> (i % len, (i / len)))
let dirs =   [|(0,-1); (1,0); (0,1); (-1,0)|]

let tryGet y x =
    if x >= 0 && x < Array2D.length1 grid && y >= 0 && y < Array2D.length2 grid then
        Some grid[y, x]
    else
        None

let rec move dir (gx, gy) steps = 
    let dx, dy = dirs[dir % 4]
    let target = (dx + gx, dy + gy)
    match tryGet (snd target) (fst target) with
    | None -> steps
    | Some '#' -> move (dir + 1) (gx, gy) steps
    | _ -> move dir target (Set.add target steps)
    

let day1 = move 0 guard (Set.singleton guard)

printfn "Day1: %A" (day1 |> Set.count) // + 1?