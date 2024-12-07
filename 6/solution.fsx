open System.Diagnostics
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

let tryGet y x obstruction =
    if obstruction = (x, y) then
        Some '#'
    elif x >= 0 && x < Array2D.length1 grid && y >= 0 && y < Array2D.length2 grid then
        Some grid[y, x]
    else
        None

let rec move obstruction (gx, gy, dir) steps =
    let dx, dy = dirs[dir]
    let target = (dx + gx, dy + gy, dir)
    let tx, ty, _ = target
    match tryGet ty tx obstruction with
    | None -> Some steps
    | Some '#' -> move obstruction (gx, gy, (dir + 1)  % 4) steps
    | _ ->
        if Set.contains target steps then
            None // loop detected!!
        else 
            move obstruction target (Set.add target steps)
            
let start = (fst guard, snd guard, 0)

let findLoopPositions (originalPath: Set<int*int*int>) =
    originalPath 
    |> Set.toArray 
    |> Array.Parallel.choose (fun (x, y, _) ->
        if grid[y,x] = '.' then
            match move (x, y) start (Set.singleton start) with
            | None -> Some (x,y)
            | Some _ -> None
        else
            None )
    |> Set.ofArray
    
let sw = Stopwatch.StartNew()
let originalPath =
    match move (-1, -1) start (Set.singleton start) with
    | Some path -> path
    | None -> Set.empty
let day1 = originalPath |> Set.map (fun (x,y,_) -> (x,y)) |> Set.count
let day1ms = sw.ElapsedMilliseconds

sw.Restart()
let day2 = findLoopPositions originalPath |> Set.count
let day2ms = sw.ElapsedMilliseconds


printfn "Day1: %A, calc in %i" day1 day1ms
printfn "Day2: %A, calc in %i" day2 day2ms