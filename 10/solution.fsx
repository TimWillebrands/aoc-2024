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
        
printfn $"Part1:\n%A{grid}"
printfn $"Zeroes: length {zeroes |> Seq.length} \n%A{zeroes}"
