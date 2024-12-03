open System.IO
open System.Text.RegularExpressions

let inputFile = fsi.CommandLineArgs[1]
let input = File.ReadAllText(inputFile)

let (|Prefix|_|) (p:string) (s:string) =
    if s.StartsWith(p) then
        Some(s.Substring(p.Length))
    else
        None

type Exp = 
    | Do 
    | Dont 
    | Mul of int * int

let found =
    seq {
        for m in Regex.Matches(input, "(?i)mul\((\d+,\d+)?\)|do\(\)|don't\(\)") do
            match m.Value with
            | Prefix "mul" rest ->
                yield Regex.Matches(rest, "\d+") 
                |> Seq.cast<System.Text.RegularExpressions.Match>
                |> Seq.map (fun x -> x.Value |> int)
                |> Seq.toList
                |> (fun s -> Mul(s[0],s[1]))
            | Prefix "don" _ -> yield Dont
            | Prefix "do" _ -> yield Do
            | _ -> ()
    }

let day1 input =
    input |> Seq.map (fun exp -> 
        match exp with | Mul (s1,s2) -> s1*s2 | _ -> 0) 

let day2 =
    found 
        |> Seq.fold (fun (isBlocked, acc) exp -> 
            match exp with 
            | Mul _ -> 
                if isBlocked then
                    (isBlocked, acc)
                else
                    (isBlocked, Seq.append acc [exp])
            | Do -> (false, acc)
            | Dont -> (true, acc)
            ) 
            (false, [])

let (_, d2) = day2

printfn "Day1: %A" (day1, day1 found |> Seq.sum)
printfn "Day2: %A" (day2, day1 d2 |> Seq.sum)
