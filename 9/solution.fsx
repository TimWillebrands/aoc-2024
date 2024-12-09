open System.IO

type Block = { Id: int; Len: int; Spc: int }

let input = fsi.CommandLineArgs[1] |> File.ReadAllText |> _.ToCharArray() |> Seq.map string |> Seq.map int |> Array.ofSeq
let toBlocks (input: int array) =
    seq {
        for i = 0 to input.Length - 1 do
            if i % 2 = 0 then
                yield {
                    Id = i / 2
                    Len = input[i]
                    Spc = match Array.tryItem (i+1) input with | Some s -> s | None -> 0
               } }
    
let frag (allBlocks: Block list) =
    let rec reduce blocks acc = 
        match blocks with
        | [] -> acc
        | { Id = id; Len = len; Spc = spc } :: tail ->
            if len = 0 then 
                // If length is already zero, skip this block
                reduce tail acc
            elif spc > 0 then
                // Move blocks into free space
                let moveCount = min len spc
                let newBlock = { Id = id; Len = len - moveCount; Spc = spc - moveCount }
                // Add the updated block and continue processing
                reduce (newBlock :: tail) (acc @ [moveCount])
            else
                // No free space; just keep the block as is
                reduce tail (acc @ [len])
    reduce allBlocks []

printfn $"Simple case: %A{[|1;2;3;4;5|] |> toBlocks |> Seq.toList |> frag}"
// printfn $"Day1: %A{Seq.toList (stuff input)}"
