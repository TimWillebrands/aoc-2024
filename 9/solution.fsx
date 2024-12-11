open System.IO

type Block = { Id: int; Len: int; Spc: int }
type Result = { Chksm: int64; Len: int; Spc: int }

let input =
    fsi.CommandLineArgs[1]
    |> File.ReadAllText
    |> _.ToCharArray()
    |> Seq.map string
    |> Seq.map int
    |> Array.ofSeq
let toBlocks (input: int array) =
    seq {
        for i = 0 to input.Length - 1 do
            if i % 2 = 0 then
                yield {
                    Id = i / 2
                    Len = input[i]
                    Spc = match Array.tryItem (i+1) input with | Some s -> s | None -> 0
               } }

let move (block: Block) len amount =
    let rec mv cnt checksumAdd =
        if cnt < amount then
            let i = len + cnt
            let add = (checksumAdd + (int64 i * int64 block.Id))
            mv (cnt + 1) add
        else checksumAdd
    mv 0 0

let defrag (allBlocks: Block list) =
    let rec reduce blocks (acc: Result) =
        match blocks with
        | [] -> acc 
        | _ ->
            // If there is no space left we 'move' the whole first block
            if acc.Spc = 0 then
                let headBlock :: tail = blocks
                let chksm = int64 <| move headBlock acc.Len headBlock.Len
                let newAcc = { Chksm = acc.Chksm + chksm
                               Len = acc.Len + headBlock.Len
                               Spc = headBlock.Spc }
                reduce tail newAcc
            // If there is space we move (part of) the last block to the empty space
            else
                let tailBlock = (List.last blocks)
                let amount = min acc.Spc tailBlock.Len
                let chksm = int64 <| move tailBlock acc.Len amount
                let newAcc = { Chksm = acc.Chksm + chksm
                               Len = acc.Len + amount
                               Spc = acc.Spc - amount }
                let restOfBlocks = (List.except [tailBlock] blocks)
                
                // We've moved the whole block so drop it like its hot
                if tailBlock.Len = amount then
                    reduce restOfBlocks newAcc
                // There is some juice left in this box so lets move that too
                else
                    let updatedTailBlock = { tailBlock with Len = tailBlock.Len - amount }
                    reduce (restOfBlocks @ [ updatedTailBlock ]) newAcc            
            
    reduce allBlocks { Chksm = 0; Len = 0; Spc = 0}

let part1 = input |> toBlocks |> Seq.toList |> defrag

printfn $"Part-1: %A{part1}" 