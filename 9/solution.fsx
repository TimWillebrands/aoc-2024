open System.IO
open System.Text

type Result = { Chksm: int64; Len: int; Spc: int }
type Block = { Id: int; Len: int; Spc: int }

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

let frag (allBlocks: Block list) =
    let move (block: Block) len amount =
        let rec mv cnt checksumAdd =
            if cnt < amount then
                let i = len + cnt
                let add = (checksumAdd + (int64 i * int64 block.Id))
                mv (cnt + 1) add
            else checksumAdd
        mv 0 0

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
                // There is some juice left in this box so let's move that too
                else
                    let updatedTailBlock = { tailBlock with Len = tailBlock.Len - amount }
                    reduce (restOfBlocks @ [ updatedTailBlock ]) newAcc            
            
    reduce allBlocks { Chksm = 0; Len = 0; Spc = 0}
    
let optimise (originalBlocks: Block array) =
    let printBlocks blocks =
        let sb = StringBuilder()
        for block in blocks do
            for _ in 1 .. block.Len do sb.Append(block.Id % 10) |> ignore
            for _ in 1 .. block.Spc do sb.Append('.') |> ignore
        printfn $"Blocks:\t{sb.ToString()}"
        
    let findSpace (blocks: Block seq) id =
        let len = originalBlocks[id].Len
        let fitIndex = blocks |> Seq.tryFindIndex (fun block -> block.Spc >= len)
        let movedIndex = blocks |> Seq.findIndexBack (fun b -> b.Id = id)
        match fitIndex with | Some fit -> Some (fit, movedIndex) | None -> None
        
    let update (fit:Block) (block:Block) =
        ({fit with Spc = 0}, {block with Spc = fit.Spc - block.Len})
        
    let rec move id (blocks: Block list) =
        if id = 0 then
            blocks
        else
            match findSpace blocks id with
            | Some (fitIndex, movedIndex) when fitIndex < movedIndex ->
                // printBlocks blocks
                let fit = blocks[fitIndex]
                let moved = blocks[movedIndex]
                // printfn $"Moved: {id}/{moved} -> {fit}\n----------"
                let f,b = (update fit originalBlocks[id])
                
                let preFit = blocks[0..(fitIndex-1)]
                let postFitAndInsert = blocks[(fitIndex+1)..(movedIndex-2)]
                let preMoved = blocks[movedIndex-1]
                let postMoved = blocks[(movedIndex+1)..]
               
                preFit @ [f;b] @ postFitAndInsert @ [{preMoved with Spc = preMoved.Spc + moved.Len + moved.Spc}] @ postMoved
                |> move (id - 1)
            | _ ->
                // printfn $"Skip: {id}/ {originalBlocks[id]}\n----------"
                move (id - 1) blocks
                
    move (originalBlocks |> Seq.last |> _.Id) (originalBlocks |> List.ofArray)
    
let checksum (blocks: Block seq) =
    blocks
    |> Seq.fold
        (fun (chksm, i) block ->
            let mutable localChck = 0L
            for index in i .. (i+block.Len-1) do
                localChck <- localChck + (int64 index) * (int64 block.Id)
            (chksm + localChck, i + block.Len + block.Spc)
        )
        (0L,0)
    
let part1 = toBlocks >> Seq.toList >> frag >> _.Chksm
let part2 = toBlocks >> Seq.toArray >> optimise >> checksum >> fst

printfn $"Part-1: %A{part1 input}"
printfn $"Part-2: %A{part2 input}"