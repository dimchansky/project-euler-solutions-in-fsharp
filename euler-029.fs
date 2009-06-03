#light

open Microsoft.FSharp.Math.BigInt

let interval = {2I .. 100I} 

let answer = 
    interval |> Seq.map (fun a -> interval |> Seq.map (fun b -> pow a b)) |> Seq.concat |> Set.of_seq |> Set.size

print_any answer    