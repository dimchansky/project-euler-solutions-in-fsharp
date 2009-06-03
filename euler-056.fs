#light

open Microsoft.FSharp.Math.BigInt

let digitsSum (v:bigint) = 
    v.ToString() |> Seq.map (fun d -> uint64(d)-uint64('0')) |> Seq.fold (+) 0UL
    
let answer = 
    let limit = 99I
    {for a in 2I..limit do
        for b in 2I..limit -> (a,b)} |> Seq.map (fun (a,b) -> (pow a b) |> digitsSum) |>
        Seq.fold (fun acc v -> if v > acc then v else acc) 0UL

print_any answer        