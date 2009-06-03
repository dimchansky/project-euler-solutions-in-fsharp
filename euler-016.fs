#light

open Microsoft.FSharp.Math.BigInt 

let answer =
    (pow 2I 1000I).ToString() |> Seq.map (fun d -> int(d)-int('0')) |> Seq.fold (+) 0
    
print_any answer