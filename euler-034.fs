#light

open Microsoft.FSharp.Math.BigInt

let simpleFactorial N =
    if N < 1 then 1
    else {1 .. N} |> Seq.fold ( * ) 1

(*
d - digits count
d*9! >= 10^d - 1
d<=7
*)

let answer =
    let digits (v:int) =
        v.ToString() |> Seq.map (fun d -> int(d)-int('0'))    
    {10..(simpleFactorial 9)*7} |> Seq.filter(fun v ->
        v = (v |> digits |> Seq.map (fun d -> simpleFactorial d) |> Seq.fold (+) 0)) |>
    Seq.fold (+) 0

print_any answer        