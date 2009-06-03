#light

open Microsoft.FSharp.Math.BigInt

let digits = 10

let answer = 
    let s = ({1I..1000I} |> Seq.map (fun v -> pow v v) |> Seq.fold ( + ) 0I).ToString()
    let sl = s.Length
    if sl > digits
    then String.sub s (sl-digits) digits
    else s

print_any answer    