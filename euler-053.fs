#light

open Microsoft.FSharp.Math.BigInt

let factorial N =
    if N < 1 then 1I
    else {1I .. bigint.FromInt32(N)} |> Seq.fold ( * ) 1I

let C n r = factorial n/(factorial r * factorial (n-r))
    
let answer = 
    {for n in 1..100 do
        for r in 1..n -> (n,r)} |> 
    Seq.map (fun (n,r) -> C n r) |> 
    Seq.filter (fun v -> v > 1000000I) |>
    Seq.length

print_any answer        