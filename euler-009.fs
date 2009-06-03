#light

let sum = 1000

let ms = {1 .. sum/2} |> Seq.filter (fun v -> sum % (2*v) = 0)
let ns = ms |> Seq.map (fun v -> sum / (2*v) - v) |> Seq.filter (fun v -> v > 0)

let triple = 
    {for m in ms do
     for n in ns do
     yield (2*m*n, (m*m-n*n), (m*m+n*n))} |>
    Seq.filter (fun (a,b,c) -> b > 0) |>
    Seq.first (fun (a,b,c) -> if a + b + c = sum then Some((a,b,c)) else None)

let answer = 
    match triple with
    | Some(a,b,c)   -> printfn "%d * %d * %d = %d" a b c (a*b*c)
    | _             -> printfn "not found"
