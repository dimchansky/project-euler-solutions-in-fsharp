#light

open Microsoft.FSharp.Math.BigInt

let powerValue = 5I

let answer =
    let maxValue d = (pow 10I d) - 1I
    let maxDigits = 
        {for d in 1I..10I -> (d, maxValue d, d * pow 9I powerValue)} |> 
        Seq.first (fun (d, value, sum) -> 
            if value > sum then Some(d) else None) |> Option.get
    {10I .. maxValue maxDigits} |> Seq.filter (fun v -> 
        v = (v.ToString() |> 
             Seq.map (fun d -> int(d)-int('0')) |> 
             Seq.map (fun d -> pow (bigint.FromInt32(d)) powerValue) |> 
             Seq.fold (+) 0I)) |>
    Seq.fold (+) 0I
    
print_any answer