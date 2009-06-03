#light

let rec extractPeriodic_aux a b frac =  
    let digit = if a<b then 0 else a/b
    if (digit, a) = (0,0)
    then (0, frac)
    else
        let idx = try frac |> List.find_index (fun (digit, numerator) -> a=numerator) with _ -> -1
        if idx > 0
        then ((frac |> List.length) - idx, frac)
        else extractPeriodic_aux (10*(a%b)) b (frac @ [(digit, a)])

let extractPeriodic a b = extractPeriodic_aux a b []

let answer =
    let (d, cycleLen) =
        {2 .. 999} |>
        Seq.map (fun d -> 
            let (cycleLen, fraction) = extractPeriodic 1 d
            (d, cycleLen)) |> 
        Seq.max_by (fun (d, cycleLen) -> cycleLen)
    d
    
print_any answer