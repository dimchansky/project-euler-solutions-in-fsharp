#light

let answer =
    let digits (v:int) =
        v.ToString() |> Seq.map (fun d -> uint64(d)-uint64('0')) |> Seq.orderBy (fun v -> v)
    let sameDigits d1 d2 =
        if (Seq.length d1) = (Seq.length d2)
        then Seq.zip d1 d2 |> Seq.for_all (fun (d1, d2) -> d1=d2)
        else false
    Seq.unfold (fun b -> Some(b, b+1)) 1 |>
    Seq.first (fun x -> 
        let x2d = digits (x*2)
        let x3d = digits (x*3)
        let x4d = digits (x*4)
        let x5d = digits (x*5)
        let x6d = digits (x*6)
        let rez =
            (sameDigits x2d x3d) &&
            (sameDigits x3d x4d) && 
            (sameDigits x4d x5d) && 
            (sameDigits x5d x6d)
        if rez then Some(x) else None)
        
print_any answer        