#light

let answer =
    Seq.unfold (fun (f1, f2, term) -> Some((term, f2), (f2, f1+f2, term + 1))) (0I, 1I, 1) |>
    Seq.first (fun (term, v) -> if v.ToString().Length = 1000 then Some(term) else None)
    
print_any answer