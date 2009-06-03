#light
let fibsBelow limit = (1I,2I) |> Seq.unfold (fun (n0, n1) -> if n0 < limit then Some(n0, (n1, n0 + n1)) else None)
let evenValuedFibsBelow limit = fibs limit |> Seq.filter (fun v -> v % 2I = 0I)
let sumOfEvenValuedFibsBelow limit = evenValuedFibs limit |> Seq.fold (fun a b -> a + b) 0I
let sumOfEvenValuedFibsBelow4000000 = sumOfEvenValuedFibs 4000000I
print_any sumOfEvenValuedFibsBelow4000000