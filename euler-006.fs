#light 

let sumOfSquares numbers = numbers |> Seq.map (fun v -> v * v) |> Seq.fold (+) 0I

let squareofSum numbers =
    let v = numbers |> Seq.fold (+) 0I
    v * v

let difference numbers = (numbers |> squareofSum) - (numbers |> sumOfSquares)

print_any (difference {1I..100I})