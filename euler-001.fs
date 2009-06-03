#light
let naturalNumbersBelow limit = {1..limit-1}
let naturalMultiplesOf3Or5Below limit = naturalNumbersBelow limit |> Seq.filter(fun v -> v % 3 = 0 || v % 5 = 0)
let sumOfAllTheMultiplesOf3Or5Below limit = naturalMultiplesOf3Or5Below limit |> Seq.fold (fun a b -> a + b) 0
let sumOfAllTheMultiplesOf3Or5Below1000 = sumOfAllTheMultiplesOf3Or5Below 1000
print_any sumOfAllTheMultiplesOf3Or5Below1000;;