#light

let primes N =
    let sieve = [| for i in 2u .. N -> true |]
    let numberToIndex n = n-2u
    let setFlag n value = sieve.[int(numberToIndex n)] <- value
    let getFlag n = sieve.[int(numberToIndex n)]
    let checkComposites prime = for n in {2u*prime .. prime .. N} do false |> setFlag n
    let rec tryfind_nextprime n =
        if n > N then None elif getFlag n then Some n else tryfind_nextprime (n+1u)
    let rec filterSieve i =
        match i with
        | Some(i) when i*i > N -> ()
        | Some(i)              -> checkComposites i;
                                  i+1u |> tryfind_nextprime |> filterSieve
        | None                 -> ()
    filterSieve (Some 2u);
    { 2u .. N } |> Seq.filter (fun n -> getFlag n)

let N = 2000000u

let answer =
    primes (N - 1u) |> 
    Seq.map (fun v -> bigint.FromInt64(int64(v))) |>
    Seq.fold ( + ) 0I
    
printfn "sum of primes below %d is %A" N answer