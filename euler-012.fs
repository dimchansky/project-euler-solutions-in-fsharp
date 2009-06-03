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

let primeFactors N = 
    let rec isqrt_aux n g0 =
        if n <= 0UL
        then 0UL
        else
            let g1 = (n / g0 + g0) / 2UL
            if g0 > g1
            then isqrt_aux n g1
            else g0
    let isqrt n = isqrt_aux n n
    let isqrtN = isqrt N    
    let firstHalfPrimeFactors = primes (uint32(isqrtN)) |> Seq.map (fun v -> uint64(v)) |> Seq.filter (fun x -> N % x = 0UL)   
    let rec divsCount_aux N p rslt =
        if p = 1UL
        then (rslt + 1, N)
        else
            if N % p = 0UL
            then divsCount_aux (N/p) p (rslt + 1)
            else (rslt, N)
    let divsCount N p = divsCount_aux N p 0
    let (NRem, pPows) = firstHalfPrimeFactors |> Seq.fold (fun (n,pows) p ->
        let (count, divRem) = divsCount n p
        (divRem, pows @ [count])) (N,[])
    let result = Seq.zip firstHalfPrimeFactors (pPows |> List.to_seq)
    if NRem > isqrtN
        then Seq.append result (seq [(NRem, 1)])
        else result
    
let answer =
    Seq.unfold (fun b -> Some(b, b+1UL)) 1UL |>
    Seq.map (fun v -> v * (v + 1UL) / 2UL) |>
    Seq.map (fun v ->
        let factors = v |> primeFactors |> Seq.fold (fun prod (p,cnt) -> prod * (cnt + 1)) 1
        (v, factors)) |>
    Seq.first (fun (v,factors) -> if factors > 500 then Some((v,factors)) else None)   
    
printfn "the first triangle number to have over five hundred divisors: %A (value, divisors)" answer