#light

let isPrimes N =
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
    (fun n -> getFlag n)
    
let primes_aux isPrime = 
    fun N -> {2u .. N} |> Seq.filter (fun n -> isPrime n)    
    
let primes N =
    primes_aux (isPrimes N) N

let rec isqrt_aux n g0 =
    if n <= 0UL
    then 0UL
    else
        let g1 = (n / g0 + g0) / 2UL
        if g0 > g1
        then isqrt_aux n g1
        else g0
let isqrt n = isqrt_aux n n

let primeFactors_aux N isPrime = 
    let isqrtN = isqrt N    
    let firstHalfPrimeFactors = (uint32(isqrtN)) |> primes_aux isPrime |> Seq.map (fun v -> uint64(v)) |> Seq.filter (fun x -> N % x = 0UL)   
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

let divisors_aux N isPrime = 
    if N = 1UL
    then seq [1UL]
    else
        primeFactors_aux N isPrime |>
        Seq.map (fun (p,n) -> Seq.unfold (fun (prod,i) -> if i<=n then Some(prod,(prod*p,i+1)) else None) (1UL,0)) |>
        Seq.fold1 (fun s1 s2 -> 
            {for i in s2 do
             for j in s1 do
             yield i*j}) 
         
let properDivisors_aux N isPrime = divisors_aux N isPrime |> Seq.filter(fun d -> d<N)

let sumOfDivisors_aux N isPrime = properDivisors_aux N isPrime |> Seq.fold (+) 0UL

let numbersLimit = 28123UL
let numbersToCheck = {1UL .. numbersLimit}

let answer =
    let isPrime = isPrimes (uint32(numbersLimit))
    let abundantNumbers = numbersToCheck |> Seq.filter (fun n -> (sumOfDivisors_aux n isPrime) > n) |> Seq.to_list
    let sumOfAbundantNumbers = 
        {for a1 in abundantNumbers do
         for a2 in abundantNumbers do
         yield a1 + a2}
    let sieve = new System.Collections.BitArray(int(numbersLimit))
    sieve.SetAll(true);    
    sumOfAbundantNumbers |> Seq.iter (fun soan -> if soan <= numbersLimit then sieve.Set(int(soan)-1, false));
    numbersToCheck |> Seq.filter (fun v -> sieve.Get(int(v)-1)) |>
    Seq.fold ( + ) 0UL
    
print_any answer

