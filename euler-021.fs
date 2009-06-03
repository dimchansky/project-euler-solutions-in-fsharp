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
    
let primes N =
    let isPrime = isPrimes N
    {2u .. N} |> Seq.filter (fun n -> isPrime n)

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

let divisors N = 
    if N = 1UL
    then seq [1UL]
    else
        primeFactors N |>
        Seq.map (fun (p,n) -> Seq.unfold (fun (prod,i) -> if i<=n then Some(prod,(prod*p,i+1)) else None) (1UL,0)) |>
        Seq.fold1 (fun s1 s2 -> 
            {for i in s2 do
             for j in s1 do
             yield i*j}) 
         
let properDivisors N = divisors N |> Seq.filter(fun d -> d<N)
    
let d N = properDivisors N |> Seq.fold (+) 0UL

let N = 10000
let answer =
    let sieve = [| for i in 2 .. N -> false |]
    let rez = new System.Collections.Generic.List<int * int>()
    let setSafeFlag i value = if i>=2 && i<=N then sieve.[i-2] <- value
    let getFlag i = sieve.[i-2]
    let rec findUncompleted from = 
        if from > N then None elif (getFlag from)=false then Some(from) else findUncompleted (from+1)        
    let rec iterateSieve from =
        match from with
        | Some(a)   ->
            if (getFlag a)=false then
                let b = d (uint64(a))
                let c = d b
                setSafeFlag a true; setSafeFlag (int(b)) true;
                if c=uint64(a) && b<>uint64(a) then rez.Add((a,int(b)));
            iterateSieve (findUncompleted (a + 1))
        | None      -> ()
    iterateSieve (Some(2))
    rez |> Seq.map (fun (a,b) -> a+b) |> Seq.fold (+) 0
    
print_any answer