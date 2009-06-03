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

(*
1/x + 1/y = 1/n
(x-n) * (y-n) = n*n
X*Y=N, X=x-n, Y=y-n, N=n*n
the one-one correspondence between solutions (X, Y) and (x, y) is obvious

n=4, 
N=16 (1,2,4) - number of pairs = 3
16=1*16 X=1,Y=16 => x=5,y=20
16=2*8 X=2,Y=8 => x=6,y=12
16=4*4 X=4,Y=4 => x=8,y=8

http://www.mathpages.com/home/kmath332.htm
http://www.research.att.com/~njas/sequences/A018894
http://www.research.att.com/~njas/sequences/A126098
http://www.research.att.com/~njas/sequences/A018892
http://www.research.att.com/~njas/sequences/A048691
http://mathworld.wolfram.com/HighlyCompositeNumber.html
http://www.research.att.com/~njas/sequences/A002182
http://www.research.att.com/~njas/sequences/A002183
http://www.research.att.com/~njas/sequences/A016017
http://wwwhomes.uni-bielefeld.de/achim/julianmanuscript3.pdf
http://wwwhomes.uni-bielefeld.de/achim/highly.txt
*)
 
open Microsoft.FSharp.Math.BigInt

let answer =
    let pairLimit = 1000
    let primesSiveSize = 10000u
    let isPrime = isPrimes primesSiveSize
    let maxPrimeFactorsCount = int(System.Math.Truncate(System.Math.Log(2.0*float(pairLimit))/System.Math.Log(3.0))) + 1 
    let primeFactors = 
        primes_aux isPrime primesSiveSize |> Seq.take maxPrimeFactorsCount |> Seq.map (fun p -> uint64 p)   
    let initSolution = Seq.zip primeFactors {for i in 1..maxPrimeFactorsCount -> 1UL}
    let numberOfPairs solution = 
        ((solution |> Seq.map (fun (f,p) -> p) |> Seq.map (fun v -> v * 2UL + 1UL) |> Seq.fold ( * ) 1UL) + 1UL) / 2UL
    let value solution =
        solution |> Seq.map (fun (f,p) -> pow (bigint.FromInt64 (int64 f)) (bigint.FromInt64 (int64 p))) |> Seq.fold ( * ) 1I
    let replaceWithSmallerFactors solution =
        let sollen = solution |> Seq.length 
        let (lastPrime, lastPrimePow) = solution |> Seq.nth (sollen - 1)
        let truncatedSolution = solution |> Seq.truncate (sollen - 1)
        let factorsForSmallerValues =
            {2UL .. lastPrime-1UL} |> Seq.map (fun smallerVal -> primeFactors_aux smallerVal isPrime |> Seq.map (fun (f, p) -> (f, uint64 p)))
        let multiplyTruncatedSolution fp =
            truncatedSolution |> Seq.map (fun (sf, sp) -> 
                let addpow = fp |> Seq.first (fun (f,p) -> if f=sf then Some(p) else None)
                match addpow with
                | Some(p) -> (sf, sp+p)
                | _ -> (sf, sp))
        let powersOrderedDesc fp =
            let fplen = fp |> Seq.length
            let ((lastf,lastp), rezorder) = 
                Seq.zip fp {for i in 1..fplen -> true} |> 
                Seq.fold1 (fun ((f1,p1), order1) ((f2,p2),order2) -> ((f2,p2),order1 && p1>=p2))
            rezorder
        let newSolutions = 
            factorsForSmallerValues |> Seq.map (fun s -> s |> multiplyTruncatedSolution) |>
            Seq.filter (fun s -> s |> powersOrderedDesc) |>
            Seq.filter (fun s -> (s |> numberOfPairs) > uint64 pairLimit)
        if Seq.nonempty newSolutions
        then       
            let (bestSolutionValue, bestSolution) = 
                newSolutions |> Seq.map (fun s -> (s |> value, s)) |>
                Seq.fold1 (fun (v1,s1) (v2,s2) -> if v1<v2 then (v1,s1) else (v2,s2))            
            Some(bestSolution)
        else None
    let rec findSmallerSolution solution =
        match solution |> replaceWithSmallerFactors with
        | Some(newsol) -> findSmallerSolution newsol
        | _ -> solution       
    let bestSol = initSolution |> findSmallerSolution
    (bestSol |> value, bestSol |> numberOfPairs, bestSol |> Seq.to_list)
    
print_any answer    