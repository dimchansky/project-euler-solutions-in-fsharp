#light

open System.Collections

let isPrimes N =
    let sieve = new BitArray(int(N-1u))
    sieve.SetAll(true); 
    let numberToIndex n = n-2u
    let setFlag n value = sieve.Set(int(numberToIndex n), value)
    let getFlag n = sieve.Get(int(numberToIndex n))
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

let factorial N =
    if N < 1 then 1I
    else {1I .. bigint.FromInt32(N)} |> Seq.fold ( * ) 1I

let getLexicographicPermutationByIndex permutation index =    
    let rec getLexicographicPermutationByIndex_aux permEls i acc =
        match permEls with
        | [] -> acc
        | p  ->
            let N = p |> List.length
            let from1_NToPermElement n = {1..N} |> Seq.zip (p |> Seq.orderBy (fun a -> a)) |> Seq.find_index (fun v -> v=n)
            let facN_1 = factorial (N-1)
            let elOfPerm = bigint.ToInt32(bigint.FromInt32(i) / facN_1 + 1I) |> from1_NToPermElement
            let newPermEls = p |> List.filter (fun v -> v <> elOfPerm)
            let newi = bigint.ToInt32(bigint.FromInt32(i) % facN_1) 
            let newacc = acc @ [elOfPerm]
            getLexicographicPermutationByIndex_aux newPermEls newi newacc
    getLexicographicPermutationByIndex_aux permutation index []

let allDigits (v:int) = v.ToString() |> Seq.map (fun d -> int(d)-int('0'))
let digitsListToNumber l =
    let (sum, pow10) = List.fold_right (fun iN (sum, pow10) -> (sum+iN*pow10,pow10*10)) l (0,1)
    sum

//Note: Nine numbers cannot be done (1+2+3+4+5+6+7+8+9=45 => always dividable by 3)
//Note: Eight numbers cannot be done (1+2+3+4+5+6+7+8=36 => always dividable by 3)

let maxDigits = 7

let answer =
    let d = {maxDigits .. -1 .. 1} 
    let isPrime = isPrimes (uint32(d |> Seq.to_list |> digitsListToNumber ))
    d |> Seq.map (fun dlen -> 
        let total = bigint.ToInt32(factorial dlen)
        {total-1 .. -1 .. 0} |> Seq.map (fun index -> 
            getLexicographicPermutationByIndex ({1..dlen} |> Seq.to_list) index)
        ) |> Seq.concat |> Seq.map (fun s -> digitsListToNumber s) |> 
        Seq.first (fun p -> if isPrime (uint32(p)) then Some(p) else None)
       
print_any answer
