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

let answer =
    let limit = 999999u
    let isPrime = isPrimes limit
    let allRotations (num:uint32) =
        let snum = num.ToString()
        Seq.unfold (fun (s:string) ->
            let nexts = 
                if s.Length>1 then s.Substring(1) + s.Substring(0, 1) else s
            Some(s, nexts)) snum |> Seq.take snum.Length |> Seq.map (fun s -> System.UInt32.Parse(s))
    {2u..limit} |> Seq.filter (fun v -> isPrime v) |> 
    Seq.filter (fun p -> p |> allRotations |> Seq.for_all (fun r -> isPrime r)) |>
    Seq.length
    
print_any answer    