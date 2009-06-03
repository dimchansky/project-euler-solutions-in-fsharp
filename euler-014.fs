#light

let N = 1000000

let answer =
    let sieve = [|for i in 1..N -> false|]
    let t = new System.Collections.Generic.Dictionary<bigint,int>()
    let setFlag i value = sieve.[i-1] <- value
    let getFlag i = sieve.[i-1]
    let rec findUncompleted from = 
        if from > N then None elif (getFlag from)=false then Some(from) else findUncompleted (from+1)    
    let rec getCollatzSizeCached n =
        if t.ContainsKey(n)
        then t.[n]
        else
            if n <= bigint.FromInt32 N then setFlag (bigint.ToInt32 n) true;                                        
            let res =         
                if n = 1I
                then 1
                else
                    let next = if n % 2I = 0I then (n/2I) else ((3I*n+1I)/2I)
                    1 + (getCollatzSizeCached next)
            t.Add(n, res)
            res
    let rec iterateSieve from (value, size) =
        match from with
        | Some(v)   ->
            if (100 * v % N = 0) then printfn "%d" v;
            let cseqLen = getCollatzSizeCached (bigint.FromInt32 v)
            let maxpair = if cseqLen>size then (v,cseqLen) else (value,size)
            iterateSieve (findUncompleted (v + 1)) maxpair
        | None      -> (value, size)
    iterateSieve (Some(1)) (0,0)    
    
printfn "value=%d, collatz len=%A" (fst answer) (snd answer)
