#light

let digitsNs = seq [1; 10; 100; 1000; 10000; 100000; 1000000]
let maxDigitsPos = digitsNs |> Seq.fold1 (fun v0 v1 -> if v1 > v0 then v1 else v0)

let answer = 
    let fraction = 
        Seq.unfold (fun (v:int, len) -> 
            if len >= maxDigitsPos
            then None
            else 
                let s = v.ToString() |> Seq.map (fun d -> int(d)-int('0'))
                let slen = s |> Seq.length
                Some (s, (v+1, len+slen))) (1,0) |>
        Seq.concat
    digitsNs |> Seq.map (fun idx -> fraction |> Seq.nth (idx - 1)) |> Seq.fold ( * ) 1
    
print_any answer       
