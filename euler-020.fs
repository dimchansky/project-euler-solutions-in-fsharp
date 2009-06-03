#light

let answer = 
    ({1I..100I} |> Seq.fold ( * ) 1I).ToString() |> Seq.map (fun d -> int(d)-int('0')) |> Seq.fold (+) 0

print_any answer