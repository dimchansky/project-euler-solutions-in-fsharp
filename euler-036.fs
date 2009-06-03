#light

let isSymmetric str = 
    let rec isSymmetric_aux (str:string) s e =
        if e - s > 0
        then
            if (str.[s] <> str.[e])
            then false
            else isSymmetric_aux str (s+1) (e-1)
        else true   
    isSymmetric_aux str 0 ((String.length str) - 1)    

let answer =
    {1..999999} |> 
    Seq.filter (fun v -> (System.Convert.ToString(v,10) |> isSymmetric) && (System.Convert.ToString(v,2) |> isSymmetric)) |>
    Seq.fold (+) 0

print_any answer