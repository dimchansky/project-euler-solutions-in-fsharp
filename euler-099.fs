#light

let pairs =
    System.IO.File.ReadAllText("base_exp.txt") |>
    String.split(['\n']) |> 
    List.map (fun sl -> sl |> String.split([',']) |> List.map (fun sv -> System.Int32.Parse(sv))) |>
    List.mapi (fun idx v ->
        match v with
        | base::exp::[] -> (idx, base,exp)
        | _ -> failwith "bad file")

let answer =
    (
    pairs |> List.map (fun (idx, base, exp) -> (idx, float(exp) * System.Math.Log(float(base)))) |>
    List.fold_left (fun (fidx, fv) (idx, v) -> if v>fv then (idx, v) else (fidx, fv)) (-1, 0.0) |> fst
    ) + 1
    
print_any answer    