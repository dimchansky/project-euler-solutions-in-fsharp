#light

let answer = 
    new System.String(System.IO.File.ReadAllText("names.txt") |> Seq.filter (fun c -> c<>'"') |> Seq.to_array) |>
    String.split([',']) |> List.to_seq |> Seq.orderBy (fun s -> s) |>
    Seq.mapi (fun idx s ->
        let wordScore = s |> Seq.map (fun c -> int(c)-int('A')+1) |> Seq.fold (+) 0
        (idx+1)*wordScore) |>
    Seq.fold (+) 0

print_any answer    