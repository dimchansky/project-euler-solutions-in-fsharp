#light

let answer =
    {1901 .. 2000} |> 
    Seq.map (fun y -> {1 .. 12} |> Seq.map (fun m -> (y,m,1))) |> 
    Seq.concat |> 
    Seq.map (fun (y,m,d) -> new System.DateTime(y,m,d)) |> 
    Seq.map (fun dt -> dt.DayOfWeek) |>
    Seq.filter (fun dow -> dow = System.DayOfWeek.Sunday) |> 
    Seq.length

print_any answer