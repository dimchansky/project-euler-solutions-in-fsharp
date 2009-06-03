#light

let triangle =
    System.IO.File.ReadAllText("triangle.txt") |>
    String.split(['\n']) |> 
    List.map (fun sl -> sl |> String.split([' ']) |> List.map (fun sv -> System.Int32.Parse(sv))) 

let rec selectMaxesFromPairs_aux l acc = 
    match l with
    | h0 :: h1 :: tail -> 
        let max = if h0 > h1 then h0 else h1
        selectMaxesFromPairs_aux (h1::tail) (acc @ [max])
    | _ ->
        acc
        
let selectMaxesFromPairs l = selectMaxesFromPairs_aux l []

let answer = 
    triangle |> List.reduce_right (fun l0 l1 ->
        l1 |> selectMaxesFromPairs |> List.map2 (fun v0 v1 -> v0 + v1) l0)
        
print_any answer