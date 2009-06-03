#light

let answer = 
    let spiral =
        let changeDirection dir = 
            match dir with
            | ( 1,  0) -> ( 0, -1)
            | ( 0, -1) -> (-1,  0)
            | (-1,  0) -> ( 0,  1)
            | _        -> ( 1,  0)
        (1 (*number*), (0,0) (*coord*), (1,0) (*direction*), 0 (*current step*), 1 (*stepLimit*), 0 (*dirStep*)) |> 
        Seq.unfold (fun (num, (x,y), (dx,dy), step, stepLimit, dirStep) ->
            if step < stepLimit
            then Some((num, (x,y)), (num+1, (x+dx, y+dy), (dx,dy), step+1, stepLimit, dirStep))
            else 
                let (ndx, ndy) = changeDirection (dx,dy)
                if dirStep < 1
                then Some((num, (x,y)), (num+1, (x+ndx, y+ndy), (ndx, ndy), 1, stepLimit, dirStep + 1))
                else Some((num, (x,y)), (num+1, (x+ndx, y+ndy), (ndx, ndy), 1, stepLimit+1, 0))
                ) 
    let spiralSize = 1001                
    spiral |> Seq.take (spiralSize*spiralSize) |> 
    Seq.filter (fun (v,(x,y)) -> System.Math.Abs x = System.Math.Abs y) |>
    Seq.map (fun (v,(x,y)) -> v) |>
    Seq.fold (+) 0

print_any answer    