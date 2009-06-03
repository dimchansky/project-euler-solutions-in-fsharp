#light

// http://www.problems.ru/inf/view_problem_details_new.php?id=102943

let factorial N =
    if N < 1 then 1I
    else {1I .. bigint.FromInt32(N)} |> Seq.fold ( * ) 1I

let getIndexOfLexicographicPermutation permutation =
    let rec getIndexOfLexicographicPermutation_aux p sum =
        let N = p |> List.length
        let permElementTo1_N el = p |> Seq.orderBy (fun a -> a) |> Seq.zip {1..N} |> Seq.find_index (fun v -> v=el)
        let pl = p |> List.map (fun v -> permElementTo1_N v)
        match pl with
        | a::tl -> 
            let num = bigint.FromInt32(a-1) * (factorial (N-1))
            getIndexOfLexicographicPermutation_aux tl (sum+num)
        | [] -> sum
    getIndexOfLexicographicPermutation_aux permutation 0I
            
let getLexicographicPermutationByIndex permutation index =    
    let rec getLexicographicPermutationByIndex_aux permEls i acc =
        match permEls with
        | [] -> acc
        | p  ->
            let N = p |> List.length
            let from1_NToPermElement n = {1..N} |> Seq.zip (p |> Seq.orderBy (fun a -> a)) |> Seq.find_index (fun v -> v=n)
            let facN_1 = factorial (N-1)
            let elOfPerm = bigint.ToInt32(bigint.FromInt32(i) / facN_1 + 1I) |> from1_NToPermElement
            let newPermEls = p |> List.filter (fun v -> v <> elOfPerm)
            let newi = bigint.ToInt32(bigint.FromInt32(i) % facN_1) 
            let newacc = acc @ [elOfPerm]
            getLexicographicPermutationByIndex_aux newPermEls newi newacc
    getLexicographicPermutationByIndex_aux permutation index []
    
let answer =
    getLexicographicPermutationByIndex ({0..9} |> Seq.to_list) (1000000-1) |>
    List.map (fun v -> v.ToString()) |>
    List.reduce_left (fun a b -> a + b)
    
print_any answer