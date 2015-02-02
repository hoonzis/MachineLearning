module MachineLearning.Combinatorics

//taken from https://gist.github.com/yvanin/9ffbd0a3bf8e183dee0a
// assuming elements in a set do not repeat
let rec kCombinations k (set: 'a list) =
    match k with
    | 1 -> set |> List.map (fun x -> [x])
    | _ ->
        match set with
        | [] -> []
        | head::tail ->
            (tail |> kCombinations (k - 1) |> List.map (fun x -> head::x)) 
            @ (tail |> kCombinations k)

//taken from http://stackoverflow.com/questions/4495597/combinations-and-permutations-in-f
/// Gets all combinations (with repetition) of specified length from a list.
let rec getCombsWithRep n lst = 
    match n, lst with
    | 0, _ -> seq [[]]
    | _, [] -> seq []
    | k, (x :: xs) -> Seq.append (Seq.map ((@) [x]) (getCombsWithRep (k - 1) lst)) (getCombsWithRep k xs)