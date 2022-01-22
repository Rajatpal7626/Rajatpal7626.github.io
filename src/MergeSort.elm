module MergeSort exposing (..)
import Stack
import Basics

type State = UNSORTED  | SORTED 

--MERGE

type alias SortedList = {slist : List Int, lidx : Int, updated : Bool}

canMerge : SplitList -> SplitList -> Bool 
canMerge splitList1 splitList2 =
  if (splitList1.status == SORTED && splitList2.status == SORTED) then 
    True 
  else 
    False


mergeItem : List Int -> Int -> SortedList -> SortedList
mergeItem llist num sortedList =
  case llist of
     [] ->
        {sortedList| slist = sortedList.slist++[num]}
     _ ->
        List.foldl (\x a ->
          if (compare x num == LT) then
            if (compare (a.lidx+1)  (List.length llist ) == EQ ) then
              {a | slist = a.slist++[x,num] , lidx = a.lidx +1 } 
            else 
              {a | slist = a.slist++[x] , lidx = a.lidx +1 }
          else if a.updated == False then
            { a | slist =  a.slist ++ [num,x] , updated = True } 
          else {a | slist =  a.slist ++ [x] , updated = True }
        ) sortedList llist


merge : (Stack.Stack SplitList, List (Stack.Stack SplitList)) -> (Stack.Stack SplitList, List (Stack.Stack SplitList))
merge param =
  if Stack.size (Tuple.first param) > 1 then 
    let leftSplitList = (Stack.pop (Tuple.first param) |> Tuple.first) |> Maybe.withDefault (SplitList [] UNSORTED)
        rightSplitList = Stack.pop (Tuple.first param) |> Tuple.second |> Stack.pop  |> Tuple.first  |>Maybe.withDefault (SplitList [] UNSORTED)
    in
      if canMerge leftSplitList rightSplitList then        
        ((Stack.pop (Tuple.first param) |>
        Tuple.second |>
        Stack.pop |> 
        Tuple.second |>
        Stack.push ( SplitList ((List.foldl (\x a -> mergeItem leftSplitList.list x {a| updated = False}) (SortedList [] 0 False) rightSplitList.list) |> .slist) SORTED)) , (Tuple.first param) :: (Tuple.second param)) |>  
        merge
      else
       ((((Stack.pop (Tuple.first param) |> 
        Tuple.second ),(Tuple.first param) :: (Tuple.second param))|> 
        split |> Tuple.first |> 
        Stack.push leftSplitList),(Tuple.first param) :: (Tuple.second param)) |> 
        merge
    else
      param


-- SPLIT

type alias SplitList = {list : List Int, status : State}

canSplit : SplitList -> Bool 
canSplit splitList =
  if (splitList.status == UNSORTED) && (compare (List.length splitList.list) 1 == GT) then 
    True 
  else 
    False



split : (Stack.Stack SplitList, List (Stack.Stack SplitList)) -> (Stack.Stack SplitList, List (Stack.Stack SplitList))
split param =
  if canSplit (Maybe.withDefault (SplitList [] UNSORTED) (Stack.pop (Tuple.first param) |> Tuple.first)) then
    let {list,status} = Stack.peek (Tuple.first param) |> Maybe.withDefault (SplitList [] UNSORTED) 
    in 
      ((Stack.pop (Tuple.first param) |>
        Tuple.second |>
        Stack.push (SplitList (List.drop (ceiling((toFloat (List.length list))/2)) list) (if floor((toFloat (List.length list))/2) ==1 then SORTED else UNSORTED)) |> 
        Stack.push (SplitList (List.take (ceiling((toFloat (List.length list))/2)) list) (if ceiling((toFloat (List.length list))/2) ==1 then SORTED else UNSORTED))),
        (Tuple.first param) :: (Tuple.second param))|> 
      split
  else
    param


-- Merge Sort

mergeSort : List Int -> (List Int, List (List (List Int)))
mergeSort ul =
  let hist = []
      output = ((Stack.push (SplitList ul UNSORTED) Stack.empty) ,hist) |> split |> merge
      sortedList = Tuple.first output |> Stack.pop |> Tuple.first |> Maybe.withDefault (SplitList [] SORTED) |> .list
      traj = List.foldl (\x ao ->(  List.foldr (\y ai -> y.list :: ai ) []  (Stack.toList x) :: ao)) [] (Tuple.second output)
  in 
    (sortedList , traj)

