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


merge : Stack.Stack SplitList -> Stack.Stack SplitList
merge stack =
  if Stack.size stack > 1 then 
    let leftSplitList = (Stack.pop stack |> Tuple.first) |> Maybe.withDefault (SplitList [] UNSORTED)
        rightSplitList = Stack.pop stack |> Tuple.second |> Stack.pop  |> Tuple.first  |>Maybe.withDefault (SplitList [] UNSORTED)
    in
      if canMerge leftSplitList rightSplitList then
        Stack.pop stack |>
        Tuple.second |>
        Stack.pop |> 
        Tuple.second |>
        Stack.push ( SplitList ((List.foldl (\x a -> mergeItem leftSplitList.list x {a| updated = False}) (SortedList [] 0 False) rightSplitList.list) |> 
        .slist) SORTED) |>
        merge
      else
        Stack.pop stack |> 
        Tuple.second |> 
        split |>  
        Stack.push leftSplitList |> 
        merge
    else
      stack


-- SPLIT

type alias SplitList = {list : List Int, status : State}

canSplit : SplitList -> Bool 
canSplit splitList =
  if (splitList.status == UNSORTED) && (compare (List.length splitList.list) 1 == GT) then 
    True 
  else 
    False



split : Stack.Stack SplitList -> Stack.Stack SplitList
split stack =
  if canSplit (Maybe.withDefault (SplitList [] UNSORTED) (Stack.pop stack |> Tuple.first)) then
    let {list,status} = Stack.peek stack |> Maybe.withDefault (SplitList [] UNSORTED) 
    in
      Stack.pop stack |> Tuple.second |>
      Stack.push (SplitList (List.drop (ceiling((toFloat (List.length list))/2)) list) (if floor((toFloat (List.length list))/2) ==1 then SORTED else UNSORTED)) |> 
      Stack.push (SplitList (List.take (ceiling((toFloat (List.length list))/2)) list) (if ceiling((toFloat (List.length list))/2) ==1 then SORTED else UNSORTED)) |> 
      split 
  else
    stack


-- Merge Sort

mergeSort : List Int -> List Int
mergeSort ul =
    Stack.empty|> 
    Stack.push (SplitList ul UNSORTED) |>
    split |>
    merge |>
    Stack.pop |> 
    Tuple.first |> 
    Maybe.withDefault (SplitList [] SORTED) |> 
    .list
