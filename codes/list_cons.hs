data List a = Nil | Cons a (List a) deriving (Show)

is_empty :: List t -> Bool
is_empty Nil =  True
is_empty _   =  False

is_empty2  lst = case lst of 
                    Nil -> True
                    _   -> False

count :: List t -> Int
count lst = 
    case lst of 
        Nil             -> 0
        Cons a next_lst -> 1 + count next_lst

suml lst = 
    case lst of
        Nil             -> 0
        Cons a next_lst -> a + suml next_lst
        
headl (Cons a _) = a
headl Nil        = error "Empty List"

safe_headl (Cons a _) = Just a
safe_headl Nil        = Nothing

lastl Nil           = error "Failed: Empty List"
lastl (Cons a Nil)  = a
lastl (Cons _ t)    = lastl t

{- Converts to Haskell List -}
to_list Nil         = []
to_list (Cons x xs) = x:(to_list xs)


takel n lst =
    case (n, lst) of
    (_, Nil      )  -> Nil
    (0, _        )  -> Nil
    (k, Cons h  t)  -> Cons h (takel (n-1) t)


nth lst n = 
    case (n, lst) of
    (_, Nil)       -> error "Index too large"
    (0, Cons a _)  -> a
    (k, Cons h t)  -> nth t (n-1) 


mapl f Nil          = Nil
mapl f (Cons h t)   = Cons (f h) (mapl f t)

filterl  f  Nil        = Nil
filterl  f (Cons h t)  = 
    if f h 
        then  Cons h (filterl f t)
        else  filterl f t

{- Empty List [] -}
list0 = Nil

{- Sigle element list -}
list1 = Cons 10 Nil
list2 = Cons "Haskell" Nil

list3 = Cons 10 (Cons 20 (Cons 30 Nil))
list4 = Cons 1.25 (Cons 0.65 (Cons 8.123 ( Cons 9.434 Nil)))
