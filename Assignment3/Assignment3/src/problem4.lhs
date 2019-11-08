> module Problem4 where

Proving Correctness With a Specification (45
points)
The purpose behind a red-black tree is to more efficiently implement a search (via
the above find function) that scales logarithmically rather than linearly with
the number of elements to search through. For example, doubling the number of
elements in the red-black tree only adds a constant (some fixed number) of steps
to find, since only a single path from the root of the tree to a leaf is searched,
and the tree only grows (approximately) one more level deep after doubling.
7
By analogy, the red-black tree find function should be equivalent to a linear
search through a sequential list, just faster. We can define the linear search
function as

> search :: Eq a => a-> [a]-> Maybe a
> search x []= Nothing
> search x (y:ys)
>         | x==y      = Just y
>         | otherwise = search x ys

It is relatively easier to see that the linear search function is correct because it
exhaustively checks every element: if the given list contains an element equal to
the given value, then search will return that element, and otherwise search will
return nothing. In contrast, it is harder to see that the binary find function is
harder to see that it is correct, because it skips over many elements without even
checking them. But, you can prove that the efficient find function is correct by
proving that it is equal to the simpler search specification function.


Exercise 4.1 (10 points). Using equational reasoning, show that, for all values
x :: a and ys :: [a], if x /= y for each y in the list ys, then
search x ys = Nothing

Proof:
We can show this by induction on lists, with the two basic cases (empty list [], and a "cons" y:ys)

The base case is direct enough to show:

search x []
= Nothing {-definition of search functions base case-}

Now lets try the Inductiove step, where we have the non-empty list as the second argument:

search x (y:yss)
=search x yss{-As x/=y for each y in ys and we can assume that search x ys (otherwise case)}
= Nothing {-IH-}


Exercise 4.2 (10 points). Suppose that x :: a, ys :: [a], and zs :: [a]
are arbitrary values. Use equational reasoning to:
1. Show that, if x /= y for each y in the list ys, then
search x (ys ++ zs) = search x zs

Proof:
1. Show that, if x /= y for each y in the list ys, then
search x (ys ++ zs) = search x zs

 
Proof:

Assume ys=y:yss and Our inductive hypothesis is search x (yss ++ zs) = search x zs
We can show this by induction on lists, with the two basic cases (empty list [], and a "cons" y:ys)

The base case is direct enough to show:

 

search x ([]++ zs)
= search x zs {-As search x []= Nothing according to the base case of 4.1-}

 

Now lets try the Inductiove step, where we have the non-empty list as the second argument:

 

search x (y:yss++zs)
=search x  y: (yss++zs) {-as (++) function-}
=search x (yss++zs) {-As x/=y -}
=search x zs {-IH-}

 

2. Show that, if x /= z for each z in the list zs, then
search x (ys ++ zs) = search x ys

 
Proof:

Assume ys=y:yss and Our inductive hypothesis is search x (yss ++ zs) = search x ys
We can show this by induction on lists, with the two basic cases (empty list [], and a "cons" y:ys)

The base case is direct enough to show:

 

L.H.S.
search x ([]++ zs)
= search x zs {-As (++) function definition }
= Nothing {-According to 4.1: search x zs = Nothing if x/=z for every z in zs-}

R.H.S.
search x []
=Nothing


Now let's try the Inductive step, where we have the non-empty list as the second argument:
There is two case we need to consider here, 

the case when x is equal to y which is the head of ys

 

L.H.S:
search x ((y:yss))++zs)
=search x (y: (yss++zs)){-As (++) function-}
=Just y{-According to the inductive step of search function-}


R.H.S:
search x (y:yss)
=Just y{-According to the inductive step of search function-}


For the case when x is not equal to the head of ys:

L.H.S:
search x ((y:yss))++zs)
=search x (y: (yss++zs)) {-As (++) function-}
= search x (yss++zs) {-as x is not equal to the head of ys and according to the search functon (otherwise)-}
=search x (yss++zs)
=search x ys {-IH-}
=R.H.S


Exercise 4.3 (10 points). Show that for all values x :: a, y :: a, ys :: [a],
and zs :: [a], if x == y and x /= z for each z in zs, then
search x (zs ++ ([y] ++ ys)) = Just y

Proof:

ssumption: zs=z:zs

I.H: search x (zss ++[y]++ys)=Just y

 

Base case:

search x ([]++ ([y]++ys)

=search x ([y]++ys){-(++) function-}

=search x (y: ys) {-(++) function-}

=Just y {-as x==y and according to search function-}

 

Inductive step:

L.H.S:
search x (zs++ ([y]++ys)

=search x ((z:zss)++[y]++ys){-(++) function-}

=search x (zss++[y]++ys){-as x/=z-}

=Just y {-I.H.-}
=R.H.S


Exercise 4.4 (15 points). Show that, for all red-black trees t :: RBTree a and
element values x :: a, if t is a well-formed red-black tree (meaning it satisfies
properties 0–3 described in the introduction to red-black trees), then
search x (toList t) = find x t


Proof:


We can show this by induction on tree, with the two basic cases (L, and a N color l x r)

 

We have the assumptios:
search x (toList l) = find x l
search x (toList r) = find x r




we have these functions:


>toList :: Ord a =>RBTree a -> [a]
>toList L=[]
>toList (N col a y b)= toList a ++ [y] ++ toList b

 

>find :: Ord a => a -> RBTree a -> Maybe a
>find x L= Nothing
>find x (N _ l n r)
> | x == n = Just n
> | x < n = find x l 
> | x > n = find x r

 

>inOrder :: RBTree Int -> Bool
>inOrder t = fff (toList t)

>fff :: Ord a => [a] -> Bool 
>fff x = nub (sort x) == x

 

The base case is direct enough to show:

 

L.H.S:
search x (toList L)
=search x [] {-base case of toList-}
=Nothing {-4.1-}

 

R.H.S: 
find x L
=Nothing {-base case of find function-}


Now lets try the Inducticve step, where we have the non-empty tree as an argument in both side:

 

L.H.S:
search x (toList t)
=search x (toList (N c l n r))

=search x (toList l ++ [n]++toList r) {-toList nonempty tree case-}

 

now, cnsider 3 cases here, when x==n, x<n, x>n

 

for x==n,

search x (toList l ++ [n]++toList r)

= Just n {-according 4.3 as x==n,x/=(any element in subtree l ) and as t is a inOrder tree, any element in l/r must be not equal to n(either greater or smaller),- }

 

for x<n

 

search x (toList l ++ [n]++toList r)

=search x (toList l ++ ([n]++toList r) {-(++) function-}

= search x (toList l){-according to 4.2 2-}

= find x l {-IH-}

 

for x>n

 

search x (toList l ++ [n]++toList r)

=search x ((toList l ++ [n])++toList r) {-(++) function-}

= search x (toList r){-according to 4.2 1-}

= find x r {-IH-}

 

R.H.S:

find x (N c l n r)

 

where we also get three cases:

 

when x==n,

find x (N c l n r)
=Just n {-According to find function x==y-}

 

when x<n,
find x (N c l n r)
= find x l{-According to find function when x<n-}

 

when x>n,
find x (N c l n r)
= find x r{-According to find function when x>n-}

 

so, R.H.S=L.H.S