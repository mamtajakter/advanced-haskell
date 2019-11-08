
import Control.Applicative
import Test.QuickCheck
import Test.Hspec
--import Data.List
import Control.Exception
import Control.Monad 
import Data.Maybe as Maybe
import Data.List (sort, nub)
--import Shuffle

main= do 
  fancyMain

fancyMain :: IO ()
fancyMain = do
  -- --quickCheck inOrder
  -- quickCheck (\x -> inOrder . insert x) 
  -- quickCheck (\x -> isBlackRoot . insert x) 
  -- quickCheck (\x -> noRedChain . insert x) 
  -- quickCheck (\x -> samePathValues . insert x) 
  quickCheck (\x y -> inOrderAt . insertAt x y)  
  quickCheck (\x y -> isBlackRootAt . insertAt x y)   
  quickCheck (\x y -> noRedChainAt . insertAt x y)  
  quickCheck (\x y -> samePathValuesAt . insertAt x y)  
  quickCheck findAfterInsertAt 
  quickCheck findAfterDeleteAt 
  quickCheck deleteAfterInsert
  
data Color = R | B 
               deriving (Eq, Show)

data RBTree a = L | N Color (RBTree a) a (RBTree a)
               deriving (Show)

type RBMap i a = RBTree (Indexed i (Maybe a))

instance (Arbitrary i, Arbitrary a) => Arbitrary (Indexed i a) where
  arbitrary = do
    i <- arbitrary 
    a <- arbitrary  
    return (Indexed i a)

instance  (Arbitrary a, Ord a) =>Arbitrary (RBTree a) where
  arbitrary = do
    x <-  arbitrary 
    return (fromList x)
    

--okay
blackenRoot :: RBTree a -> RBTree a
blackenRoot L= L
blackenRoot (N _ l n r)= N B l n r

--not sure
balance :: RBTree a -> RBTree a
balance L =L
balance (N B (N R (N R a x b) y c) z d) = N R (N B a x b) y (N B c z d)
balance (N B (N R a x (N R b y c)) z d) = N R (N B a x b) y (N B c z d)
balance (N B a x (N R (N R b y c) z d)) = N R (N B a x b) y (N B c z d)
balance (N B a x (N R b y (N R c z d))) = N R (N B a x b) y (N B c z d)
balance (N B a x b) = N B a x b
balance (N R a x b) = N R a x b

ins :: Ord a => a -> RBTree a -> RBTree a
ins x L = N R L x L
ins x (N col a y b)
          | x < y  = balance (N col (ins x a) y b)
          | x == y = N col a x b
          | x > y  = balance (N col a y (ins x b))
--okay
insert :: Ord a => a -> RBTree a -> RBTree a        
insert x t = blackenRoot ( ins x t) 



t=N B (N B (N R L (-3) L) (-4) L) 3 (N B (N R L 3 L) (-2) L)

--okay
rootColor :: RBTree a -> Color
rootColor L= B
rootColor (N c a y b)=c


colorValue :: Color -> Int
colorValue c
           | c==R =0
           | c==B =1


type Path a = [(Color, a)]

--okay
--paths :: RBTree a -> [Path a]
paths L = [[]]
paths (N col L x r) = map ((col, x):) (paths r)
paths (N col l x L) = map ((col, x):) (paths l)
paths (N col l x r) = map ((col,x):) (lhs++rhs)
                  where lhs = paths l
                        rhs = paths r


--okay
inOrder :: RBTree Int -> Bool
inOrder t = fff (toList t) 

fff :: Ord a => [a] -> Bool  
fff x = nub (sort x) == x

isBlackRoot :: RBTree Int -> Bool
isBlackRoot t = rootColor t == B

noRedChain :: RBTree Int  -> Bool
noRedChain (N color a y b) = helper (N color a y b) where
 helper :: RBTree Int -> Bool
 helper L = True
 helper (N col l _ r) = helper l && helper r
   && if col == R
      then (rootColor l == B && rootColor r == B)
      else True
 
  
samePathValues :: RBTree Int -> Bool
samePathValues (N color a y b) = fst (helper (N color a y b)) where 
   helper :: RBTree a -> (Bool, Int)
   helper L = (True, 0)
   helper (N c a x b) = (h1 == h2 && b1 && b2, if c == B then h1 + 1 else h1) where
     (b1 , h1) = helper a
     (b2 , h2) = helper b



roundTripSort :: [Int] -> Bool
roundTripSort xs = toList (fromList xs)==xs

findAfterInsert :: Int -> RBTree Int -> Bool
findAfterInsert x t=find x (insert x t)== Just x


-- 
data Indexed i a = Indexed i a deriving (Show) 

instance Eq i => Eq (Indexed i a) where
    (Indexed a _) == (Indexed b _) = a == b
    
    
instance Ord i => Ord (Indexed i a) where
  compare (Indexed i _) (Indexed j _) = compare i j
    

--okay
find :: Ord a => a -> RBTree a -> Maybe a
find x L= Nothing
find x (N _ l n r)
    | x == n = Just n
    | x  < n = find x l 
    | x  > n = find x r

ff:: Indexed i (Maybe a) -> i
ff (Indexed ii _)= ii

ffa:: Indexed i (Maybe a) -> Maybe a
ffa (Indexed ii Nothing)= Nothing
ffa (Indexed ii (Just aa))= Just aa



-- insAt :: Ord i => i ->a-> RBMap i a -> RBMap i a
-- insAt ii aa L = N R L (Indexed ii (Just aa)) L
-- insAt ii aa (N col L y@(Indexed i Nothing) L)= N col L (Indexed ii (Just aa)) L
-- insAt ii aa (N col L y@(Indexed i (Just x)) L)
--           | ii < i  = N col ( N R L (Indexed ii (Just aa)) L) y L
--           | ii == i = N col L (Indexed ii (Just aa)) L
--           | i > i  = N col  ( N R L y L) (Indexed ii (Just aa)) L  
-- insAt ii aa (N col a y@(Indexed i _) b)
--           | ii < i  = N col (insAt ii aa a) y b
--           | ii == i = N col a (Indexed ii (Just aa)) b
--           | i > i  = N col a y (insAt  ii aa b)
--okay
-- insertAt :: Ord i => i->a -> RBMap i a -> RBMap i a        
-- insertAt ii aa t = blackenRoot ( insAt ii aa t) 

insertAt :: Ord i => i -> a-> RBMap i a -> RBMap i a
insertAt ii aa L= L
insertAt ii aa t = insert (Indexed ii (Just aa)) t


findAt :: Ord i => i -> RBMap i a -> Maybe a
findAt ii L= Nothing
findAt ii t = case (find (Indexed ii Nothing) t) of 
              Nothing -> Nothing
              Just (Indexed _ x ) ->  x


del :: Ord i => i -> RBMap i a -> RBMap i a
del x L = L
del x (N col a y@(Indexed ii _) b)
          | x < ii  = N col (del x a) y b
          | x == ii= N col a (Indexed ii Nothing) b
          | x > ii = N col a y (del x b)
--okay
deleteAt :: Ord i => i -> RBMap i a -> RBMap i a       
deleteAt x t =  blackenRoot (del x t)


ffia:: [Indexed i (Maybe a)] -> [Indexed i a]
ffia []=[]
ffia ((Indexed ii (Just aa)):xs)= (Indexed ii aa) : ffia xs

--okay
toList :: Ord a =>RBTree a -> [a]
toList L=[]
toList (N col a y b)= toList a ++ [y] ++ toList b

-- toAssoc :: RBMap i a-> [Indexed i (Maybe a)]
-- toAssoc L=[]
-- toAssoc (N col a y@(Indexed ii _) b)= toAssoc a ++ [y] ++ toAssoc b


toAssoc :: RBMap i a-> [Indexed i a]
toAssoc L=[]
toAssoc (N col a y@(Indexed ii Nothing) b)= toAssoc a ++ toAssoc b
toAssoc (N col a y@(Indexed ii (Just x)) b)= toAssoc a ++ [Indexed ii x] ++ toAssoc b



fromList :: Ord a => [a] -> RBTree a
fromList [] = L
fromList (x:xs)= insert x (fromList xs)

-- fromAssoc :: Ord i=>  [Indexed i (Maybe a)]-> RBMap i a
-- fromAssoc []=L
-- fromAssoc ((Indexed ii (Just x)):xs)= insertAt ii x (fromAssoc xs)

fromAssoc :: Ord i=>  [Indexed i a]-> RBMap i a
fromAssoc []=L
fromAssoc ((Indexed ii x):xs)= insertAt ii x (fromAssoc xs)

--okay
inOrderAt :: RBMap Int Int -> Bool
inOrderAt t = isSortedNoDuplicatesAt (toAssoc t) 

isSortedNoDuplicatesAt :: [Indexed Int Int] -> Bool  
isSortedNoDuplicatesAt x = nub (sort x) == x

isBlackRootAt :: RBMap Int Int -> Bool
isBlackRootAt t = rootColorAt t == B

rootColorAt :: RBMap i a -> Color
rootColorAt L= B
rootColorAt (N c a y b)=c


noRedChainAt :: RBMap Int Int  -> Bool
noRedChainAt L= True
noRedChainAt (N color a y b) = helper (N color a y b) where
 helper :: RBMap Int Int -> Bool
 helper L = True
 helper (N col l _ r) = helper l && helper r
   && if col == R
      then (rootColorAt l == B && rootColorAt r == B)
      else True
  
samePathValuesAt :: RBMap Int Int -> Bool
samePathValuesAt L= True
samePathValuesAt (N color a y b) = fst (helper (N color a y b)) where 
   helper :: RBMap Int Int -> (Bool, Int)
   helper L = (True, 0)
   helper (N c a x b) = (h1 == h2 && b1 && b2, if c == B then h1 + 1 else h1) where
     (b1 , h1) = helper a
     (b2 , h2) = helper b


findAfterInsertAt :: Int-> Int -> RBMap Int Int-> Bool
findAfterInsertAt i x L = findAt i (N B L (Indexed i (Just x)) L)==Just x
findAfterInsertAt i x t = findAt i (insertAt i x t)== Just x
 
findAfterDeleteAt :: Int->  RBMap Int Int-> Bool
findAfterDeleteAt i t = findAt i (deleteAt i t)== Nothing
 
deleteAfterInsert:: Int-> Int -> RBMap Int Int-> Bool
deleteAfterInsert i x t = toAssoc (deleteAt i (insertAt i x t))== toAssoc (deleteAt i t)

  
-- N B (N B (N B (N R L 1 L) 2 L) 3 (N R L 4 L)) 5 (N B (N B (N R L 6 L) 7 L) 8 (N R L 9 L))


-- N B (N B (N B (N R L 2 L) 5 L) 7 (N R L 2 L)) 3 (N B (N B (N R L 1 L) 7 L) 8 (N R L 9 L))
-- Branch (Branch (Branch Leaf (-2) Leaf) (-1) (Branch Leaf 0 Leaf)) 0 (Branch (Branch Leaf 0 Leaf) 1 (Branch Leaf 2 Leaf))