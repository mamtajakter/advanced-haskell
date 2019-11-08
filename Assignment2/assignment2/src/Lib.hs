module Lib where

import Control.Applicative ((<$>), (<*>))
import Data.List
import Data.Monoid 
import System.Random
import System.Exit
import Control.Monad

--1.1,1.3
data Suit = Hearts | Spades | Clubs | Diamonds
 deriving ( Eq,Enum)


data Royalty =  King | Queen | Jack
 deriving ( Eq, Enum)
 

data Card = Aces Suit | Faces Suit Royalty | Numbers Suit Int 
 deriving (Eq)

--1.2
type Hand = [Card]
type Deck = [Card]
--1.4
instance Show Suit where
 show Hearts= "\x2661"
 show Spades ="\x2660"
 show Diamonds = "\x2662"
 show Clubs= "\x2663"

instance Show Royalty where
 show King= "K"
 show Queen ="Q"
 show Jack = "J"

instance Show Card where
 show (Aces s1) =  "A" ++ show s1
 show (Faces s1 r1) = show r1  ++ show s1  
 show (Numbers s1 int1) = show int1 ++ show s1
--1.5
suits:: [Suit]
suits=[Hearts,Spades,Clubs , Diamonds]

faces:: [Royalty]
faces=[King , Queen , Jack]

numbers:: [Int] 
numbers=[2..10]

suitface ss ff=  [Faces s f   | s<- ss, f<- ff]
suitace ss=      [Aces s      | s<- ss        ]
suitnumber ss nn=[Numbers s n | s<- ss, n<- nn]


sa=[suitaces    | suitaces    <- suitace suits  ]
sf=[suitfaces   | suitfaces   <- suitface suits faces]
sn=[suitnumbers | suitnumbers <- suitnumber suits numbers]

fullDeck :: Deck
fullDeck = sa ++ sf ++ sn

--2.1
cardValue :: Card-> Int
cardValue (Faces s r)   = 10
cardValue (Numbers s x)    = x
cardValue (Aces n)         = 11


--2.2
-- handValue :: Hand->Int
-- handValue [] = 0
-- handValue [x] = cardValue x
-- handValue (x:xs) =    (cardValue x) + (handValue xs)

--3.1

data Score  =  HardSoft Int Int deriving (Eq, Ord, Read, Show)

cardScore:: Card->Score 
cardScore (Aces s1)         = HardSoft 1 10
cardScore (Faces s r) = HardSoft 10 0
cardScore (Numbers s int1)          = HardSoft int1 0

scoreValue :: Score->Int
scoreValue (HardSoft h s) = h+s

--h+s
          
improveScore :: Score ->Score
improveScore (HardSoft h s)
           | (s+h>21) && s>0 = improveScore (HardSoft h (s-10))
           | otherwise = HardSoft h s

-- | s+h<21  = HardSoft (s+h) s 
-- | otherwise = HardSoft h 0

--3.2
instance Monoid Score where
    mempty = HardSoft 0 0  
    mappend (HardSoft hh ss) (HardSoft h s) = improveScore (HardSoft (h+hh) (s+ss))

--3.3
handScore:: Hand -> Score
handScore [] = mempty
handScore (x:xs)=  mappend (cardScore x) (handScore xs)


handValue :: Hand->Int
handValue [] = scoreValue (HardSoft 0 0)
handValue (x:xs) = scoreValue (improveScore (handScore (x:xs)))

--4.1
data Indexed i a = Indexed i a deriving (Show) 
--4.2

instance Eq i => Eq (Indexed i a) where
    (Indexed a _) == (Indexed b _) = a == b
    
    
instance Ord i => Ord (Indexed i a) where
  compare (Indexed i _) (Indexed j _) = compare i j
    
--4.3

ff:: Indexed i a -> a
ff (Indexed i a)=a

shuffle ::  [Int] -> [a] -> [a]
shuffle x y= map ff (sort (zipWith (Indexed) x y))


--ASSIGNMET 2--

--1.1


freshDeck :: IO Deck
freshDeck =do
            g <- newStdGen
            putStrLn ("Shuffling a new deck...")
            return (shuffle (randoms g :: [Int]) fullDeck)


draw:: Deck-> IO (Card,Deck)
draw []= do 
  (d:ds)<-freshDeck 
  return (d,ds)
draw (x:xs)=return (x, xs)


hitHand:: Hand-> Deck-> IO (Hand, Deck)
hitHand x y = do 
    (h,d)<- draw y 
    return ( x ++ [h], d )

deal :: Deck-> IO (Hand, Deck)
deal x = do
     (h1,d1) <- draw x
     (h2,d2) <- draw d1
     return ([h1]++ [h2], d2)

prettyPrint :: Hand-> String
prettyPrint x= (show x) ++ "(" ++ show (handValue x) ++ ")"
--putStrLn (prettyPrint [Aces Hearts, Faces Spades King, Aces Clubs, Numbers Clubs 10, Faces Spades Queen, Aces Diamonds])

prompt :: String               -- Question to ask
       -> String
       -> (String -> Maybe a)  -- Parse the answer
       -> (a-> IO b)
       -> IO b
prompt querry help parse act = do
  putStrLn querry
  input <- getLine
  case input of
    "quit" -> exitWith ExitSuccess
    "help" -> do
      putStrLn help
      prompt querry help parse act
    _      -> case parse input of
                Nothing     -> do
                    putStrLn "I didn't understand that."
                    putStrLn (show help)
                    prompt querry help parse act
                Just x -> act x

data Move =  Hit |  Stand deriving Eq
parse:: String-> Maybe Move
parse x
      | x=="hit"   = Just Hit
      | x=="stand" = Just Stand
      | otherwise  = Nothing


playerTurn :: Hand-> Deck-> IO (Hand, Deck)
playerTurn h d=do 
    let querry = "Your hand is " ++ prettyPrint h ++ ", what do you do?"
    let help= " You can either hit or stand" 
    prompt querry help parse act'
    where act' x= case x of
            Hit-> do 
                (h',d')<-hitHand h d 
                if (handValue h')>21 
                   then return (h',d') else playerTurn h' d'
            Stand-> return (h, d)

          

dealerTurn:: Hand-> Deck-> IO (Hand,Deck)
dealerTurn h d= do
    if (handValue h)<17
    then do {(h',d')<-hitHand h d; dealerTurn h' d' }
    else return (h,d)

--let act x=return ()
