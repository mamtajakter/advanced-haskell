module Main where

import Control.Applicative ((<$>), (<*>))
import Data.List
import Data.Monoid 
import System.Random
import System.Exit
import Control.Monad
import Lib


main:: IO()
main= do
    putStrLn "Welcome to BlackJack!"
    d <- freshDeck
    gameLoop d



gameLoop:: Deck-> IO a
gameLoop d=do 
        let qready="Ready?"
        let help1="Press Enter to continue"
        x<-getLine
        let parse' x= Just x
        let act x = return ()
        prompt qready help1 parse' act
        (dh,dd)<-deal d
        putStrLn ("The dealer's first card is:"++ prettyPrint ((head dh) : []) ++".")
        (ph,pd)<-deal dd
        (h1,d1)<-playerTurn ph pd
        when ((handValue h1)>21) (do 
          putStrLn ("You busted! " ++ prettyPrint h1 ++ ", The house wins.")
          gameLoop d1)
        (h2,d2)<-dealerTurn dh d1
        when ((handValue h2)>21) (do
          putStrLn ("The dealer is busted! " ++ prettyPrint h2 ++ ", You win!") 
          gameLoop d2)  
        when (((handValue h1)==(handValue h2)) && ((handValue h2)==21 )) (do
          if ((length h1) ==(length h2))
             then do {putStrLn ("The dealer reveals the hand: " ++ prettyPrint h2++", Tie; nobody wins.");  gameLoop d2}
             else do { if (length h1) >(length h2)
                          then do {putStrLn ("The dealer reveals the hand: " ++ prettyPrint h2++", The house wins."); gameLoop d2}
                          else do {putStrLn ("The dealer reveals the hand: " ++ prettyPrint h2++", You win."); gameLoop d2}})
        when (((handValue h1)==(handValue h2)) && ((handValue h2)/=21 )) (do
          putStrLn ("The dealer reveals the hand: " ++ prettyPrint h2 ++ ", Tie; nobody wins.");   gameLoop d2)
        if ((handValue h1)>(handValue h2)) 
           then do {putStrLn ("The dealer reveals the hand: " ++ prettyPrint h2 ++ ", You win.");   gameLoop d2}
           else do {putStrLn ("The dealer reveals the hand: " ++ prettyPrint h2 ++ ", The house wins.");  gameLoop d2}
        gameLoop d2

--        when (x=="stand") (putStrLn ("The dealer reveals the hand: " ++ prettyPrint dh))

