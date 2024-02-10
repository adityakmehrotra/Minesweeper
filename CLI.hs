-- Minesweeper
-- Created By: Aditya Mehrotra
-- Last Update: 02/09/2024

module CLI (main) where

import MyBoard

main = top (\x y z -> initialize x y z :: MyBoard)

top :: Board b => (Int -> (Int, Int) -> (Int, Int) -> b) -> IO ()
top cinit = do putStrLn "Enter Seed...."
               seed <- readLn
               putStrLn "Enter Width of Minesweeper Board"
               width <- readLn
               putStrLn "Enter Height of Minesweeper Board"
               height <- readLn
               putStrLn "First Click...."
               click <- readLn
               loop $ cinit seed (width, height) click

loop :: Board b => b -> IO ()
loop board
  | won board  = putStrLn $ show board ++ "\n Congratulations, you won!"
  | lost board = putStrLn $ show board ++ "\n Sorry, you lost"
  | otherwise  = do putStrLn $ show board
                    newBoard <- flag_loop (Just (-1, -1)) board
                    putStrLn "Click Now"
                    coord <- readLn
                    loop $ click coord newBoard

flag_loop :: Board b => Maybe (Int, Int) -> b -> IO b
flag_loop Nothing board = return board
flag_loop (Just coord) board = do putStrLn "Do you want to place a Flag?"
                                  mcoord <- readLn
                                  flag_loop mcoord (flag coord board)
