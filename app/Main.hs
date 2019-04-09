module Main where

import Lib

main :: IO ()
main = do
  -- lagrange
  -- dane L(0)=7, L(1)=3, L(2)=-1, L(4)=3, L(5)=-3

  -- wyświetlenie L(3)
  print $ lagrange [(0,7),(1,3),(2,-1),(4,3),(5,-3)] 3


  -- hermite
  -- dane H(0)=7, H(1)=3, H(2)=-1, H(4)=3, H(5)=-3
  -- H'(0)=3, H'(4)=4, H'(5)=1, H''(5)=2

  -- obliczenie takich danych, co z nich szybko można wielomian H wyznaczyć
  let pol = hermite'
            [ (0,[7,3])
            , (1,[3])
            , (2,[-1])
            , (4,[3,1])
            , (5,[-3,1,2])
            ]
  -- wyświetlenie H(3)
  print $ hermite pol 3
