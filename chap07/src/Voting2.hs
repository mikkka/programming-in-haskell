module Voting2 where
  import Voting1

  ballots :: [[String]]
  ballots = [
    ["red", "green"], 
    ["blue"],
    ["green", "red", "blue"],
    ["blue", "green", "red"],
    ["green"]]

  rmempty :: Eq a => [[a]] -> [[a]]
  rmempty = filter (/= [])

  elim :: Eq a => a -> [[a]] -> [[a]]
  elim x = map (filter (/= x))

  rank :: Ord a => [[a]] -> [a]
  rank = map snd . result . map head

  winnerr :: Ord a => [[a]] -> a
  winnerr bs = case rank (rmempty bs) of
                [c]       -> c
                (c:cs)    -> winnerr (elim c bs)