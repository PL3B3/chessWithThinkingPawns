module Game (
  Network,
  Player,
  game,
  game2,
  next_turn,
  except,
  newturn,
  ps,
  round_sigmoid,
  score
) where

import LinearAlgebra as L

data Network = Network Int

type Player = ((L.Tensor Double -> [Double]), Int)

game :: [Player] -> L.Tensor Double -> Int -> ([Player], L.Tensor Double)
game agents gameboard turns = if turns == 1 then next_turn agents gameboard else game (fst nextgen) (snd nextgen) (pred turns)
  where nextgen = next_turn agents gameboard

game2 :: ([Player], L.Tensor Double) -> Int -> ([Player], L.Tensor Double)
game2 i t = foldl (\a b -> newturn a) i [1..t]

--cols = players, rows = turns
--corrected player referencing (because each player has (num players - 1) decisions, and I have to evaluate each player's decision relative to the others' to ascertain payoffs, I need to make sure the player's decision towards player b, and player b's decision towards our player are aligned)
newturn :: ([Player], L.Tensor Double) -> ([Player], L.Tensor Double)
newturn prev@(p,g@(L.Tensor s i)) = (nps, ngb)
  where nps = foldl (\c d -> c ++ [(fst (p !! d), (snd (p !! d)) + (sum $ zipWith (score) (dci !! d) (aln d))) :: Player]) [] cn1
        ngb = L.makeTensor [succ $ s !! 0, s !! 1] (i ++ (concat dcs))
        (cn1, cn2) = ([0..(pred $ length p)], [0..(pred $ pred $ length p)])
        dcs = foldl (\a b -> a ++ [b (L.get_row g (pred $ s !! 0))]) [] (map fst p)
        dci = map (map round_sigmoid) dcs
        aln d = foldl (\e f -> e ++ [if (f < d) then ((except d dci) !! f) !! (d - 1) else ((except d dci) !! f) !! d]) [] cn2

next_turn :: [Player] -> L.Tensor Double -> ([Player], L.Tensor Double)
next_turn players gameboard@(L.Tensor shape info) = (new_players, new_gameboard)
  where new_players = foldl (\ls dex -> ls ++ [(fst (players !! dex), (snd (players !! dex)) + (sum $ zipWith (score) (int_decision_list !! dex) (map (!! dex) (except dex int_decision_list)))) :: Player]) [] (ps num_players)
        num_players = length players
        new_gameboard = (L.makeTensor [1 + shape !! 1, shape !! 0] ((infoOf (sd gameboard [1,0])) ++ (concat decision_list)))
        decision_list = foldl (\a b -> a ++ [(b gameboard)]) [] (map fst players)
        int_decision_list = map (map round_sigmoid) decision_list


except :: Int -> [a] -> [a]
except dex ls = let split = splitAt dex ls in (fst split) ++ (tail $ snd split)


ps a = [0..(pred a)]

round_sigmoid :: Double -> Int
round_sigmoid dub = fromEnum (dub > 0.5)

score :: Int -> Int -> Int
score ourdecision theirdecision
  | ourdecision == 0 = if theirdecision == 0 then 6 else 1
  | otherwise = if theirdecision == 0 then 10 else 3
