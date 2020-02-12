{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Search where

import ProblemState

import qualified Data.Set as S

import Data.List (sortBy)
import Data.Function (on)
import Data.Maybe (fromJust, isJust)

{-
    *** TODO ***

    Tipul unei nod utilizat în procesul de căutare. Recomandăm reținerea unor
    informații legate de:

    * stare;
    * acțiunea care a condus la această stare;
    * nodul părinte, prin explorarea căruia a fost obținut nodul curent;
    * adâncime
    * copiii, ce vor desemna stările învecinate
-}

data Node s a = Node
    { getState  :: s
    , getAction :: a
    , getParent :: Maybe (Node s a)
    , getDepth  :: Int
    , getChildren :: [Node s a]
    } deriving (Eq, Show)

{-
    *** TODO ***

    Întoarce starea stocată într-un nod.
-}

nodeState :: Node s a -> s
nodeState = getState

{-
    *** TODO ***

    Generarea întregului spațiu al stărilor 
    Primește starea inițială și creează nodul corespunzător acestei stări, 
    având drept copii nodurile succesorilor stării curente.
-}

createStateSpace :: (ProblemState s a) => s -> Node s a
createStateSpace initialLevel = let thisNode = Node initialLevel undefined Nothing 0 [] 
                                in thisNode { getChildren = map (\(a2, s2) -> loop s2 a2 thisNode) $ successors $ initialLevel}
    where
        loop s a parent = let thisNode = Node s a (Just parent) (getDepth parent + 1) [] 
                          in thisNode { getChildren = map (\(a2, s2) -> loop s2 a2 thisNode) $ successors $ s}

{-
    *** TODO PENTRU BONUS ***

    Ordonează întreg spațiul stărilor după euristica din ProblemState. 
    Puteți folosi `sortBy` din Data.List.
-}

orderStateSpace :: (ProblemState s a) => Node s a -> Node s a
orderStateSpace node = loop node
    where
        loop nod = nod { getChildren = map loop $ sortBy (compare `on` heuristic . nodeState) $ getChildren nod}

{-
    *** TODO ***

    Întoarce lista nodurilor rezultate prin parcurgerea limitată în adâncime
    a spațiului stărilor, pornind de la nodul dat ca parametru.

    Pentru reținerea stărilor vizitate, recomandăm Data.Set. Constrângerea
    `Ord s` permite utilizarea tipului `Set`.
-}

limitedDfs :: (ProblemState s a, Ord s)
           => Node s a    -- Nodul stării inițiale
           -> Int         -- Adâncimea maximă de explorare
           -> [Node s a]  -- Lista de noduri
limitedDfs nod maxDepth = loop [nod] S.empty []
  where
    loop [] _ acc = reverse acc
    loop (node : nodes) explored acc = loop (neighbors ++ nodes) (S.insert (nodeState node) explored) (node:acc)
      where
        neighbors = if getDepth node < maxDepth 
                    then filter (((flip S.notMember) explored) . nodeState) $ getChildren node
                    else []

{-
    *** TODO ***

    Explorează în adâncime spațiul stărilor, utilizând adâncire iterativă,
    pentru determinarea primei stări finale întâlnite.

    Întoarce o perche între nodul cu prima stare finală întâlnită și numărul
    de stări nefinale vizitate până în acel moment.
-}

iterativeDeepening :: (ProblemState s a, Ord s)
    => Node s a         -- Nodul stării inițiale
    -> (Node s a, Int)  -- (Nod cu prima stare finală,
                        --  număr de stări nefinale vizitate)
iterativeDeepening initial = (goal, length states)
  where
    (states, goal : _) = break (isGoal . nodeState) $ concatMap (limitedDfs initial) [0..]

{-
    *** TODO ***

    Pornind de la un nod, reface calea către nodul inițial, urmând legăturile
    către părinți.

    Întoarce o listă de perechi (acțiune, stare), care se încheie în starea
    finală, dar care EXCLUDE starea inițială.
-}

extractPath :: Node s a -> [(a, s)]
extractPath nodes = foldl (\ acc n -> (getAction n, nodeState n):acc) []
    (takeWhile (isJust . getParent) $ iterate (fromJust . getParent) nodes)

{-
    *** TODO ***

    Pornind de la o stare inițială, se folosește de iterativeDeepening pentru 
    a găsi prima stare finală și reface calea către nodul inițial folosind 
    extractPath. 
  
    Întoarce o listă de perechi (acțiune, stare), care se încheie în starea
    finală, dar care EXCLUDE starea inițială.
-}

solve :: (ProblemState s a, Ord s)
      => s          -- Starea inițială de la care se pornește 
      -> Bool       -- Dacă să folosească sau nu euristica dată 
      -> [(a, s)]   -- Lista perechilor
solve initial useHeuristic = extractPath goal
    where 
        stateSpace = createStateSpace initial
        (goal, _) = iterativeDeepening $ if useHeuristic then orderStateSpace stateSpace else stateSpace

{-
    Poate fi utilizată pentru afișarea fiecărui element al unei liste
    pe o linie separată.
-}

printSpacedList :: Show a => [a] -> IO ()
printSpacedList = mapM_ (\a -> print a >> putStrLn (replicate 20 '*'))