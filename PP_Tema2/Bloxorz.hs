{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE EmptyDataDecls, MultiParamTypeClasses,
             TypeSynonymInstances, FlexibleInstances,
             InstanceSigs #-}


module Bloxorz where

import ProblemState

import qualified Data.Array as A
{-
    Caracterele ce vor fi printate pentru fiecare tip de obiect din joc
    Puteți înlocui aceste caractere cu orice, în afară de '\n'.
-}

hardTile :: Char
hardTile = '▒'

softTile :: Char
softTile = '='

block :: Char
block = '▓'

switch :: Char
switch = '±'

emptySpace :: Char
emptySpace = ' '

winningTile :: Char
winningTile = '*'

{-
    Sinonim de tip de date pentru reprezetarea unei perechi (int, int)
    care va reține coordonatele de pe tabla jocului
-}

type Position = (Int, Int)

{-
    Direcțiile în care se poate mișcă blocul de pe tablă
-}

data Directions = North | South | West | East
    deriving (Show, Eq, Ord)

{-
    *** TODO ***

    Tip de date care va reprezenta plăcile care alcătuiesc harta și switch-urile
-}

data Cell = Hard | Soft | Block | Switch | Empty | Winning
    deriving (Eq, Ord)

instance Show Cell
    where
        show Hard = hardTile:""
        show Soft = softTile:""
        show Block = block:""
        show Switch = switch:""
        show Empty = emptySpace:""
        show Winning = winningTile:""

{-
    *** TODO ***

    Tip de date pentru reprezentarea nivelului curent
-}

data Level = LevelC { 
                      grid :: (A.Array Position Cell),
                      blockPos :: [(Position, Cell)],
                      switches :: [(Position, Bool, [Position])] }
    deriving (Eq, Ord)

{-
    *** Opțional ***

    Dacă aveți nevoie de o funcționalitate particulară,
    instantiati explicit clasele Eq și Ord pentru Level.
    În cazul acesta, eliminați deriving (Eq, Ord) din Level.
-}

-- instance Eq Level where
--     (==) = undefined

-- instance Ord Level where
--     compare = undefined

{-
    *** TODO ***

    Instantiati Level pe Show.

    Atenție! String-ul returnat va fi urmat și precedat de un rând nou.
    În cazul în care jocul este câștigat, la sfârșitul stringului se va mai
    concatena mesajul "Congrats! You won!\n".
    În cazul în care jocul este pierdut, se va mai concatena "Game Over\n".
-}

instance Show Level
    where
        show level@(LevelC mat blockP@((poz, _):_) _) =
                        "\n" ++
                        unlines [concat [show (elemAt x y) | y <- [0..highY]] | x <- [0..highX]] ++
                        gameStatus
            where 
                (_, (highX, highY)) = A.bounds mat 
                elemAt x y = (mat A.// blockP) A.! (x, y)
                gameStatus
                    | (length blockP == 1) && (mat A.! poz) == Winning = "Congrats! You won!\n"
                    | continueGame level = ""
                    | otherwise = "Game Over\n"
        show _ = ""
{-
    *** TODO ***

    Primește coordonatele colțului din dreapta jos a hârtii și poziția inițială a blocului.
    Întoarce un obiect de tip Level gol.
    Implicit, colțul din stânga sus este (0, 0).
-}

emptyLevel :: Position -> Position -> Level
emptyLevel size@(highX, highY) initialBlockPos = LevelC mat blockP []
        where
            mat = A.array ((0, 0), size) $ [((x, y), Empty) | x <- [0..highX], y <- [0..highY]] ++ [(initialBlockPos, Hard)]
            blockP = [(initialBlockPos, Block)]

{-
    *** TODO ***

    Adaugă o celulă de tip Tile în nivelul curent.
    Parametrul char descrie tipul de tile adăugat:
        'H' pentru tile hard
        'S' pentru tile soft
        'W' pentru winning tile
-}

addTile :: Char -> Position -> Level -> Level
addTile 'H' position level@(LevelC mat _ _) = level { grid = mat A.// [(position, Hard)] }
addTile 'S' position level@(LevelC mat _ _) = level { grid = mat A.// [(position, Soft)] }
addTile 'W' position level@(LevelC mat _ _) = level { grid = mat A.// [(position, Winning)] }
addTile _ _ level = level

{-
    *** TODO ***

    Adaugă o celulă de tip Swtich în nivelul curent.
    Va primi poziția acestuia și o listă de Position
    ce vor desemna pozițiile în care vor apărea sau
    dispărea Hard Cells în momentul activării/dezactivării
    switch-ului.
-}

addSwitch :: Position -> [Position] -> Level -> Level
addSwitch position positionsList level@(LevelC mat _ switchesP) = level { grid = newMat,
                                                                          switches = newSwitchesP }
            where
                newMat = mat A.// [(position, Switch)]
                newSwitchesP = switchesP ++ [(position, False, positionsList)]

{-
    === MOVEMENT ===
-}

{-
    *** TODO ***

    Activate va verifica dacă mutarea blocului va activa o mecanică specifică.
    În funcție de mecanica activată, vor avea loc modificări pe hartă.
-}


activate :: Cell  -> Level -> Level
activate Switch level@(LevelC mat blockP switchesP) = level { grid = newMat,
                                                              switches = newSwitches }
    where
        positionsOfBlock = map fst blockP
        (swPos, swValue, swCellList):[] = filter (\(pos, _, _) -> elem pos positionsOfBlock) switchesP
        cellType = if swValue then Empty else Hard
        newMat = mat A.// [(pos, cellType) | pos <- swCellList]
        newSwitches = map (\(p, v, l) -> if (swPos == p) then (p, not v, l) else (p, v, l)) switchesP
activate _ level = level

{-
    *** TODO ***

    Mișcarea blocului în una din cele 4 direcții
    Hint: Dacă jocul este deja câștigat sau pierdut, puteți lăsa nivelul neschimbat.
-}

move :: Directions -> Level -> Level
move dir level
    | not $ continueGame level = level 
    | otherwise = activate (if isBlockOnSwitch newLevel then Switch else Hard) newLevel
    where
        newLevel = doMove dir level
        isBlockOnSwitch (LevelC mat blockP _) = elem Switch $ map ((mat A.!).fst) blockP
        -- function to execute the move
        doMove :: Directions -> Level -> Level
        doMove dir level@(LevelC _ [((x, y), _)] _) = case dir of 
            North -> level { blockPos = [((x - 2, y), Block), ((x - 1, y), Block)] }
            South -> level { blockPos = [((x + 1, y), Block), ((x + 2, y), Block)] }
            West  -> level { blockPos = [((x, y - 2), Block), ((x, y - 1), Block)] }
            East  -> level { blockPos = [((x, y + 1), Block), ((x, y + 2), Block)] }
        doMove dir level@(LevelC _ ([((x1, y1), _), ((x2, y2), _)]) _) 
            | x1 == x2 = case dir of -- horizontal
                North -> level { blockPos = [((x1 - 1, y1), Block), ((x2 - 1, y2), Block)] }
                South -> level { blockPos = [((x1 + 1, y1), Block), ((x2 + 1, y2), Block)] }
                West  -> level { blockPos = [((min x1 x2, y1 - 1), Block)] } 
                East  -> level { blockPos = [((max x1 x2, y2 + 1), Block)] } 
            | y1 == y2 = case dir of -- vertical
                North -> level { blockPos = [((min x1 x2 - 1, y1), Block)] } 
                South -> level { blockPos = [((max x1 x2 + 1, y1), Block)] } 
                West  -> level { blockPos = [((x1, y1 - 1), Block), ((x2, y2 - 1), Block)] } 
                East  -> level { blockPos = [((x1, y1 + 1), Block), ((x2, y2 + 1), Block)] }
            | otherwise = level
        doMove _ _ = undefined

{-
    *** TODO ***

    Va returna True dacă jocul nu este nici câștigat, nici pierdut.
    Este folosită în cadrul Interactive.
-}

continueGame :: Level -> Bool
continueGame (LevelC mat [((x, y), _)] _) = case cell of 
    Empty -> False -- lose
    Soft -> False -- lose
    Winning -> False -- win
    _ -> True
    where 
        cell = mat A.! (x, y)
continueGame (LevelC mat [(poz1, _), (poz2, _)] _)
    | cell1 == Empty || cell2 == Empty = False -- lose
    | otherwise = True 
    where
        cell1 = mat A.! poz1
        cell2 = mat A.! poz2
continueGame _ = undefined

{-
    *** TODO ***

    Instanțiați clasa `ProblemState` pentru jocul nostru.

    Hint: Un level câștigat nu are succesori!
    De asemenea, puteți ignora succesorii care
    duc la pierderea unui level.
-}

instance ProblemState Level Directions where
    
    successors level =  filter (\ (_, lev) -> isValidSuccessor lev) neigh
        where 
            neigh = map (\ dir -> (dir, move dir level)) [North, South, East, West]
            -- winning or not lost
            isValidSuccessor :: Level -> Bool
            isValidSuccessor level@(LevelC mat [(poz, _)] _) = (mat A.! poz) == Winning || continueGame level
            isValidSuccessor level = continueGame level

    isGoal (LevelC mat ((poz, _):[]) _) = (mat A.! poz) == Winning
    isGoal _ = False       

    -- Doar petru BONUS
    heuristic (LevelC mat ((pos, _):_) _) = (euclidianDistance winningPos pos)
        where 
            [(winningPos, _)] = filter (\ (_, cell) -> cell == Winning) $ A.assocs mat
            -- distances
            -- manhattanDistance (a, b) (c, d) = abs (a - c) + abs (b - d)
            euclidianDistance (x1 , y1) (x2 , y2) = floor $ sqrt $ fromIntegral (dx * dx + dy * dy)
                where
                    dx = x1 - x2
                    dy = y1 - y2
    heuristic _ = undefined
