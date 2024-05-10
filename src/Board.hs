{-# LANGUAGE BlockArguments #-}
module Board where
import qualified Data.Vector as V
import Data.Char (toUpper)

import Components(Player(..), Piece(..), Move (Move), BoardPosition (BoardPosition))

import Data.List (maximumBy, minimumBy)
import Data.Ord (comparing)

data Position = PieceSquare { piece :: Piece, player :: Player }  | Outside | Empty

instance Show Position where
    show Outside    = "#"
    show Empty      = "."
    show (PieceSquare piece player) =
        let pieceChar = show piece
        in case player of
            Black -> pieceChar
            White -> map toUpper pieceChar

data Board = Board {
    board :: !(V.Vector Position),
    currentPlayer :: Player,
    enpassant :: Maybe BoardPosition,
    castle :: CastelInfo
}

chunksOf :: Int -> V.Vector Position -> [V.Vector Position]
chunksOf n v
  | V.null v  = []
  | otherwise = V.take n v : chunksOf n (V.drop n v)

instance Show Board where
    show b = let lines = chunksOf 10 (board b) in
        tail $ foldr (\l s -> s ++ foldl (\s l -> s ++ show l ++ " ") "\n" l) "" lines

data CastelInfo = CastleInfo {
    blackQueen :: Bool,
    blackKing :: Bool,
    whiteQueen :: Bool,
    whiteKing :: Bool
}

-- Returns a new board in the starting posistion
newBoard :: Board
newBoard = Board {
    board = V.fromList $ outsideRow ++ outsideRow ++
        pieceRow White ++ pawnRow White ++
        emptyRow ++ emptyRow ++ emptyRow ++ emptyRow ++
        pawnRow Black ++ pieceRow Black ++
        outsideRow ++ outsideRow,
    currentPlayer = White,
    enpassant = Nothing,
    castle = CastleInfo {
        whiteKing = True,
        whiteQueen = True,
        blackKing = True,
        blackQueen = True
    }
}

pieceOrder :: Player -> [Position]
pieceOrder player = let pieces = [Rook, Knight, Bishop, Queen, King, Bishop, Knight, Rook] in
    map (`PieceSquare` player) pieces

outsideRow :: [Position]
outsideRow = replicate 10 Outside

outsideColumn :: [Position] -> [Position]
outsideColumn p = [Outside] ++ p ++ [Outside]

emptyRow :: [Position]
emptyRow = outsideColumn $ replicate 8 Empty

pieceRow :: Player -> [Position]
pieceRow player = outsideColumn $ pieceOrder player

pawnRow :: Player -> [Position]
pawnRow player = outsideColumn $ replicate 8 ((`PieceSquare` player) Pawn)

-- Finds the best next move
bestMoveSearch :: Board -> Move
bestMoveSearch b = fst $ moveIter b 3

moveIter :: Board -> Int -> (Move, Float)
moveIter b 0 =
    let
        player = currentPlayer b
        moves = possibleMoves b
        movefloat = map (\m -> (m, evaluation (movePice m b))) moves
    in
        maximumBy (comparing snd) movefloat
moveIter b depth =
    let
        player = currentPlayer b
        moves = possibleMoves b
        movefloat = map (\m -> (m, snd $ moveIter (movePice m b) (depth-1))) $! moves
    in
        bestMove player movefloat

bestMove :: Player -> [(Move, Float)] -> (Move, Float)
bestMove Black = minimumBy (comparing snd)
bestMove White = maximumBy (comparing snd)

-- Evaluation of the board
evaluation :: Board -> Float
evaluation b = V.sum $ V.map positionEvaluation (board b)

playerEvaluation :: Player -> Float -> Float
playerEvaluation Black a = - a
playerEvaluation _ a = a

pieceValue:: Piece -> Float
pieceValue King = 200
pieceValue Queen = 9
pieceValue Rook = 5
pieceValue Bishop = 3
pieceValue Knight = 3
pieceValue Pawn = 1

positionEvaluation :: Position -> Float
positionEvaluation (PieceSquare piece player) = playerEvaluation player $ pieceValue piece
positionEvaluation _ = 0

-- Move piece
movePice ::  Move -> Board -> Board
movePice (Move f t pro) b  =
    let
        v = board b
        fIndex = boardPositionToIndex f
        tIndex = boardPositionToIndex t
        fPiece = v V.! fIndex
        tPiece = v V.! tIndex
    in
        b { board = v V.// [(fIndex, Empty), (tIndex, fPiece)], currentPlayer = nextPlayer (currentPlayer b) }

nextPlayer :: Player -> Player
nextPlayer White = Black
nextPlayer Black = White

enPassantNext :: Piece -> Int -> Int -> Maybe BoardPosition
enPassantNext Pawn from to = if abs (from - to) == 20 then Just (indexToBoardPosition to) else Nothing
enPassantNext _ _ _ = Nothing

boardPositionToIndex :: BoardPosition -> Int
boardPositionToIndex (BoardPosition file rank) = file + (rank + 1) * 10

-- Find leagal moves
possibleMoves :: Board -> [Move]
possibleMoves b = concatMap (possibleMoveAtIndex b) [file + rank | file <- [1..9], rank <- [20,30..100]]

possibleMoveAtIndex :: Board -> Int -> [Move]
possibleMoveAtIndex b index =
    case v V.! index of
        PieceSquare piece player -> if player == p then concatMap (convertToMove piece index) (possibleMovesAtIndex' (PieceSquare piece player) index b) else []
        _ -> []
    where
        v = board b
        p = currentPlayer b

addPromotion :: BoardPosition -> BoardPosition -> [Move]
addPromotion start end = map (Move start end . Just) picess
    where picess = [Queen, Rook, Bishop, Knight]

convertToMove :: Piece -> Int -> Int -> [Move]
convertToMove Pawn start end = let
        s = indexToBoardPosition start
        BoardPosition f r = indexToBoardPosition end
    in
        if r == 1 || r == 8 then addPromotion s (BoardPosition f r)
        else [Move s (BoardPosition f r) Nothing]
convertToMove _ start end = [Move (indexToBoardPosition start) (indexToBoardPosition end) Nothing]

indexToBoardPosition :: Int -> BoardPosition
indexToBoardPosition index = BoardPosition (index `mod` 10) ((index `div` 10) - 1)

playerDir :: Player -> Int -> Int
playerDir Black d = - d
playerDir White d = d

dirCoordToIndex :: (Int, Int) -> Int
dirCoordToIndex (file, rank) = file + rank*10

indexToDirCoord :: Int -> (Int, Int)
indexToDirCoord index = (index `mod` 10, index `div` 10)

-- Spessial moves
spessialMoves :: Board -> [Move]
spessialMoves b = enPassant b 

-- En passant
enPassant :: Board -> [Move]
enPassant b =
    case enpassant b of
        Just pos ->
            let
                forward = playerDir (currentPlayer b) 1
                index = boardPositionToIndex pos
                up = index + (10 * forward)
            in
                case board b V.! up of
                    Empty -> [Move (indexToBoardPosition up) (indexToBoardPosition p) Nothing | p <- [index + 1, index - 1], colorCheck p b]
        _ -> []
    where colorCheck index b = case board b V.! index of
            PieceSquare Pawn player -> player == currentPlayer b
            _ -> False

-- castleMove :: Board -> [Move]

-- inCheck :: Board -> Int -> Bool


-- "Normal" moves created based on peaces
possibleMovesAtIndex' :: Position -> Int -> Board -> [Int]
possibleMovesAtIndex' (PieceSquare Rook player) index b =
    let
        dir = map dirCoordToIndex [(1,0),(0,1),(-1,0),(0,-1)]
    in
        dir >>= (\d -> straightMove index d player b)
possibleMovesAtIndex' (PieceSquare Bishop player) index b =
    let
        dir = map dirCoordToIndex [(1,1),(-1,1),(-1,-1),(1,-1)]
    in
        dir >>= (\d -> straightMove index d player b)
possibleMovesAtIndex' (PieceSquare Queen player) index b =
    let
        dir = map dirCoordToIndex [(1,1),(-1,1),(-1,-1),(1,-1),(1,0),(0,1),(-1,0),(0,-1)]
    in
        dir >>= (\d -> straightMove index d player b)
possibleMovesAtIndex' (PieceSquare Knight player) index b =
    let
        dir = map dirCoordToIndex [(1,2),(-1,2),(-1,-2),(1,-2),(2,1),(-2,1),(-2,-1),(2,-1)]
    in
        dir >>= (\d -> oneMove index d player b)
possibleMovesAtIndex' (PieceSquare King player) index b =
    let
        dir = map dirCoordToIndex [(1,1),(-1,1),(-1,-1),(1,-1),(1,0),(0,1),(-1,0),(0,-1)]
    in
        dir >>= (\d -> oneMove index d player b)

possibleMovesAtIndex' (PieceSquare Pawn player) index b =
    let
        forward = playerDir player 1
    in
        (pawnForward index forward b ++ pawnCapture index forward b)

pawnForward :: Int -> Int -> Board -> [Int]
pawnForward index forward b =
    let
        newPos = index + forward * 10
        end = board b V.! newPos
        start = (index `div` 10 - 1) == (4 - 2 * forward)
    in
        case end of
            Empty -> newPos : (if start then pawnForward newPos forward b else [])
            _ -> []

pawnCapture :: Int -> Int -> Board -> [Int]
pawnCapture index forward b =
    let
        capture = [index + forward * 10 - 1, index + forward * 10 + 1]
    in
        concatMap (possible (board b) (currentPlayer b)) capture
    where
        possible v p newPos = case v V.! newPos of
            (PieceSquare pice player) -> ([newPos | player /= p])
            _ -> []

oneMove :: Int -> Int -> Player -> Board -> [Int]
oneMove index dir player b =
    case v V.! (index + dir) of
        Outside -> []
        PieceSquare _ p -> ([index + dir | p /= player])
        _ -> [index + dir]
    where
        v = board b

straightMove :: Int -> Int -> Player -> Board -> [Int]
straightMove index dir player b =
    case v V.! (index + dir) of
        Outside -> []
        PieceSquare _ p -> ([index + dir | p /= player])
        _ -> (index + dir): straightMove (index + dir) dir player b
    where
        v = board b
