module Types (
    Board, Regions, Region, Cell
) where

type Board = [[Int]]
type Cell = (Int, Int)
type Region = [Cell]
type Regions = [(Int, Region)]