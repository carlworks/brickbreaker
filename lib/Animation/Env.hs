module Animation.Env
  ( Env(..)
  , initGameEnv
  ) where

data Env = Env
  { column    :: Int
  , row       :: Int
  , boardSize :: Int
  }
  deriving Show

initGameEnv = Env 30 16 5