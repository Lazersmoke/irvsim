{-# LANGUAGE LambdaCase #-}
module IrvSim where

import Data.List
import Data.Ord
import Data.Ratio
import Text.Parsec
import Text.Parsec.String
import Control.Monad.Writer
import Control.Monad.State

type Log a = StateT Integer (Writer String) a

runLogger :: Log a -> (a,String)
runLogger l = runWriter $ evalStateT l 0

write :: String -> Log ()
write s = do
  i <- fromIntegral <$> get
  tell (replicate i ' ' ++ s ++ "\n")

indent :: Integer -> Log a -> Log a
indent i l = do
  modify (+i)
  a <- l
  modify (subtract i)
  return a

data Election = Election [Candidate] [Ballot] deriving Eq
data Ballot = Ballot [Candidate] deriving Eq
newtype Candidate = Candidate String deriving Eq

instance Show Candidate where
  show (Candidate s) = s

instance Show Ballot where
  show (Ballot []) = "Empty Ballot"
  show (Ballot cs) = ("Ballot: "++) . intercalate ", " . map show $ cs

instance Show Election where
  show (Election cs []) = "Candidates were: " ++ intercalate ", " (map show cs) ++ "No votes in the election"
  show (Election cs bs) = "Candidates were: " ++ intercalate ", " (map show cs) ++ "\n  " ++ intercalate "\n  " (map show bs)

writeElection :: Election -> Log ()
writeElection (Election [] _) = do
  write "No Candidates Remain!"
  write "You should check that the file parsed correctly"
writeElection (Election cs []) = do
  write "No votes remaining"
  write "Remaining Candidates:"
  write $ intercalate ", " (map show cs)
writeElection (Election cs bs) = do
  write "Remaining Candidates:"
  indent 2 . write $ intercalate ", " (map show cs)
  write "Tallies for Candidates:"
  indent 2 $ forM_ cs fancyWrite
  where
    fancyWrite c = 
      write $ show c ++ ": " ++ show (tally bs c)
      --write $ replicate (fromIntegral $ tally bs c) '*'

runElection :: String -> IO (Maybe Candidate)
runElection fn =
  parseElection fn >>= \case
    Left e -> print e *> pure Nothing
    Right e -> do
      let (mc,l) = runLogger (resolveElection e)
      putStrLn l
      pure mc

parseElection :: String -> IO (Either ParseError Election)
parseElection = parseFromFile election

lexeme :: Parser a -> Parser a
lexeme = (<* many (char ' '))

lexString :: String -> Parser String
lexString = lexeme . string

election :: Parser Election
election = Election <$> candidate `sepBy` lexString "|" <*> ballot `endBy1` lexString "\n"

ballot :: Parser Ballot
ballot = Ballot <$> candidate `sepBy` lexString ","

candidate :: Parser Candidate
candidate = Candidate <$> lexeme (many1 (alphaNum <|> oneOf ['_','.']))

resolveElection :: Election -> Log (Maybe Candidate)
-- No ballots, no one wins
resolveElection (Election _ []) = pure Nothing
-- If there are ballots, then
resolveElection e@(Election cs bs) = do
  write "Resolving Election:"
  indent 2 $ writeElection e
  -- If anyone has more than half the votes then they win
  if percent topVoted > (1%2)
    -- Last is the highest percent
    then do
      write (show topVoted ++ " won because they have the highest percent!")
      pure $ Just topVoted
    -- Otherwise, eliminate the lowest voted person and run a new election
    -- Write a newline to seperate elections
    else resolveElection =<< eliminate bottomVoted e <* write ""
  where
    total = sum (map (tally bs) cs)
    percent c = tally bs c % total
    sorted = sortBy (comparing percent) cs
    topVoted = last sorted
    bottomVoted = head sorted

tally :: [Ballot] -> Candidate -> Integer
-- If there is a non-empty ballot that matches the candidate, add one
tally (Ballot (x:_):bs) c = (if c == x then (+1) else id) $ tally bs c
-- If there is an empty ballot, ignore it
tally (Ballot []:bs) c = tally bs c
-- If we are out of ballots, then no votes
tally [] _ = 0

eliminate :: Candidate -> Election -> Log Election
eliminate c (Election cs bs) = 
  Election
    <$> pure (delete c cs) 
    <*> mapM moveVote bs <* write ("Eliminating " ++ show c ++ " from the election")
  where
    -- There is somewhere to move this vote to
    moveVote (Ballot (x:xs)) = 
      if c == x 
        then Ballot <$> pure xs <*
          case xs of 
            (y:_) -> write ("Moving vote from " ++ show c ++ " to " ++ show y) 
            [] -> write ("Vote for " ++ show c ++ " has no further candidate to move to, and is eliminated.") 
        else Ballot <$> pure (x:xs)
    moveVote (Ballot []) = pure $ Ballot []
