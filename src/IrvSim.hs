module IrvSim where

import Data.List
import Data.Ord
import Data.Ratio
import Text.Parsec
import Text.Parsec.String

data Election = Election [Candidate] [Ballot] deriving (Eq,Show)
data Ballot = Ballot [Candidate] deriving (Eq,Show)
newtype Candidate = Candidate String deriving (Eq,Show)

test :: String -> IO (Maybe Candidate)
test fp = do
  Right e <- parseFromFile election fp
  return $ resolveElection e

lexeme :: Parser a -> Parser a
lexeme = (<* many (char ' '))

lexString :: String -> Parser String
lexString = lexeme . string

election :: Parser Election
election = Election 
  <$> candidate `sepBy1` lexString "," <* lexString "\n"
  <*> ballot `sepBy1` lexString "\n"

ballot :: Parser Ballot
ballot = Ballot <$> candidate `sepBy` lexString ","

candidate :: Parser Candidate
candidate = Candidate <$> lexeme (many1 alphaNum)

resolveElection :: Election -> Maybe Candidate
-- No candidates, no one wins
resolveElection (Election [] _) = Nothing
-- Only one candidate, they win
resolveElection (Election [c] _) = Just c
-- At least two candidates
resolveElection e@(Election cs bs) =
  -- If anyone has >50% then they win
  if any (> 0.5) percents
    -- Last is the highest percent
    then Just $ last sorted
    -- otherwise, eliminate the lowest person
    else resolveElection $ eliminate (head sorted) e
  where
    total = sum (map (tally bs) cs)
    percents = map (\c -> tally bs c % total) cs
    sorted = map fst . sortBy (comparing snd) $ zip cs percents

tally :: [Ballot] -> Candidate -> Integer
-- If there is a non-empty ballot that matches the candidate, add one
tally (Ballot (x:_):bs) c = (if c == x then (+1) else id) $ tally bs c
-- If there is an empty ballot, ignore it
tally (Ballot []:bs) c = tally bs c
-- If we are out of ballots, then no votes
tally [] _ = 0

eliminate :: Candidate -> Election -> Election
eliminate c (Election cs bs) = Election (filter (/=c) cs) (map moveVote bs)
  where
    moveVote (Ballot (x:xs)) = Ballot $ if c == x then xs else x:xs
    moveVote (Ballot []) = Ballot []
