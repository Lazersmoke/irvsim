{-# LANGUAGE LambdaCase #-}
module IrvSim where

import Data.List
import Data.Ord
import Data.Ratio
import qualified Data.Set as S
import Text.Parsec
import Text.Parsec.String
import Control.Monad.Writer

data Election = Election [Ballot] deriving Eq
data Ballot = Ballot [Candidate] deriving Eq
data Tally = Tally Candidate Integer deriving Eq
newtype Candidate = Candidate String deriving Eq

instance Ord Tally where
  (Tally _ a) <= (Tally _ b) = a <= b

instance Show Candidate where
  show (Candidate s) = s

instance Show Ballot where
  show (Ballot []) = "Empty Ballot"
  show (Ballot cs) = ("Ballot: "++) . intercalate ", " . map show $ cs

instance Show Election where
  show (Election []) = "No Votes in the Election"
  show (Election bs) = intercalate "\n" . map show $ bs

runElection :: String -> IO (Maybe Candidate)
runElection fn = do
  parseFromFile election fn >>= \case
    Left e -> print e *> pure Nothing
    Right e -> do
      let (mc,l) = runWriter (resolveElection e)
      putStrLn l
      pure mc

parseElection :: String -> IO (Either ParseError Election)
parseElection = parseFromFile election

lexeme :: Parser a -> Parser a
lexeme = (<* many (char ' '))

lexString :: String -> Parser String
lexString = lexeme . string

election :: Parser Election
election = Election <$> ballot `endBy1` lexString "\n"

ballot :: Parser Ballot
ballot = Ballot <$> candidate `sepBy` lexString ","

candidate :: Parser Candidate
candidate = Candidate <$> lexeme (many1 alphaNum)

resolveElection :: Election -> Writer String (Maybe Candidate)
-- No ballots, no one wins
resolveElection (Election []) = pure Nothing
-- If there are ballots, then
resolveElection e@(Election bs) = do
  tell ("Resolving Election:\n" ++ show e ++ "\n")
  tell ("Here are the tallies:\n" ++ (cs >>= fancyShow) ++ "\n") 
  -- If anyone has more than half the votes then they win
  if percent topVoted > (1%2)
    -- Last is the highest percent
    then do
      tell (show topVoted ++ " won because they have the highest percent!\n")
      pure $ Just topVoted
    -- Otherwise, eliminate the lowest voted person and run a new election
    else resolveElection =<< eliminate bottomVoted e
  where
    fancyShow c = show c ++ " " ++ replicate (fromIntegral $ tally bs c) '*' ++ "\n"
    cs = getCandidates bs
    total = sum (map (tally bs) cs)
    percent c = tally bs c % total
    sorted = sortBy (comparing percent) cs
    topVoted = last sorted
    bottomVoted = head sorted

getCandidates :: [Ballot] -> S.Set Candidate
-- If there are any ballots at all, then include their candidates
getCandidates ((Ballot cs):bs) = S.union (S.fromList cs) (getCandidates bs)
-- If there are no ballots, then there can be no candidates
getCandidates [] = S.empty

tally :: [Ballot] -> Candidate -> Tally
-- If there is a non-empty ballot that matches the candidate, add one
tally (Ballot (x:_):bs) c = (if c == x then (+1) else id) $ tally bs c
-- If there is an empty ballot, ignore it
tally (Ballot []:bs) c = tally bs c
-- If we are out of ballots, then no votes
tally [] _ = 0

eliminate :: Candidate -> Election -> Writer String Election
eliminate c (Election bs) = do
  tell ("Eliminating " ++ show c ++ " from the election\n")
  pure $ Election (map moveVote bs)
  where
    moveVote (Ballot (x:xs)) = Ballot $ if c == x then xs else x:xs
    moveVote (Ballot []) = Ballot []
