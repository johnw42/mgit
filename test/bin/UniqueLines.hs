#! /usr/bin/env runhaskell
-- A program simliar to uniq(1), except that it can filter out
-- duplicate lines even if they are not adjacent.  Has options to keep
-- either the ifst or last occurrence of each line, and may optionally
-- split and join lines with a delimiter, making it suitable for
-- parsing $PATH values, etc.
module Main (main) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import Data.Foldable (foldl', toList)
import Data.Sequence ((<|), (|>))
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import Data.List (intercalate)
import Data.List.Split (splitOn)
import Options.Applicative (
  (<>), (<$>), (<*>), Parser,
  execParser, help, helper, idm, info, long, metavar,
  optional, short, strOption, switch)

data Opts = Opts {
  lastOpt :: Bool,
  joinOpt :: Maybe String,
  splitOpt :: Maybe String
  }

parser :: Parser Opts
parser =
  Opts
  <$> (switch $
       short 'l' <>
       long "keep-last" <>
       help "keep the last of each set of unique lines")
  <*> (optional $ strOption $
       short 'j' <>
       long "join" <>
       metavar "<sep>" <>
       help "join output lines using <sep>")
  <*> (optional $ strOption $
       short 's' <>
       long "split" <>
       metavar "<sep>" <>
       help "split until lines at <sep>")

main = do
  opts <- execParser (info (helper <*> parser) idm)
  C.interact $ process opts

process :: Opts -> B.ByteString -> B.ByteString
process opts = joinLines . filterLines . splitLines where
  joinLines = C.unlines . maybeJoin . toList . maybeReverse
  splitLines = maybeReverse . Seq.fromList . concatMap maybeSplit . C.lines
  filterLines = fst . foldl' filterOneLine (Seq.empty, Set.empty)
  filterOneLine (output, seen) line =
    if line `Set.notMember` seen
    then (output |> line, Set.insert line seen)
    else (output, seen)
  maybeReverse =
    if lastOpt opts
    then id
    else Seq.reverse
  maybeJoin lines =
    case joinOpt opts of
     Nothing -> lines
     Just delim -> [B.intercalate (C.pack delim) lines]
  maybeSplit line =
    case splitOpt opts of
     Nothing -> [line]
     Just delim -> map C.pack $ splitOn delim (C.unpack line)

-- Local Variables:
-- mode: haskell
-- End:
