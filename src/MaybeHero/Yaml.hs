{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverlappingInstances #-}

module MaybeHero.Yaml (
  (££),
  (££!),
  (£!),
  (£),
  Parseable,
  ParseError,
  parseYaml,
  parseFromFile)
where

import qualified Data.Yaml.YamlLight as Y
import qualified Data.Map as M
import qualified Data.ByteString.Char8 as C8
import qualified Data.Maybe as Maybe
import qualified Data.Either as Either
import Control.Monad ((>>=), sequence)

type ParseError = String
type Yaml = Y.YamlLight
type YamlMap = M.Map Y.YamlLight Y.YamlLight
type YamlParser a = (Yaml -> Either ParseError a)

yamlKey :: String -> Yaml
yamlKey = Y.YStr . C8.pack

parseString :: YamlParser String
parseString y = Maybe.maybe (Left ("Failure to parse string: [" ++ show y ++ "]")) (Right . C8.unpack) $ Y.unStr y

parseSeq :: YamlParser a -> YamlParser [a]
parseSeq p y =
  Maybe.maybe (Left ("Failure to parse sequence: [" ++ show y ++ "]")) (sequence .(map p)) $ Y.unSeq y

parseMap :: YamlParser YamlMap
parseMap y = Maybe.maybe (Left ("Failure to parse map for yaml: " ++ show y)) Right $ Y.unMap y

parseMapValue :: (Parseable a) => Yaml -> String -> Either ParseError a
parseMapValue y s = do
  m <- parseMap y
  Maybe.maybe (Left "FAILED") parseYaml $ M.lookup (yamlKey s) m

parseMapValueOptional :: (Parseable a) => Yaml -> a -> String -> Either ParseError a
parseMapValueOptional y d s = do
  m <- parseMap y
  Maybe.maybe (Right d) parseYaml $ M.lookup (yamlKey s) m

parseMapEntry :: (Ord a) => YamlParser a -> YamlParser b -> (Yaml, Yaml) -> Either ParseError (a, b)
parseMapEntry kp vp (ky, vy) = do
  k <- kp ky
  v <- vp vy
  return (k, v)

parseMapKeysAndValues :: (Ord a) => YamlParser a -> YamlParser b -> YamlParser (M.Map a b)
parseMapKeysAndValues kp vp y = do
  m <- parseMap y
  l <- sequence $ map (parseMapEntry kp vp) $ M.toList m
  return (M.fromList l)

class Parseable a where
  parseYaml :: Yaml -> Either ParseError a

instance Parseable String where
  parseYaml = parseString

instance (Parseable a) => Parseable [a] where
  parseYaml = parseSeq parseYaml

instance (Ord a, Parseable a, Parseable b) => Parseable (M.Map a b) where
  parseYaml = parseMapKeysAndValues parseYaml parseYaml

(££) :: Parseable a => (a -> b) -> (String, Yaml) -> Either ParseError b
(££) f (s, y) = f <$> parseMapValue y s

(££!) :: Parseable a => (a -> b) -> (String, a, Yaml) -> Either ParseError b
(££!) f (s, d, y) = f <$> parseMapValueOptional y d s

(£) :: Parseable a => Either ParseError (a -> b) -> (String, Yaml) -> Either ParseError b
(£) f (s, y) = f <*> parseMapValue y s

(£!) :: Parseable a => Either ParseError (a -> b) -> (String, a, Yaml) -> Either ParseError b
(£!) f (s, d, y) = f <*> parseMapValueOptional y d s

parseFromFile :: (Parseable a) => String -> IO (Either ParseError a)
parseFromFile s = do
  f <- readFile s
  y <- Y.parseYaml f
  return (parseYaml y)
