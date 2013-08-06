module HoS.Network.Util (
    trimStr,
    splitAtEmptyLine,
    linesCRLF,
    httpDate,
    fromHttpDate,
) where

import Data.Maybe
import Data.List
import Data.DateTime
import Network.Socket



httpDate :: DateTime -> String
httpDate time = formatDateTime rfc1123DateFormat time
    where rfc1123DateFormat = "%a, %d %b %Y %H:%M:%S GMT"

fromHttpDate :: String -> DateTime
fromHttpDate time = fromJust $ parseDateTime rfc1123DateFormat time
    where rfc1123DateFormat = "%a, %d %b %Y %H:%M:%S GMT"

trimStr :: String -> String
trimStr str = reverse $ trim' $ reverse (trim' str)
    where
        cs = [' ', '\t', '\n', '\r', '\0', '\x0B']
        trim' str' = dropWhile (\c -> elem c cs) str'

--
-- * RFC 2046 CRLF
--

-- | Drop everything up to and including the first CRLF.
dropLine :: String -> Maybe String
dropLine s = fmap snd (splitAtCRLF s)

-- | Split a string at the first empty line. The CRLF (if any) before the
--   empty line is included in the first result. The CRLF after the
--   empty line is not included in the result.
--   'Nothing' is returned if there is no empty line.
splitAtEmptyLine :: String -> Maybe (String, String)
splitAtEmptyLine s | startsWithCRLF s = Just ([], dropCRLF s)
                   | otherwise = spl 0
  where
  spl i = case findCRLF (drop i s) of
              Nothing -> Nothing
              Just (j,l) | startsWithCRLF s2 -> Just (s1, dropCRLF s2)
                         | otherwise -> spl (i+j+l)
                where (s1,s2) = splitAt (i+j+l) s

-- | Split a string at the first CRLF. The CRLF is not included
--   in any of the returned strings.
splitAtCRLF :: String -- ^ String to split.
            -> Maybe (String,String)
            -- ^  Returns 'Nothing' if there is no CRLF.
splitAtCRLF s = case findCRLF s of
                  Nothing -> Nothing
                  Just (i,l) -> Just (s1, drop l s2)
                      where (s1,s2) = splitAt i s
-- | Lines at CRLF
linesCRLF :: String -- ^ String to line
            -> [String]
linesCRLF s = case findCRLF s of
                Nothing -> [s]
                Just (i,l) -> line : linesCRLF (drop l restStr)
                    where (line, restStr) = splitAt i s


-- | Like 'splitAtCRLF', but if no CRLF is found, the first
--   result is the argument string, and the second result is empty.
splitAtCRLF_ :: String -> (String,String)
splitAtCRLF_ s = fromMaybe (s, []) (splitAtCRLF s)

-- | Get the index and length of the first CRLF, if any.
findCRLF :: String -- ^ String to split.
         -> Maybe (Int,Int)
findCRLF s =
    case findCRorLF s of
              Nothing -> Nothing
              Just j | null (drop (j+1) s) -> Just (j,1)
              Just j -> case (s !! j, s !! (j+1)) of
                           ('\n','\r') -> Just (j,2)
                           ('\r','\n') -> Just (j,2)
                           _           -> Just (j,1)

findCRorLF :: String -> Maybe Int
findCRorLF s = findIndex (\c -> c == '\n' || c == '\r') s

startsWithCRLF :: String -> Bool
startsWithCRLF s = not (null s) && (c == '\n' || c == '\r')
  where c = s !! 0

-- | Drop an initial CRLF, if any. If the string is empty,
--   nothing is done. If the string does not start with CRLF,
--   the first character is dropped.
dropCRLF :: String -> String
dropCRLF s | null s = []
           | null (drop 1 s) = []
           | c0 == '\n' && c1 == '\r' = drop 2 s
           | c0 == '\r' && c1 == '\n' = drop 2 s
           | otherwise = drop 1 s
  where c0 = s !! 0
        c1 = s !! 1
