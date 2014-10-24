f :: Int -> String -> [String] -> [String]

splitIntoChunks n s ss  | length s <= n = (take n s):ss
                        | otherwise     = (take n s):(f n (drop n s) ss)

{- inPiecesOf Int -> String -> [String] -}
{- inPiecesOf n = foldr (\arr str -> (take n str):arr) [] -}




{- f :: Int -> String -> (String, String) -}
