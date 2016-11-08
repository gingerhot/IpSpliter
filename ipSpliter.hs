--
-- by idegorepl<at>gmail.com
-- run "runghc ipSpliter.hs" in shell
-- after making sure you've installed a Haskell runtime like GHC
--

import Data.List (intercalate, tails)

ipSpliter :: String -> [String]
ipSpliter s = [ intercalate "." x | x <- allIps, validCheck x ]
    where validCheck = all (\x -> (read x :: Int) < 256 && checkHeadZero x)
          checkHeadZero x = if length x > 1 then head (map (:[]) x) /= "0" else True
          allSplitPos = combinations 3 [1..length s - 1]
          allIps = map (\x -> splitIp x s) allSplitPos

splitIp :: [Int] -> String -> [String]
splitIp [] s = [s]
splitIp (x:xs) s = m : splitIp (map (subtract x) xs) n
    where (m,n) = splitAt x s

combinations :: Int -> [Int] -> [[Int]]
combinations 0 _ = [[]]
combinations n xs = [ y:ys | y:xs' <- tails xs, ys <- combinations (n-1) xs']

main :: IO ()
main = do
    let s = "2502501135"
    putStrLn $ show $ ipSpliter s
