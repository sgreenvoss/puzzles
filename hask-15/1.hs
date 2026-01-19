santaClimb xs = [if x == '(' then 1 else -1 | x <- xs]

findPos :: [Int] -> Int -> Int
findPos xs pos 
	| head xs == -1 = pos
	| otherwise = findPos (tail xs) (pos+1)

main :: IO ()
main = do
	contents <- readFile "inputs/in-1.txt"
	let actions = santaClimb contents
	putStr "part 1: " 
	print (sum actions)

	let prefix = scanl1 (+) actions
	putStr "part 2: "
	print (findPos prefix 1)