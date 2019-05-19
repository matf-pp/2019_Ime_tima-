import Data.List
import Data.Ord
import Data.Function

prec 0 0 = [3,3,4,4] :: [Int]
prec 0 1 = [1,3,3,4]
prec 0 2 = [3,4,2,1]
prec 0 3 = [1,3,2,0]
prec 0 4 = [1,2,0,0]
prec 1 0 = [0,3,4,5]
prec 2 0 = [0,3,1,4]
prec 3 0 = [0,3,1,2]
prec 1 1 = [0,3,0,4]
prec 1 2 = [0,3,2,1]
prec 1 3 = [0,1,2,0]
prec 2 1 = [0,3,0,2]
prec 2 2 = [0,1,0,2]
prec _ _ = []

combos = [x | x <- mapM (const [0..5]) [0..3]] :: [[Int]]

toInt x = read x :: Int

count n xs = (length . filter (==n)) xs

yHelp l = [count x l| x<-[0..5]]

yellow l1 l2 = sum(zipWith min (yHelp l1) (yHelp l2))

red l1 l2 = count True (zipWith (==) l1 l2)

judge expec got = (r,y-r)
    where y = yellow expec got
          r = red expec got

nextComb oldCombs lastTry w = [x | x<- oldCombs, judge x lastTry == w]

nextCombs combos sol trry = nextComb combos trry w
    where w = judge sol trry

fitness combos comb = [length x | x <- [nextCombs combos sol comb | sol <- combos]]

sumsOf combos = [sum(fitness combos comb)| comb <- combos]

mini xs = minimumBy (comparing fst) (zip xs [0..])

makeZeMove combos = combos !! (snd (mini (sumsOf combos)))

