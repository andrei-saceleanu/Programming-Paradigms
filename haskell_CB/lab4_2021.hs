import Data.Char
import Dataset

type Matrix = [[Integer]]


--1.1
parsem ::String->Matrix
parsem s = map (map read) (map (splitBy ' ') (splitBy '\n' s))


splitBy::Char->String->[String]
splitBy ch s = foldr op [] s
		where op x acc
			  	|x==ch = []:acc
			  	|null acc = [[x]]
			  	|otherwise = (x:(head acc)):(tail acc)

concatWith::Char->[String]->String
concatWith ch l = init (foldr (\x acc -> x++[ch]++acc) "" l)  


--1.2
displayLine::[Integer]->String
displayLine l = foldr (\x acc -> x++" "++acc) "\n" (map show l)
toString::Matrix->String
toString  = (foldl (++) "").(map displayLine)

--1.3
displayMatrix = putStr . toString

--2.1
vprod::Integer->Matrix->Matrix
vprod v = map (map (*v))

--2.2
hjoin :: Matrix -> Matrix -> Matrix
hjoin = zipWith (++)

--2.3
vjoin :: Matrix -> Matrix -> Matrix
vjoin = (++)

--2.4
msum :: Matrix -> Matrix -> Matrix
msum = zipWith (zipWith (+))

--2.5
transpose :: [[a]] -> [[a]]
transpose ([]:_) = []
transpose m = (map head m):(transpose (map tail m))

--2.6
value::[Integer]->[Integer]->Integer
value lx cy = foldl (+) 0 (zipWith (*) lx cy)

line::[Integer]->Matrix->[Integer]
line l m = map (value l) m

mprod :: Matrix -> Matrix -> Matrix
mprod m1 m2 = map (\ln -> line ln (transpose m2)) m1

type Image = [String]

logo = [l1,l2,l3,l4,l5,l6,l7,l8,l9,l10,l11,l12,l13,l14,l15,l16,l17,l18,l19]
    where l1 ="        ***** **            ***** **    "
          l2 ="     ******  ****        ******  ****   "
          l3 ="    **   *  *  ***      **   *  *  ***  "
          l4 ="   *    *  *    ***    *    *  *    *** "
          l5 ="       *  *      **        *  *      ** "
          l6 ="      ** **      **       ** **      ** "
          l7 ="      ** **      **       ** **      ** "
          l8 ="    **** **      *      **** **      *  "
          l9 ="   * *** **     *      * *** **     *   "
          l10="      ** *******          ** *******    "
          l11="      ** ******           ** ******     "
          l12="      ** **               ** **         "
          l13="      ** **               ** **         "
          l14="      ** **               ** **         "
          l15=" **   ** **          **   ** **         "
          l16="***   *  *          ***   *  *          "
          l17=" ***    *            ***    *           "
          l18="  ******              ******            "
          l19="    ***                 ***             "

mask = [l1,l2,l3,l4,l5,l6,l7,l8,l9,l10,l11,l12,l13,l14,l15,l16,l17,l18,l19]
    where l1 ="                       *****************"
          l2 ="                       *****************"
          l3 ="                       *****************"
          l4 ="                       *****************"
          l5 ="                       *****************"
          l6 ="                       *****************"
          l7 ="                       *****************"
          l8 ="                       *****************"
          l9 ="                       *****************"
          l10="                       *****************"
          l11="                       *****************"
          l12="                       *****************"
          l13="                       *****************"
          l14="                       *****************"
          l15="                       *****************"
          l16="                       *****************"
          l17="                       *****************"
          l18="                       *****************"
          l19="                       *****************"

--3.1
toStringImg :: Image ->String
toStringImg = foldl ((++).(++"\n")) ""

displayImg = putStrLn . toStringImg

--3.2
my_reverse::[a]->[a]
my_reverse = foldl (\acc x -> x:acc) []

flipH::Image->Image
flipH = map reverse

--3.3
flipV::Image->Image
flipV = reverse

--3.4
rotate90r :: Image -> Image
rotate90r = flipH.transpose

--3.5
rotate90l :: Image -> Image
rotate90l = flipV.transpose

--3.6
invert :: Image -> Image
invert = map (map (\el -> if el==' ' then '*' else ' ')) 


--3.7
maskKeep :: Image -> Image -> Image
maskKeep = zipWith (zipWith (\el1 el2 -> if el1=='*' then el2 else ' '))

--3.8
maskDiscard :: Image -> Image -> Image
maskDiscard = zipWith (zipWith (\el1 el2 -> if el1=='*' then ' ' else el2))

--3.9
union :: Image -> Image -> Image
union = zipWith (zipWith (\el1 el2 -> if el1=='*'||el2=='*' then '*' else ' '))

--3.10
transformationSequence :: [Image -> Image] -> Image -> Image
transformationSequence l img = foldl (\acc x-> x acc) img l

seq1 = transformationSequence [invert,union mask ,rotate90r]

--4.1
above::Matrix->Matrix
above = foldl (\acc x->acc++[(op ((length acc)+1) x)]) []
	where op n l = foldl (\acc x-> if (length acc)>=n then acc++[0] else acc++[x]) [] l

--4.2
below::Matrix->Matrix
below = foldl (\acc x->acc++[(op (length acc) x)]) []
	where op n l = foldl (\acc x-> if (length acc)>=n then acc++[x] else acc++[0]) [] l


--4.3
above_v2::Matrix->Matrix
above_v2 = foldl (\acc x->acc++[(op ((length acc)+1) x)]) []
	where op n l = (take n l)++[ 0|i<-[1..(length l)-n]] 

--4.3
below_v2::Matrix->Matrix
below_v2 = foldl (\acc x->acc++[(op (length acc) x)]) []
	where op n l = [ 0|i<-[1..n]]++(drop n l) 

--4.4
diagonal::Matrix->Matrix
diagonal = foldl (\acc x -> acc++[(op (length acc) x)]) []
	where op n l = foldl (\acc x -> if (length acc)==n then acc++[x] else acc++[0]) [] l

--4.5
remove_xy::Int->Int->Matrix->Matrix
remove_xy x y mat = [[(mat!!i)!!j|j<-[0..((length (mat!!0))-1)],j/=y]|i<-[0..((length mat)-1)],i/=x]

determinant::Matrix->Integer
determinant mat 
	|length mat == 2 = ((mat!!0)!!0)*((mat!!1)!!1)-((mat!!0)!!1)*((mat!!1)!!0)
	|otherwise = sum [((-1)^y)*(determinant (remove_xy 0 y mat)) |y<-[0..((length (mat!!0))-1)]]







