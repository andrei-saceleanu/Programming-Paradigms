import Data.List
import Data.Char


type Matrix=[[Integer]]

{-1-}
mysplit::Char->[Char]->[[Char]]
mysplit sep l = auxsplit sep l [] [] where
		auxsplit sep l acc_string acc_list
			|null l = acc_list ++ [acc_string]
			|head l == sep = auxsplit sep (tail l) [] (acc_list ++ [acc_string])
			|otherwise = auxsplit sep (tail l) (acc_string++[head l]) acc_list




parsem::String->Matrix
parsem s = map  (map (\el -> read el::Integer) ) (map (mysplit ' ') (mysplit '\n' s))


{-2-}
toString::Matrix->String
toString mat = intercalate "\n" (map unwords (map (map (\el -> show el::String)) mat))

{-3-}
displaymat =putStrLn . toString

{-4-}
vprod::Integer->Matrix->Matrix
vprod v mat = [[v*el|el<-x]|x<-mat]

{-5-}
hjoin::Matrix->Matrix->Matrix
hjoin m1 m2=zipWith (++) m1 m2

{-6-}
vjoin::Matrix->Matrix->Matrix
vjoin m1 m2=m1++m2

{-7-}
msum::Matrix->Matrix->Matrix
msum m1 m2 = [[((m1!!i)!!j)+((m2!!i)!!j)|j<-[0..((length (m1!!0))-1)]]|i<-[0..((length m1)-1)]]

{-8-}
tr::Matrix->Matrix
tr mat = [[(mat!!j)!!i|j<-[0..((length (mat!!0))-1)]]|i<-[0..((length mat)-1)]]
tr2::Matrix->Matrix
tr2 mat = (map head mat):(if null (tail (mat!!0)) then [] else tr2 (map tail mat))

{-9-}
prodline::[Integer]->[Integer]->Integer
prodline a b = sum [(a!!i)*(b!!i)|i<-[0..((length a)-1)]]
prodline2::[Integer]->[Integer]->Integer
prodline2 a b = foldl (\acc el -> acc+(fst el)*(snd el)) 0 (zip a b)

mline::[Integer]->Matrix->[Integer]
mline l m = [prodline l col |col<-t] where t=tr m


mprod::Matrix->Matrix->Matrix
mprod m1 m2=[mline line m2|line<-m1]

type Image = [String]

{-10-}

l1="        ***** **            ***** **    "
l2="     ******  ****        ******  ****   "
l3="    **   *  *  ***      **   *  *  ***  "
l4="   *    *  *    ***    *    *  *    *** "
l5="       *  *      **        *  *      ** "
l6="      ** **      **       ** **      ** "
l7="      ** **      **       ** **      ** "
l8="    **** **      *      **** **      *  "
l9="   * *** **     *      * *** **     *   "
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
 
logo = [l1,l2,l3,l4,l5,l6,l7,l8,l9,l10,l11,l12,l13,l14,l15,l16,l17,l18,l19]

toStringImg::Image->String
toStringImg img = intercalate "\n" img

displaym= putStrLn . toStringImg

{-11-}

flipH::Image->Image
flipH img =map reverse img

{-12-}
flipV::Image->Image
flipV img = reverse img

{-13-}
f1::Image->Int->String
f1 img index =[line!!index|line<-img]

tr_img::Image->Image
tr_img img = [f1 img i|i<-[0..((length (img!!0))-1)]]

rotate90r::Image->Image
rotate90r img = tr_img (flipV img)

{-14-}
rotate90l::Image->Image
rotate90l img = tr_img (flipH img)

{-15-}
diamond::Int->Image
diamond nr=part ++[(take (2*nr-1) (cycle "*"))] ++ (flipV part) where part=[(take (nr-x) (cycle " "))++(take (2*x-1) (cycle "*"))++(take (nr-x) (cycle " "))|x<-[1..(nr-1)]]

{-16-}
overlay::Image->Image->Image
overlay img1 img2 = map (map (\el -> if ((fst el)=='*')&&((snd el)=='*') then '*' else ' ')) (map (\el -> zip (fst el) (snd el)) (zip img1 img2))

{-17-}
above::Matrix->Matrix
above mat = foldl (\acc curr->acc++[[curr!!i|i<-[0..(length acc)]]++[0|i<-[((length acc)+1)..((length (mat!!0))-1)]]]) [] mat

{-18-}
below::Matrix->Matrix
below mat = foldl (\acc curr->acc++[[0|i<-[0..(length acc)-1]]++[curr!!i|i<-[(length acc)..((length (mat!!0))-1)]]]) [] mat

{-19-}
above_v2::Matrix->Matrix
above_v2 mat = foldl (\acc curr->acc++[(take ((length acc)+1) curr)++(map (fromIntegral . digitToInt) (take ((length (mat!!0))-(length acc)-1) (cycle "0")))]) [] mat

{-20-}
diag::Matrix->[Integer]
diag mat = [(mat!!i)!!i|i<-[0..(length mat)-1]]

{-21-}
remove_xy::Matrix->Int->Int->Matrix
remove_xy mat x y = [[line!!j|j<-[0..(length (mat!!0))-1],j/=y]|line<-line_removed]where line_removed=[mat!!i|i<-[0..(length mat)-1],i/=x]

determinant::Matrix->Int
determinant mat
	|length mat==2 = fromIntegral (((mat!!0)!!0)*((mat!!1)!!1)-((mat!!0)!!1)*((mat!!1)!!0))
	|otherwise = sum [(-1)^y*(fromIntegral ((mat!!0)!!y))*determinant (remove_xy mat 0 y) |y<-[0..(length (mat!!0))-1]]

