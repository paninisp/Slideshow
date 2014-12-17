module Example where

import SlideShow

slide1 :: Slide
slide1 = Slide [(Text "Imperative Languages" (12,"Cambria",red),1),(Text "Functional Languages" (12,"Cambria",green), 2)] "Languages Slide 2" "Description" [((1,Text "Functional Languages are great.." (12,"Cambria",red)),3),((1,Text "We will Learn them.." (24,"Cambria",blue)),0)]
slide2 :: Slide
slide2 = Slide [(Text "Haskell" (12,"Cambria",red),1),(Text "Agda" (12,"Cambria",green), 2)] "Title Slide 3" "Description" [((1,Text "Both are Functional" (12,"Cambria",red)),3),((1,Text "We concentrate on Haskell more.." (24,"Cambria",blue)),0)]
slide3 :: Slide
slide3 = Slide [(Text "Language Aspects" (12,"Cambria",red),1),(Text "Language Features" (12,"Cambria",green), 2)] "Title Slide 4" "Description" [((1,Text "Language is Tree" (12,"Cambria",red)),3),((1,Text "Help in better understanding." (24,"Cambria",blue)),0)]
slide4 :: Slide
slide4 = Slide [(Text "Topic 1" (12,"Cambria",red),1),(Text "Topic 2" (12,"Cambria",green), 2)] "Title Slide 5" "Description" [((1,Text "Animation" (12,"Cambria",red)),3),((1,Text "Animation 2" (24,"Cambria",blue)),0)]
slide5 :: Slide
slide5 = Slide [(Text "Topic 1" (12,"Cambria",red),1),(Text "Topic 2" (12,"Cambria",green), 2)] "Title Slide 6" "Description" [((1,Text "Animation" (12,"Cambria",red)),3),((1,Text "Animation 2" (24,"Cambria",blue)),0)]
slide6 :: Slide
slide6 = Slide [(Text "Topic 1" (12,"Cambria",red),1),(Text "Topic 2" (12,"Cambria",green), 2)] "Title Slide 7" "Description" [((1,Text "Animation" (12,"Cambria",red)),3),((1,Text "Animation 2" (24,"Cambria",blue)),0)]
slide7 :: Slide
slide7 = Slide [(Text "Topic 1" (12,"Cambria",red),1),(Text "Topic 2" (12,"Cambria",green), 2)] "Title Slide 8" "Description" [((1,Text "Animation" (12,"Cambria",red)),3),((1,Text "Animation 2" (24,"Cambria",blue)),0)]
slide8 :: Slide
slide8 = Slide [(Text "Topic 1" (12,"Cambria",red),1),(Text "Topic 2" (12,"Cambria",green), 2)] "Title Slide 9" "Description" [((1,Text "Animation" (12,"Cambria",red)),3),((1,Text "Animation 2" (24,"Cambria",blue)),0)]
slide9 :: Slide
slide9 = Slide [(Text "Topic 1" (12,"Cambria",red),1),(Text "Topic 2" (12,"Cambria",green), 2)] "Title Slide 10" "Description" [((1,Text "Animation" (12,"Cambria",red)),3),((1,Text "Animation 2" (24,"Cambria",blue)),0)]
slide10 :: Slide
slide10 = Slide [(Text "Topic 1" (12,"Cambria",red),1),(Text "Topic 2" (12,"Cambria",green), 2)] "Title Slide 11" "Description" [((1,Text "Animation" (12,"Cambria",red)),3),((1,Text "Animation 2" (24,"Cambria",blue)),0)]
slidetitle :: Slide
slidetitle = Slide [] "CS 583 - Functional Languages" "Programming in Haskell" []
--
slidelist = [slidetitle,slide1,slide2,slide3,slide4,slide5,slide6,slide7,slide8,slide9,slide10]



setFontSize :: [Location] -> Int -> Slide -> Slide
setFontSize xs i s = Slide (helper xs i (preload s)) (title s)  (description s) (subsequence s)
								

helper :: [Location] -> Int -> [(Content,Location)] -> [(Content,Location)]
helper ls n ((Text s (a,b,c),i):cs) | elem i ls = (Text s (n,b,c),i) : helper ls n cs
									 | otherwise = (Text s (a,b,c),i) : helper ls n cs
helper _  _ []									= []



setFont :: [Location] -> String -> Slide -> Slide
setFont xs i s = Slide (helper1 xs i (preload s)) (title s)  (description s) (subsequence s)
								

helper1 :: [Location] -> String -> [(Content,Location)] -> [(Content,Location)]
helper1 ls n ((Text s (a,b,c),i):cs) | elem i ls = (Text s (a,n,c),i) : helper1 ls n cs
									 | otherwise = (Text s (a,b,c),i) : helper1 ls n cs
helper1 _  _ []									= []
									 --helper ls n ((Img a b,i):cs) 					= (Img a b,i) : helper ls n cs
