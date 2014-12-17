module SlideShow where

import Control.Monad
import Control.Monad.State
import Prelude hiding (foldl)
import Control.Monad.Trans
import Control.Monad.Trans.Maybe

-- We wanted to connect the creating part and presenting part, but failed.
-- all things presented in IO Monad, so things can't be stored
-- A guess : maybe can be stored in the value part of the state monad.
-----------------------------------------------------------
-- 														 --
--		                    Slide		                 --
--														 --
-----------------------------------------------------------
--type Source = String
type TOps = (FontSize, Font, Color)

-- text attributes
type FontSize = Int
type Font = String
type Color = (Int,Int,Int)

red,green,blue :: Color
red = (255,0,0)
green = (0,255,0)
blue = (0,0,255)

--type GOps = String
--type AOps = String
type Location = Int
type Style = Int

type Title = String
type Description = String 

--
data Content = Text String TOps
			 -- | Img Source GOps
			 -- | Align AOps Content Content
			 deriving (Show,Eq)
--
type Animation = (Style,Content)

-- basic object are slide's
data Slide = Slide { preload :: [(Content,Location)]
                    ,title :: Title
					,description :: Description
					,subsequence :: [(Animation,Location)]
					--,background :: Source
					}
					deriving (Show,Eq)
-----------------------------------------------------------
-- 														 --
--		                Show a slide		             --
--														 --
-----------------------------------------------------------
-- run slide by first print title, preload and then animation
runSlide :: Maybe Slide -> IO ()
runSlide Nothing  = putStrLn "Error : Please check if the initialRepo is used !!!"
runSlide (Just a) = do
	putStrLn "\n---------------------------------------Slide Start--------------------------------------"
	putStrLn $ spaces 25 ++ "Title :" ++ title a 
	putStrLn $ printContent (preload a) 
	runAnimation (subsequence a)
	putStrLn "\n---------------------------------------Slide Ends---------------------------------------"

-- print the preload part (plain content)
printContent = foldr (\(c,i) b -> "\n   " ++ show c ++ " appears at position " ++ show i ++ ".\n" ++ b) ""

-----------------------------------------------------------
-- we run Animation w.r.t a valid answer in each step
runAnimation :: [(Animation,Location)] -> IO ()
runAnimation []     = putStrLn "\nTrying to access next page....."
runAnimation (x:xs) = do
	Just p <- runMaybeT getUntilValid
	if p == 'y' 
	then (putStrLn.printAnimation) x >> runAnimation xs 
	else  putStrLn.printContent $ map (\((s,c),l)-> (c,l)) (x:xs)

-- print a single animation
printAnimation ((style,content), location) = "\nContent : " ++ show content ++ " : acts in style " ++ show style ++ " at location " ++ show location

--------------------------------------------------------------------
-- for the first valid answer for operations on a single slide
--interaction with users in animations(from password.hs) 

-- Check to see if the response from user is valid
-- "y" for continue and "n" for skip 
isPassed :: Char -> Bool
isPassed p = elem p "yn"

-- get the answer from users
getAnswer :: IO Char
getAnswer = do
  putStr "\nEnter y for continue; Enter n to skip all animations\n"
  getChar

-- | Get a response from the user, or Nothing.
getResponse :: MaybeT IO Char
getResponse = do
  p <- lift getAnswer
  guard (isPassed p)
  return p

-- | The answer was invalid, try again.
tryAgain :: MaybeT IO Char
tryAgain = do
  lift $ putStrLn "Answer invalid, try again!"
  getResponse

-- | try multiple times
getUntilValid :: MaybeT IO Char
getUntilValid = msum (getResponse : repeat tryAgain)

-----------------------------------------------------------
-- 														 --
--				slideEnv(for storing all slides)		 --
--														 --
-----------------------------------------------------------	
type PageNum = Int
-- let presentations be ordered lists of Slides
-- which should consist of order list which is [Int]
-- and a repository of Slides
type Order = [PageNum]
type SlideEnv = PageNum -> Maybe Slide
-- environment-like repository is itself a search function
-- we have to encode existing page#s in the State part, or can't do catalogue
type SlideRepo = (Order,SlideEnv)
--type Concept = (String,[Slide]) 

-- deleted function for grouping by relations
-- the same-titile slides under the same concept which can be skipped in the presentation

-- this definition can group Subject and Concept together
-- but only the nest times matter (layer is the key to struct)
--data Presentation = Concept String [Slide]  
--                  | Subject String [Presentation]
--				  deriving (Show)
--------------------------------------------------------------
-- 														    --
--RuntimeRepo("zipper" for record the order of presentation)--
--														    --
--------------------------------------------------------------
-- when we run presentations, we want to use zipper to record the pages we have gone through, so here it is
type RuntimeRepo = (Pred,ToPresent)
-- following two parts form a current state in show Presentation part. The last of Pred is the current page we are at
type Pred = [PageNum]
type ToPresent = [([PageNum],AttachMents)]
-- attachmensts are slides that can be skipped
type AttachMents = [PageNum]

{- pack SlideRepo and RuntimeRepo-}
type Repo = (SlideRepo,RuntimeRepo)

--type Presentation m = StateT SlideEnv m Slide
--type Presentation m a = StateT SlideRepo m a
type Presentation m a = StateT Repo m a
-- Presentation definition

-----------------------------------------------------------
-- 														 --
--		operations on slideEnv(for maximum reuse)		 --
--														 --
-----------------------------------------------------------	
-- empty environment
-- pay attention to the literal meaning of the argument name
empty :: SlideEnv
empty = \absoluteIdentifier -> Nothing

-- initial state
initialRepo :: Repo
initialRepo = (([],empty),([],[]))


-- for refactoring
case_of :: Bool -> Presentation IO ()
case_of False = return ()
case_of True  = do
	((ord,env),(pre,to)) <- get
	lift (runSlide (env (last pre))) -- runslide on new state
	continue
-- apply the same modification to several slides
-- users can define function f in this part because modifications on slides 
-- are just functions can be composable
-- modifyOn :: [PageNum] -> (Slide -> Slide) -> SlideEnv -> SlideEnv
-- modifyOn ns f env = \x -> if elem x ns then fmap f $ env x else env x 
--modifyOn :: [PageNum] -> (Slide -> Slide) -> Presentation IO ()
--modifyOn ns f = do 
--	((ord,env),(pre,to)) <- get
--	case pre of 
--		[] -> put ((ord, \x -> if elem x ns then fmap f $ env x else env x) , (pre,to)) -- the presentation hasn't started so we only change the slideRepo 
--		_  -> do
--    			put ((ord, \x -> if elem x ns then fmap f $ env x else env x) , (pre,to)) -- the presentaion is on, so we will also print the current view 
--    			lift (runSlide (env (last pre)))
--    			continue
modifyOn :: [PageNum] -> (Slide -> Slide) -> Presentation IO ()
modifyOn ns f = do 
	((ord,env),(pre,to)) <- get
	put ((ord, \x -> if elem x ns then fmap f $ env x else env x) , (pre,to))
	case_of $ (not.isEmpty) pre
-- add a new slide to the repository
-- addS :: Slide -> SlideEnv -> SlideEnv
-- addS s env = \x -> if x == fresh env then Just s else env x 
--addS :: Slide -> Presentation IO ()
--addS s = do
--	((ord,env),(pre,to)) <- get
--	let y = fresh ord 
--	 in 
--	 case pre of 
--		[] -> put ((ord++[y], \x -> if x == y then Just s else env x) , (pre,to)) -- the presentation hasn't started so we only change the slideRepo
--		_  -> do
--				put ((ord++[y], \x -> if x == y then Just s else env x) , (pre,to)) -- the presentaion is on, so we will also print the current view
--				(lift $ runSlide (env (last pre))) >> continue
addS :: Slide -> Presentation IO ()
addS s = do
	((ord,env),(pre,to)) <- get
	let y = fresh ord in put ((ord++[y], \x -> if x == y then Just s else env x) , (pre,to)) 
	case_of $ (not.isEmpty) pre
-- add several slides to the repository
-- addL :: [Slide] -> SlideEnv -> SlideEnv 
-- addL []     env = env
-- addL (s:ss) env = addL ss (addS s env)
addL :: [Slide] -> Presentation IO ()
addL = foldl (\x y -> x >> addS y) (return ()) 
-- delete slides
-- deleteS :: [PageNum] -> SlideEnv -> SlideEnv
-- deleteS ns env = \x -> if elem x ns then empty x else env x 
--deleteS :: [PageNum] -> Presentation IO ()
--deleteS ns = do
--	((ord,env),(pre,to)) <- get
--	case pre of 
--		[] -> put ((filter (\x -> not (elem x ns)) ord, \x -> if elem x ns then empty x else env x), (pre,to)) -- the presentation hasn't started so we only change the slideRepo
--		_  -> if elem (last pre) ns -- check if the current page is deleted because deleteS may change the current view
--			  then do 
--			  		put ((filter (\x -> not (elem x ns)) ord, \x -> if elem x ns then empty x else env x), -- change the SlideRepo
--			  			 (filter (\x -> not (elem x ns)) pre, filterT (\x -> not (elem x ns)) to)) -- change the runtimeRep
--			  		(lift $ putStrLn "Current page removed. Trying to access next page.....") >> continue
--			  else do
--			  		put ((filter (\x -> not (elem x ns)) ord, \x -> if elem x ns then empty x else env x), -- change the SlideRepo
--			  			 (filter (\x -> not (elem x ns)) pre, filterT (\x -> not (elem x ns)) to))
--			  		(lift $ runSlide (env (last pre))) >> continue
deleteS :: [PageNum] -> Presentation IO ()
deleteS ns = do
	((ord,env),(pre,to)) <- get
	case pre of 
		[] -> put ((filter (\x -> not (elem x ns)) ord, \x -> if elem x ns then empty x else env x), (pre,to)) -- the presentation hasn't started so we only change the slideRepo
		_  -> do
			  	put ((filter (\x -> not (elem x ns)) ord, \x -> if elem x ns then empty x else env x), -- change the SlideRepo
			  			 (filter (\x -> not (elem x ns)) pre, filterT (\x -> not (elem x ns)) to)) -- change the runtimeRep
			  	if elem (last pre) ns 
			  		then (lift $ putStrLn "Current page removed. Trying to access next page.....") >> continue
			  		else (lift $ runSlide (env (last pre))) >> continue

-- give brief info of the slideRepo
--
catalogue :: Presentation IO ()
catalogue = do
	((ord,env),(pre,to)) <- get
	lift $ putStrLn (let xs = map env ord -- get [Just Slide]
		   			  in concat $ zipWith (++) 
		   			  	 (map (\x -> "\nPage#:  " ++ show x ++ "\n") ord) 
						 (map (maybe "\nerror in catalogue" (\x -> "Title:\n" ++ title x ++ "\nBrief:\n" ++ description x ++ "\n")) xs))
	case_of $ (not.isEmpty) pre	
	--case pre of 
	--	[] -> lift $ putStrLn ""
	--	_  -> (lift $ runSlide (env (last pre))) >> continue 

-- give possible page#s to be grouped
pages :: Presentation IO ()
pages = do
	((ord,env),(pre,to)) <- get
	lift $ putStrLn ("There are slideIDs :" ++ foldr (\x y -> show x ++ "," ++ y) "" ord)
	case_of $ (not.isEmpty) pre		
	--case pre of 
	--	[] -> lift $ putStrLn "" 
	--	_  -> (lift $ runSlide (env (last pre))) >> continue 

-----------------------------------------------------------
-- 														 --
--		immediate view control(for presenting) 		     --
--														 --
-----------------------------------------------------------	
index :: Presentation IO ()
index = do 
	((ord,env),(pre,to)) <- get
	(let total = pre ++ (concat $ map fst to)
	 in lift.putStrLn.concat $ zipWith (++) (map (\x -> "\nOn Page: " ++ show x ++ "\t") $ take (length total) nat) (map (maybe "\nerror in index" (\x -> "\nTitle:" ++ title x ++ "\nBrief:\n" ++ description x ++ "\n")) $ map env total))

goback :: Presentation IO ()
goback = do
	((ord,env),(pre,to)) <- get
	case reverse pre of	
		x:y:xs -> (lift.runSlide.env $ y) >> put ((ord,env),(reverse (y:xs), ([x],[]):to))
		x:_ -> lift. putStrLn $ "\nStay on current page\n"
		_ -> lift. putStrLn $ "Error in goback"

getAttach :: Presentation IO ()
getAttach = do
	((ord,env),(pre,to)) <- get
	case to of 
		([],x:[]):xxs -> (lift.runSlide.env $ x) >> put ((ord,env),(pre ++ [x],xxs))
		([],x:xs):xxs -> (lift.runSlide.env $ x) >> put ((ord,env),(pre ++ [x],([],xs):xxs))
		_ -> lift. putStrLn $ "Error in getAttach"

end :: Presentation IO ()
end = do
	((ord,env),(pre,to)) <- get
	lift $ putStrLn "\nThe end. Thank you. Enjoy other functionality SlideShow provides.\n"
	put ((ord,env),([],[]))

next :: Presentation IO ()
next = do
	((ord,env),(pre,to)) <- get
	case to of
		(x:[],[]):xys -> (lift $ runSlide (env x)) >> put ((ord,env),(pre ++ [x],xys))
		(x:xs,ys):xys -> (lift $ runSlide (env x)) >> put ((ord,env),(pre ++ [x],(xs,ys):xys))
		([],_):(y:[],[]):xys -> (lift.runSlide.env $ y) >> put ((ord,env),(pre ++ [y],xys)) -- next for skipping also remove ([],[])
		([],_):(y:xs,ys):xys -> (lift.runSlide.env $ y) >> put ((ord,env),(pre ++ [y],(xs,ys):xys)) -- next for skipping 
		_ -> lift $ putStrLn "Error in next Or end of presentation. Pls enter end."

-- check if the presentation is over or not . if not, then give the info of next page
nextInfo :: Presentation IO ()
nextInfo = do 
	((ord,env),(pre,to)) <- get
	if to == []
	then lift $ putStrLn "\nReached End of Presentation..."
	else lift $ putStrLn ("\nCommands Available:\n" ++ spaces 5 ++ "next\t: to get next Slide\n" ++ spaces 5 ++ "goback\t: to Go Back\n"++ spaces 5 ++ "index\t: to Get Index\n"++ spaces 5 ++ "get\t: to get additional Slide to current slides\n" ++ 
						(case (head to,tail to) of 
							((x:_,_),_) -> "Next slide sneek peek:" ++ maybe "\n!!error!!" (\a -> "\n" ++ title a ++ "\nBrief: " ++ description a) (env x)
							(([],y:_),[]) -> "Additional slide sneek peek: " ++ maybe "\n!!error!!" (\a -> "\n" ++ title a ++ "\nBrief: " ++ description a) (env y) ++ "\nThen here comes the end."
							(([],y:_),ds) -> "Additional slide sneek peek: " ++ maybe "\n!!error!!" (\a -> "\n" ++ title a ++ "\nBrief: " ++ description a) (env y) ++ 
											"\n\nOR\n\nTo skip attachments enter next." ++ maybe "\n!!nextInfo error!!" (\a -> "\nNext slide sneek peek:" ++ title a ++ "\nBrief: " ++ description a) 
											(env ((head.fst.head.tail) to))
							_ -> "\nnext-error occured!!"))

-- recusion to make presentation
continue :: Presentation IO ()
continue = do
	((ord,env),(pre,to)) <- get
	if to == []
	then (do 
		 lift $ putStrLn "Reached end of presentation.Enter end/goback/index to end/goback/get index." 
		 Just p <- lift $ runMaybeT getUntilValidS
		 case p of 
		 	"end" -> end
		 	"goback" -> goback >> continue
		 	"index" -> index >> continue
		 	)
	else (do
		 nextInfo
		 Just p <- lift $ runMaybeT getUntilValidS
		 case p of 
		 	"end" -> end
		 	"goback" -> goback >> continue
		 	"get" -> getAttach >> continue
		 	"next" -> next >> continue
		 	"index" -> index >> continue
		)

-- Present view
runBy :: ToPresent -> Presentation IO ()
runBy ts = do
	((ord,env),(pre,to)) <- get
	case filter (not.isEmpty.fst) (filterT (flip elem ord) ts) of
		(x:[],[]):xs -> put ((ord,env),([x],xs)) >> (lift.runSlide.env) x >>index >> continue
		(x:xs,ys):xys -> put ((ord,env),([x],(xs,ys):xys)) >> (lift.runSlide.env) x >> index >> continue
		_ -> lift.putStrLn $ "error in runBy!"
	

-- it seems restart maks no sense because you have to run all stuff again
--restart :: Presentation IO ()
runPresentation :: Presentation IO () -> IO ()
runPresentation s = do
	runStateT s initialRepo
	putStrLn "success"
------------------------------------------------------------
-- | Get a response from the user, or Nothing.
getResponseS :: MaybeT IO String
getResponseS = do
  p <- lift getLine
  guard (elem p ["next","get","goback","end","index"])
  return p

-- | The answer was invalid, try again.
tryAgainS :: MaybeT IO String
tryAgainS = do
  lift $ putStrLn "Answer invalid, try again!"
  getResponseS 

-- | try multiple times
getUntilValidS :: MaybeT IO String
getUntilValidS = msum (getResponseS : repeat tryAgainS)

-----------------------------------------------------------
-- 														 --
--					helper functions 				     --
--														 --
-----------------------------------------------------------	
isEmpty [] = True
isEmpty _ = False

--filter on topresent zipper
filterT :: (PageNum -> Bool) -> ToPresent -> ToPresent
filterT _ [] 			= []
filterT p ((xs,ys):xys) = case (filter p xs,filter p ys) of
							([],[]) -> filterT p xys
							(a,b) -> (a,b):filterT p xys
-- for fresh page#'s
-- natural #'s
nat :: [Int]
nat = 1 : (map succ nat) 
-- fresh page #
fresh :: [Int] -> Int
fresh ord = head (do 
					n <- nat
					guard (not (elem n ord))
					return n
					)

--for reference from hoogle
foldl        :: (b -> a -> b) -> b -> [a] -> b
foldl f z0 xs0 = lgo z0 xs0
             where
                lgo z []     =  z
                lgo z (x:xs) = lgo (f z x) xs

--for alignments
spaces :: Int -> String
spaces n = replicate n ' '
