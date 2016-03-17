-- ---
-- title: Homework #1, Due Monday 1/26/15
-- ---


-- Haskell Formalities
-- -------------------

-- We declare that this is the Hw1 module and import some libraries:

module Hw1 where
import SOE
import Play
import XMLTypes

-- Part 0: All About You
-- ---------------------

-- Tell us your name, email and student ID, by replacing the respective
-- strings below

myName  = "Moojin Chae"
myEmail = "mochae@ucsd.edu"
mySID   = "A53102484"

-- Part 1: Defining and Manipulating Shapes
-- ----------------------------------------

-- You will write all of your code in the `Hw1.hs` file, in the spaces
-- indicated. Do not alter the type annotations --- your code must
-- typecheck with these types to be accepted.

-- The following are the definitions of shapes:

data Shape = Rectangle Side Side
           | Ellipse Radius Radius
           | RtTriangle Side Side
           | Polygon [Vertex]
           deriving Show
-- >
type Radius = Float
type Side   = Float
type Vertex = (Float, Float)

-- 1. Below, define functions `rectangle` and `rtTriangle` as suggested
--    at the end of Section 2.1 (Exercise 2.1). Each should return a Shape
--    built with the Polygon constructor.

rectangle :: Side -> Side -> Shape
rectangle l w = Polygon [(0, 0), (0, l), (w, l), (w, 0)]

rtTriangle :: Side -> Side -> Shape
rtTriangle b h = Polygon [(0, 0), (b, 0), (0, h)]

-- 2. Define a function

sides :: Shape -> Int
sides (Rectangle _ _)        = 4
sides (Ellipse _ _)          = 42
sides (RtTriangle _ _)       = 3
sides (Polygon [])           = 0
sides (Polygon [v1])         = 0
sides (Polygon [v1, v2])     = 0
sides (Polygon [v1, v2, v3]) = 3
sides (Polygon (_:vs))       = 1 + sides (Polygon vs)

--   which returns the number of sides a given shape has.
--   For the purposes of this exercise, an ellipse has 42 sides,
--   and empty polygons, single points, and lines have zero sides.

-- 3. Define a function

bigger :: Shape -> Float -> Shape
bigger (Rectangle l w) e  = Rectangle (l * sqrt e) (w * sqrt e)
bigger (Ellipse r1 r2) e  = Ellipse (r1 * sqrt e) (r2 * sqrt e)
bigger (RtTriangle b h) e = RtTriangle (b * sqrt e) (h * sqrt e)
bigger (Polygon vs) e     = Polygon (map (\(x, y) -> (x * sqrt e, y * sqrt e)) vs)

--   that takes a shape `s` and expansion factor `e` and returns
--   a shape which is the same as (i.e., similar to in the geometric sense)
--   `s` but whose area is `e` times the area of `s`.

-- 4. The Towers of Hanoi is a puzzle where you are given three pegs,
--    on one of which are stacked $n$ discs in increasing order of size.
--    To solve the puzzle, you must move all the discs from the starting peg
--    to another by moving only one disc at a time and never stacking
--    a larger disc on top of a smaller one.

--    To move $n$ discs from peg $a$ to peg $b$ using peg $c$ as temporary storage:

--    1. Move $n - 1$ discs from peg $a$ to peg $c$.
--    2. Move the remaining disc from peg $a$ to peg $b$.
--    3. Move $n - 1$ discs from peg $c$ to peg $b$.

--    Write a function

hanoi :: Int -> String -> String -> String -> IO ()
hanoi 0 a b c = putStr ""
hanoi n a b c = do hanoi (n - 1) a c b
                   putStr ("move disc from " ++ a ++ " to " ++ b ++ "\n")
                   hanoi (n - 1) c b a

--   that, given the number of discs $n$ and peg names $a$, $b$, and $c$,
--   where a is the starting peg,
--   emits the series of moves required to solve the puzzle.
--   For example, running `hanoi 2 "a" "b" "c"`

--   should emit the text

-- ~~~
-- move disc from a to c
-- move disc from a to b
-- move disc from c to b
-- ~~~

-- Part 2: Drawing Fractals
-- ------------------------

-- 1. The Sierpinski Carpet is a recursive figure with a structure similar to
--    the Sierpinski Triangle discussed in Chapter 3:

-- ![Sierpinski Carpet](/static/scarpet.png)

-- Write a function `sierpinskiCarpet` that displays this figure on the
-- screen:

sierpinskiCarpetDetail :: Window -> Int -> Int -> Int -> IO ()
sierpinskiCarpetDetail w x y d = if d < 5 then
                                    drawInWindow w (withColor Blue(
                                        polygon [(x, y), (x + d, y), (x + d, y + d), (x, y + d), (x, y)]
                                    ))
                                 else
                                    let d' = d `div` 3
                                    in do
                                        sierpinskiCarpetDetail w x y d'
                                        sierpinskiCarpetDetail w x (y + d') d'
                                        sierpinskiCarpetDetail w x (y + 2 * d') d'
                                        sierpinskiCarpetDetail w (x + d') y d'
                                        sierpinskiCarpetDetail w (x + d') (y + 2 * d') d'
                                        sierpinskiCarpetDetail w (x + 2 * d') y d'
                                        sierpinskiCarpetDetail w (x + 2 * d') (y + d') d'
                                        sierpinskiCarpetDetail w (x + 2 * d') (y + 2 * d') d'

sierpinskiCarpet :: IO ()
sierpinskiCarpet = runGraphics $ do
                    w <- openWindow "The Sierpinski Carpet" (620, 620)
                    sierpinskiCarpetDetail w 10 10 600
                    k <- getKey w
                    closeWindow w

-- Note that you either need to run your program in `SOE/src` or add this
-- path to GHC's search path via `-i/path/to/SOE/src/`.
-- Also, the organization of SOE has changed a bit, so that now you use
-- `import SOE` instead of `import SOEGraphics`.

-- 2. Write a function `myFractal` which draws a fractal pattern of your
--    own design.  Be creative!  The only constraint is that it shows some
--    pattern of recursive self-similarity.

myFractalDetail :: Window -> Int -> Int -> Int -> Bool -> IO ()
myFractalDetail w x y d f = if d < 30 then
                                if f then
                                    drawInWindow w (withColor Green(
                                        ellipse (x + d, y) (x, y + d)
                                    ))
                                else
                                    drawInWindow w (withColor Red(
                                        ellipse (x + d, y) (x, y + d)
                                    ))
                            else
                                let d' = d `div` 3
                                    f' = not f
                                in do
                                    myFractalDetail w x y d' f
                                    myFractalDetail w x (y + d') d' f'
                                    myFractalDetail w x (y + 2 * d') d' f
                                    myFractalDetail w (x + d') y d' f'
                                    myFractalDetail w (x + d') (y + 2 * d') d' f'
                                    myFractalDetail w (x + 2 * d') y d' f
                                    myFractalDetail w (x + 2 * d') (y + d') d' f'
                                    myFractalDetail w (x + 2 * d') (y + 2 * d') d' f

myFractal :: IO ()
myFractal = runGraphics $ do
            w <- openWindow "My Fractal" (620, 620)
            myFractalDetail w 10 10 600 True
            k <- getKey w
            closeWindow w

-- Part 3: Recursion Etc.
-- ----------------------

-- First, a warmup. Fill in the implementations for the following functions.

-- (Your `maxList` and `minList` functions may assume that the lists
-- they are passed contain at least one element.)

-- Write a *non-recursive* function to compute the length of a list

lengthNonRecursive :: [a] -> Int
lengthNonRecursive = foldr (\_ n -> 1 + n) 0

-- `doubleEach [1,20,300,4000]` should return `[2,40,600,8000]`

doubleEach :: [Int] -> [Int]
doubleEach []       = []
doubleEach (x : xs) = (2 * x) : doubleEach xs

-- Now write a *non-recursive* version of the above.

doubleEachNonRecursive :: [Int] -> [Int]
doubleEachNonRecursive = foldr (\x xs -> (2 * x) : xs) []

-- `pairAndOne [1,20,300]` should return `[(1,2), (20,21), (300,301)]`

pairAndOne :: [Int] -> [(Int, Int)]
pairAndOne []       = []
pairAndOne (x : xs) = (x, x + 1) : pairAndOne xs

-- Now write a *non-recursive* version of the above.

pairAndOneNonRecursive :: [Int] -> [(Int, Int)]
pairAndOneNonRecursive = foldr (\x y -> (x, x + 1) : y) []

-- `addEachPair [(1,2), (20,21), (300,301)]` should return `[3,41,601]`

addEachPair :: [(Int, Int)] -> [Int]
addEachPair []            = []
addEachPair ((x, y) : xs) = (x + y) : addEachPair xs

-- Now write a *non-recursive* version of the above.

addEachPairNonRecursive :: [(Int, Int)] -> [Int]
addEachPairNonRecursive = foldr (\(x, y) z -> (x + y) : z) []

-- `minList` should return the *smallest* value in the list. You may assume the
-- input list is *non-empty*.

minList :: [Int] -> Int
minList [x]      = x
minList (x : xs) = min x (minList xs)

-- Now write a *non-recursive* version of the above.

minListNonRecursive :: [Int] -> Int
minListNonRecursive l = foldr min (head l) l

-- `maxList` should return the *largest* value in the list. You may assume the
-- input list is *non-empty*.

maxList :: [Int] -> Int
maxList [x]      = x
maxList (x : xs) = max x (maxList xs)

-- Now write a *non-recursive* version of the above.

maxListNonRecursive :: [Int] -> Int
maxListNonRecursive l = foldr max (head l) l

-- Now, a few functions for this `Tree` type.

data Tree a = Leaf a | Branch (Tree a) (Tree a)
              deriving (Show, Eq)

-- `fringe t` should return a list of all the values occurring as a `Leaf`.
-- So: `fringe (Branch (Leaf 1) (Leaf 2))` should return `[1,2]`

fringe :: Tree a -> [a]
fringe (Leaf x)     = [x]
fringe (Branch x y) = fringe x ++ fringe y

-- `treeSize` should return the number of leaves in the tree.
-- So: `treeSize (Branch (Leaf 1) (Leaf 2))` should return `2`.

treeSize :: Tree a -> Int
treeSize (Leaf _)     = 1
treeSize (Branch x y) = treeSize x + treeSize y

-- `treeSize` should return the height of the tree.
-- So: `treeHeight (Branch (Leaf 1) (Leaf 2))` should return `1`.

treeHeight :: Tree a -> Int
treeHeight (Leaf _)     = 0
treeHeight (Branch x y) = 1 + max (treeHeight x) (treeHeight y)

-- Now, a tree where the values live at the nodes not the leaf.

data InternalTree a = ILeaf | IBranch a (InternalTree a) (InternalTree a)
                      deriving (Show, Eq)

-- `takeTree n t` should cut off the tree at depth `n`.
-- So `takeTree 1 (IBranch 1 (IBranch 2 ILeaf ILeaf) (IBranch 3 ILeaf ILeaf))`
-- should return `IBranch 1 ILeaf ILeaf`.

takeTree :: Int -> InternalTree a -> InternalTree a
takeTree _ ILeaf = ILeaf
takeTree n (IBranch a tl tr)
  | n == 0    = ILeaf
  | n < 0     = error "Wrong!"
  | otherwise = IBranch a (takeTree (n - 1) tl) (takeTree (n - 1) tr)

-- `takeTreeWhile p t` should cut of the tree at the nodes that don't satisfy `p`.
-- So: `takeTreeWhile (< 3) (IBranch 1 (IBranch 2 ILeaf ILeaf) (IBranch 3 ILeaf ILeaf))`
-- should return `(IBranch 1 (IBranch 2 ILeaf ILeaf) ILeaf)`.

takeTreeWhile :: (a -> Bool) -> InternalTree a -> InternalTree a
takeTreeWhile _ ILeaf             = ILeaf
takeTreeWhile p (IBranch a tl tr) = if p a then IBranch a (takeTreeWhile p tl) (takeTreeWhile p tr)
                                    else ILeaf

-- Write the function map in terms of foldr:

myMap :: (a -> b) -> [a] -> [b]
myMap f = foldr (\x xs -> f x : xs) []

-- Part 4: Transforming XML Documents
-- ----------------------------------

-- The rest of this assignment involves transforming XML documents.
-- To keep things simple, we will not deal with the full generality of XML,
-- or with issues of parsing. Instead, we will represent XML documents as
-- instances of the following simpliﬁed type:

-- ~~~~
-- data SimpleXML =
--    PCDATA String
--  | Element ElementName [SimpleXML]
--  deriving Show

-- type ElementName = String
-- ~~~~

-- That is, a `SimpleXML` value is either a `PCDATA` ("parsed character
-- data") node containing a string or else an `Element` node containing a
-- tag and a list of sub-nodes.

-- The file `Play.hs` contains a sample XML value. To avoid getting into
-- details of parsing actual XML concrete syntax, we'll work with just
-- this one value for purposes of this assignment. The XML value in
-- `Play.hs` has the following structure (in standard XML syntax):

-- ~~~
-- <PLAY>
--   <TITLE>TITLE OF THE PLAY</TITLE>
--   <PERSONAE>
--     <PERSONA> PERSON1 </PERSONA>
--     <PERSONA> PERSON2 </PERSONA>
--     ... -- MORE PERSONAE
--     </PERSONAE>
--   <ACT>
--     <TITLE>TITLE OF FIRST ACT</TITLE>
--     <SCENE>
--       <TITLE>TITLE OF FIRST SCENE</TITLE>
--       <SPEECH>
--         <SPEAKER> PERSON1 </SPEAKER>
--         <LINE>LINE1</LINE>
--         <LINE>LINE2</LINE>
--         ... -- MORE LINES
--       </SPEECH>
--       ... -- MORE SPEECHES
--     </SCENE>
--     ... -- MORE SCENES
--   </ACT>
--   ... -- MORE ACTS
-- </PLAY>
-- ~~~

-- * `sample.html` contains a (very basic) HTML rendition of the same
--   information as `Play.hs`. You may want to have a look at it in your
--   favorite browser.  The HTML in `sample.html` has the following structure
--   (with whitespace added for readability):

-- ~~~
-- <html>
--   <body>
--     <h1>TITLE OF THE PLAY</h1>
--     <h2>Dramatis Personae</h2>
--     PERSON1<br/>
--     PERSON2<br/>
--     ...
--     <h2>TITLE OF THE FIRST ACT</h2>
--     <h3>TITLE OF THE FIRST SCENE</h3>
--     <b>PERSON1</b><br/>
--     LINE1<br/>
--     LINE2<br/>
--     ...
--     <b>PERSON2</b><br/>
--     LINE1<br/>
--     LINE2<br/>
--     ...
--     <h3>TITLE OF THE SECOND SCENE</h3>
--     <b>PERSON3</b><br/>
--     LINE1<br/>
--     LINE2<br/>
--     ...
--   </body>
-- </html>
-- ~~~

-- You will write a function `formatPlay` that converts an XML structure
-- representing a play to another XML structure that, when printed,
-- yields the HTML speciﬁed above (but with no whitespace except what's
-- in the textual data in the original XML).

formatPlay :: SimpleXML -> SimpleXML
formatPlay (PCDATA s) = PCDATA s
formatPlay (Element tag xs)
  | tag == "PLAY" =
    Element "html" [Element "body" (map formatPlay' xs)]
  | otherwise =
    error "Wrong Format"

formatPlay' :: SimpleXML -> SimpleXML
formatPlay' (PCDATA s) = PCDATA s
formatPlay' (Element tag xs)
  | tag == "TITLE" =
    Element "h1" (map formatTitle xs)
  | tag == "PERSONAE" =
    PCDATA (xml2string (Element "h2" [PCDATA "Dramatis Personae"]) ++ concatMap (xml2string . formatPersona) xs)
  | tag == "ACT" =
    PCDATA (concatMap (xml2string . formatAct) xs)
  | otherwise =
    error "Wrong Format"

formatTitle :: SimpleXML -> SimpleXML
formatTitle (PCDATA s) = PCDATA s
formatTitle _          = error "Wrong Format"

formatPersona :: SimpleXML -> SimpleXML
formatPersona (PCDATA s)              = PCDATA (s ++ "<br/>")
formatPersona (Element "PERSONA" [x]) = formatPersona x
formatPersona _                       = error "Wrong Format"

formatAct :: SimpleXML -> SimpleXML
formatAct (PCDATA s) = PCDATA s
formatAct (Element tag xs)
  | tag == "TITLE" =
    Element "h2" (map formatTitle xs)
  | tag == "SCENE" =
    PCDATA (concatMap (xml2string . formatScene) xs)
  | otherwise =
    error "Wrong Format"

formatScene :: SimpleXML -> SimpleXML
formatScene (PCDATA s) = PCDATA s
formatScene (Element tag xs)
  | tag == "TITLE" =
    Element "h3" (map formatTitle xs)
  | tag == "SPEECH" =
    PCDATA (concatMap ((++ "<br/>") . xml2string . formatSpeech) xs)
  | otherwise =
    error "Wrong Format"

formatSpeech :: SimpleXML -> SimpleXML
formatSpeech (PCDATA s) = PCDATA s
formatSpeech (Element tag xs)
  | tag == "SPEAKER" =
    Element "b" (map formatSpeech xs)
  | tag == "LINE" =
    PCDATA (concatMap (xml2string . formatSpeech) xs)
  | otherwise =
    error "Wrong Format"

-- The main action that we've provided below will use your function to
-- generate a ﬁle `dream.html` from the sample play. The contents of this
-- ﬁle after your program runs must be character-for-character identical
-- to `sample.html`.

mainXML = do writeFile "dream.html" $ xml2string $ formatPlay play
             testResults "dream.html" "sample.html"
-- >
firstDiff :: Eq a => [a] -> [a] -> Maybe ([a],[a])
firstDiff [] [] = Nothing
firstDiff (c:cs) (d:ds)
     | c==d = firstDiff cs ds
     | otherwise = Just (c:cs, d:ds)
firstDiff cs ds = Just (cs,ds)
-- >
testResults :: String -> String -> IO ()
testResults file1 file2 = do
  f1 <- readFile file1
  f2 <- readFile file2
  case firstDiff f1 f2 of
    Nothing -> do
      putStr "Success!\n"
    Just (cs,ds) -> do
      putStr "Results differ: '"
      putStr (take 20 cs)
      putStr "' vs '"
      putStr (take 20 ds)
      putStr "'\n"

-- Important: The purpose of this assignment is not just to "get the job
-- done" --- i.e., to produce the right HTML. A more important goal is to
-- think about what is a good way to do this job, and jobs like it. To
-- this end, your solution should be organized into two parts:

-- 1. a collection of generic functions for transforming XML structures
--    that have nothing to do with plays, plus

-- 2. a short piece of code (a single deﬁnition or a collection of short
--    deﬁnitions) that uses the generic functions to do the particular
--    job of transforming a play into HTML.

-- Obviously, there are many ways to do the ﬁrst part. The main challenge
-- of the assignment is to ﬁnd a clean design that matches the needs of
-- the second part.

-- You will be graded not only on correctness (producing the required
-- output), but also on the elegance of your solution and the clarity and
-- readability of your code and documentation.  Style counts.  It is
-- strongly recommended that you rewrite this part of the assignment a
-- couple of times: get something working, then step back and see if
-- there is anything you can abstract out or generalize, rewrite it, then
-- leave it alone for a few hours or overnight and rewrite it again. Try
-- to use some of the higher-order programming techniques we've been
-- discussing in class.

-- Submission Instructions
-- -----------------------

-- * If working with a partner, you should both submit your assignments
--   individually.

-- * Make sure your `Hw1.hs` is accepted by GHCi without errors or warnings.

-- * Attach your `hw1.hs` file in an email to `cse230@goto.ucsd.edu` with the
--   subject "HW1" (minus the quotes). *This address is unmonitored!*

-- Credits
-- -------

-- This homework is essentially Homeworks 1 & 2 from
-- <a href="http://www.cis.upenn.edu/~bcpierce/courses/552-2008/index.html">UPenn's CIS 552</a>.
