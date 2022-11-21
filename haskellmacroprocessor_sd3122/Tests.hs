module Tests where

import IC.TestSuite

import MP hiding (main)

lookUpTestCases
  = [ ("A", [("A", 8), ("B",9), ("C",5), ("A",7)]) ==> [8,7]
    , ("a", []) ==> []
    , ("a", [("a", 9)]) ==> [9]
    , ("a", [("b", 9)]) ==> []
    , ("abc", [("def", 2), ("abc", 10), ("xyz", 5)]) ==> [10]
    , ("a", [("a", 10), ("a", 5), ("a", 20), ("a", 7)]) ==> [10, 5, 20, 7]
    ]

splitTextTestCases
  = [ (" .,", "A comma, then some words.")
        ==> (" ,   .",["A","comma","","then","some","words",""])
    , ("", "")
        ==> ("", [""])
    , (".", "A.B")
        ==> (".", ["A","B"])
    , (" ", " A")
        ==> (" ", ["", "A"])
    ]

combineTestCases
  = [ (" ,   .", ["A","comma","","then","some","words",""])
        ==> ["A"," ","comma",",",""," ","then"," ","some"," ","words",".",""]

    , ("", [""])
        ==> [""]
    , (".", ["A","B"])
        ==> ["A",".","B"]
    , (" ", ["", "A"])
        ==> [""," ","A"]
    ]

getKeywordDefsTestCases
  = [ ["$rule Reproduce this precisely -- or else!!"]
        ==> [("$rule","Reproduce this precisely -- or else!!")]
    , ["$x Define x", "$y 55"]
        ==> [("$x","Define x"),("$y","55")]
    , ["$a A", "$b B", "$c C"]
        ==> [("$a","A"),("$b","B"),("$c","C")]
    , []
        ==> []
    , ["$x-y-z $$$"]
        ==> [("$x-y-z","$$$")]
    , ["$$ something to think about"]
        ==> [("$$","something to think about")]
    , ["$ meanie!"]
        ==> [("$","meanie!")]
    , ["$var  Tristan Allwood"]
        ==> [("$var", " Tristan Allwood")]
    , ["$-1;2 Think\nabout"] ==> [("$-1;2", "Think\nabout")]
    ]

expandTestCases :: [((String, String), String)]
expandTestCases
  = [ ("The capital of $1 is $2", "$1 Peru\n$2 Lima.")
        ==> "The capital of Peru is Lima."
    , ("The time is $a", "$a now.")
        ==> "The time is now."
    , ("Keywords (e.g. $x, $y, $z...) may appear anwhere, e.g. <$here>.",
       "$x $a\n$y $b\n$z $c\n$here $this-is-one")
        ==> "Keywords (e.g. $a, $b, $c...) may appear anwhere, e.g. <$this-is-one>."
    , ("$var1,$var2;$var3\n$var4",
       "$var1 1\n$var2 10\n$var3 abc\n$var4 ()")
        ==> "1,10;abc\n()"
    ]

enhancedExpandTestCases :: [((String, String), String)]
enhancedExpandTestCases
    = [ ("Your name is $name, your age is $age", 
        "$name a\n$age 12\n#\n$name b\n$age 20\n#\n$name c\n$age 90")
        ==> "Your name is a, your age is 12\n-----\nYour name is b, your age is 20\n-----\nYour name is c, your age is 90" ]

allTestCases
  = [ TestCase "lookUp"  (uncurry lookUp)
                         lookUpTestCases
    , TestCase "splitText"   (uncurry splitText)
                         splitTextTestCases
    , TestCase "combine" (uncurry combine)
                         combineTestCases

    , TestCase "getKeywordDefs" getKeywordDefs
                                getKeywordDefsTestCases

    , TestCase "expand"  (uncurry expand)
                         expandTestCases
    , TestCase "enhancedExpand"  (uncurry enhancedExpand)
                         enhancedExpandTestCases
    ]

runTests = mapM_ goTest allTestCases

main = runTests
