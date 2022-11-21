module LSystems where

import IC.Graphics

type Rule
  = (Char, String)

type Rules
  = [Rule]

type Angle
  = Float

type Axiom
  = String

type LSystem
  = (Angle, Axiom, Rules)

type Vertex
  = (Float, Float)

type TurtleState
  = (Vertex, Float)

type Command
  = Char

type Commands
  = [Command]

type Stack
  = [TurtleState]

type ColouredLine
  = (Vertex, Vertex, Colour)

----------------------------------------------------------
-- Functions for working with systems.

-- Returns the rotation angle for the given system.
angle :: LSystem -> Float
angle (a, ax, r) = a

-- Returns the axiom string for the given system.
axiom :: LSystem -> String
axiom (a, ax, r) = ax

-- Returns the set of rules for the given system.
rules :: LSystem -> Rules
rules (a, ax, r) = r

-- Return the binding for the given character in the list of rules
lookupChar :: Char -> Rules -> String
-- Pre: the character has a binding in the Rules list
lookupChar key [] = ""
lookupChar key ((c, str):rs)
  | c == key = str
  | otherwise = lookupChar key rs

-- Expand command string s once using rule table r
expandOne :: String -> Rules -> String
expandOne s r = concatMap (`lookupChar` r) s

-- Expand command string s n times using rule table r
expand :: String -> Int -> Rules -> String
expand s 0 r = s
expand s n r = expand (expandOne s r) (n - 1) r

radiansPerDegree = 2 * pi / 360
degreesToRadians x = x / 360 * 2 * pi

-- Move a turtle
move :: Command -> Angle -> TurtleState -> TurtleState
move 'F' _ ((x, y), a) = 
  ((x + cos (degreesToRadians a), y + sin (degreesToRadians a)), a)
move 'R' da ((x, y), a) = ((x, y), a - da)
move 'L' da ((x, y), a) = ((x, y), a + da)

--
-- Accepts a list of colours rather than single argument, with each line having
-- a colour from this list
--

trace1Color :: Commands -> Angle -> [Colour] -> [ColouredLine]
trace1Color commands a cols = snd $ helper ((0, 0), 90) (commands, []) 0
  where helper state ([], lines) colIndex = ([], lines)
        helper state (']':cmds, lines) colIndex = (cmds, lines)
        helper state ('[':cmds, lines) colIndex =
          helper state (helper state (cmds, lines) (colIndex + 1)) $ colIndex + 1
        helper s@((x, y), angle) (cmd:cmds, lines) colIndex =
          helper ((x', y'), angle') (cmds,lines ++
            [((x, y), (x', y'), cols 
              !! (colIndex `mod` length cols)) | cmd == 'F']) (colIndex + 1)
                where ((x', y'), angle') = move cmd a s

--
-- Trace lines drawn by a turtle using the given colour, following the
-- commands in `cs' and assuming the given angle of rotation.
--

trace1 :: Commands -> Angle -> Colour -> [ColouredLine]
trace1 commands a c = trace1Color commands a [c]

trace2 :: Commands -> Angle -> Colour -> [ColouredLine]
trace2 commands a c = helper commands ((0, 0), 90) [] []
  where turtleCmds = commands
        helper [] _ lines stateStack = lines
        helper ('[':cmds) state lines stateStack =
          helper cmds state lines (state : stateStack)
        helper (']':cmds) state lines (lastState : stateStack) = 
          helper cmds lastState lines stateStack
        helper (cmd:cmds) s@((x, y), angle) lines stateStack =
          helper cmds ((x',y'), angle')
            (lines ++ [((x, y), (x', y'), c) | cmd == 'F']) stateStack
              where ((x', y'), angle') = move cmd a s
----------------------------------------------------------
-- Some given functions

expandLSystem :: LSystem -> Int -> String
expandLSystem (_, axiom, rs) n
  = expandOne (expand axiom n rs) commandMap

drawLSystem1 :: LSystem -> Int -> Colour -> IO ()
drawLSystem1 system n colour
  = drawLines (trace1 (expandLSystem system n) (angle system) colour)

drawLSystem2 :: LSystem -> Int -> Colour -> IO ()
drawLSystem2 system n colour
  = drawLines (trace2 (expandLSystem system n) (angle system) colour)

----------------------------------------------------------
-- Some test systems.

cross, triangle, arrowHead, peanoGosper, dragon, snowflake, tree, bush, canopy, galaxy :: LSystem

cross
  = (90,
     "M-M-M-M",
     [('M', "M-M+M+MM-M-M+M"),
      ('+', "+"),
      ('-', "-")
     ]
    )

triangle
  = (90,
     "-M",
     [('M', "M+M-M-M+M"),
      ('+', "+"),
      ('-', "-")
     ]
    )

arrowHead
  = (60,
     "N",
     [('M', "N+M+N"),
      ('N', "M-N-M"),
      ('+', "+"),
      ('-', "-")
     ]
    )

peanoGosper
  = (60,
     "M",
     [('M', "M+N++N-M--MM-N+"),
      ('N', "-M+NN++N+M--M-N"),
      ('+', "+"),
      ('-', "-")
     ]
    )

dragon
  = (45,
     "MX",
     [('M', "A"),
      ('X', "+MX--MY+"),
      ('Y', "-MX++MY-"),
      ('A', "A"),
      ('+', "+"),
      ('-', "-")
     ]
    )

snowflake
  = (60,
     "M--M--M",
     [('M', "M+M--M+M"),
      ('+', "+"),
      ('-', "-")
     ]
    )

tree
  = (45,
     "M",
     [('M', "N[-M][+M][NM]"),
      ('N', "NN"),
      ('[', "["),
      (']', "]"),
      ('+', "+"),
      ('-', "-")
     ]
    )

bush
  = (22.5,
     "X",
     [('X', "M-[[X]+X]+M[+MX]-X"),
      ('M', "MM"),
      ('[', "["),
      (']', "]"),
      ('+', "+"),
      ('-', "-")
     ]
    )

canopy
  = (30.0,
     "M",
     [('M', "M[+MM][-MM]M[-M][+M]M"),
      ('[', "["),
      (']', "]"),
      ('+', "+"),
      ('-', "-")
     ]
    )

galaxy
  = (36.0,
     "[M]++[M]++[M]++[M]++[M]",
     [('M', "+M--M---M"),
      ('[', "["),
      (']', "]"),
      ('+', "+"),
      ('-', "-")
     ]
    )

commandMap :: Rules
commandMap
  = [('M', "F"),
     ('N', "F"),
     ('X', ""),
     ('Y', ""),
     ('A', ""),
     ('[', "["),
     (']', "]"),
     ('+', "L"),
     ('-', "R")
    ]
