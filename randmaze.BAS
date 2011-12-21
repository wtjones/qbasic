'               ********** Random   Maze  Generator 1.1 **********
'                            William Travis Jones
'
'
'
'Press b to toggle vsync during maze building.
'Press s to toggle vsync during solving.
'Press v to toggle both.
'Press ESC to quit.
'
'Flaws/Ideas:
'-- Doesn't let you select/randomize the complexity. Could probably do this
'     simply by not using up all the wall corners. (denoted by the "." in the
'     maze array)
'-- Could use a variable speed during solving, maybe by only waiting for
'     vsync every 3 calls to SolveMaze ().
'-- Start and endpoints sometimes too close.
'-- Needlessly ineffecient. The maze array probably doesn't really need
'     to be there.

'History:
'-- Started December 14, 2000
'-- Finished working version .9 much later that night (only makes the maze).
'-- 12/15/2000: finished 1.0 with maze solver
'-- Version 1.1 -- 12/15/2000:
'     Few tweaks. Start/endpoints still too close sometimes

DECLARE SUB DrawBorder ()
DECLARE SUB InsertCorners ()
DECLARE SUB ClearMaze ()
DECLARE SUB BuildMaze ()
DECLARE SUB HandleKeys ()
DECLARE SUB RandomSize ()
DECLARE SUB RandomStartAndEnd ()
DECLARE SUB PrintOnMaze (pCol AS INTEGER, pRow AS INTEGER, char AS STRING)
DECLARE SUB SolveMaze (col AS INTEGER, row AS INTEGER, success AS INTEGER)
DECLARE FUNCTION CoordInBounds% (col AS INTEGER, row AS INTEGER)

DEFINT A-Z: RANDOMIZE TIMER
CLEAR , , 12000      '-- Must Increase the stack space or it WILL run out --

'------CONSTANTS-----
CONST TRUE = 1, FALSE = 0, UP = 1, RIGHT = 2, DOWN = 3, LEFT = 4
CONST SCREENCOL = 80, SCREENROW = 50

'-----GLOBALS------
DIM SHARED colMax AS INTEGER
DIM SHARED rowMax AS INTEGER

DIM SHARED topLeftCol AS INTEGER
DIM SHARED topLeftRow AS INTEGER
DIM SHARED startCol AS INTEGER, startRow AS INTEGER
DIM SHARED endCol AS INTEGER, endRow AS INTEGER
DIM SHARED maze(SCREENCOL - 2, SCREENROW - 2) AS STRING * 1
DIM SHARED theKey AS STRING * 1
DIM SHARED vsync AS INTEGER
DIM SHARED buildVsync AS INTEGER   'toggle for vsync
DIM SHARED solveVsync AS INTEGER
DIM SHARED done AS INTEGER    'has user pressed esc?
DIM SHARED success AS INTEGER

'------SCREEN SETUP------
SCREEN 11: WIDTH SCREENCOL, SCREENROW

'------MAIN PROGRAM---------
vsync = FALSE        '--To auto toggle both buildVsync and solveVsync.--
buildVsync = FALSE
solveVsync = TRUE
done = FALSE
CALL ClearMaze

'-----MAIN LOOP---------
WHILE done = FALSE
   CALL RandomSize
   CALL InsertCorners
   CALL DrawBorder
   CALL BuildMaze
   CALL HandleKeys
  
   '-- Now solve the maze, drawing the progress...
   clr = INT(RND * 15 + 1): COLOR clr, 0
   success = FALSE
   CALL SolveMaze(startCol, startRow, success)
  
   SLEEP 1: CLS
   CALL ClearMaze
WEND

END

SUB BuildMaze
   'Derived from explanation at
   'http://chesworth.com/pv/games/random
  
wallColor = INT(RND * 15) + 1
wallChar = INT(RND * 3) + 176
COLOR wallColor, 0

totalCorners = ((colMax - 1) / 2) * ((rowMax - 1) / 2)

cornerLeft = TRUE
WHILE cornerLeft = TRUE    'begin the nested yukiness
   gotCorner = FALSE
   WHILE gotCorner = FALSE    'main speed culprit left to fix
      randCol = INT(RND * ((colMax) / 2)) * 2
      randRow = INT(RND * ((rowMax) / 2)) * 2
      IF maze(randCol, randRow) = "." THEN gotCorner = TRUE
   WEND  'We now have a corner.
  
   randDir = INT(RND * 4 + 1)  'Constants UP, DOWN, LEFT, RIGHT
   hitWall = FALSE: col = randCol: row = randRow
   WHILE hitWall = FALSE
     
      'LOCATE topLeftRow + row - 1, topLeftCol + col - 1
      'PRINT CHR$(wallChar)
      CALL PrintOnMaze(col, row, CHR$(wallChar))
      maze(col, row) = "X"
      IF buildVsync = TRUE THEN WAIT &H3DA, 8
      SELECT CASE randDir
         CASE UP:
            row = row - 1
         CASE DOWN:
            row = row + 1
         CASE LEFT:
            col = col - 1
         CASE RIGHT
            col = col + 1
      END SELECT
      'LOCATE 1, 1: PRINT col, row: SLEEP
      IF row < 1 OR row > rowMax OR col < 1 OR col > colMax THEN
         hitWall = TRUE
      ELSE IF maze(col, row) = "X" THEN hitWall = TRUE
      END IF

      CALL HandleKeys    '---Key press detour------
      IF done = TRUE THEN END    'This is very bad practice.
   WEND     'We hit a wall.

   'See if there are any corners left...
   cornerLeft = FALSE
   FOR col = 1 TO colMax
      FOR row = 1 TO rowMax
         IF maze(col, row) = "." THEN
         cornerLeft = TRUE: EXIT FOR         'yuk...
         END IF
      NEXT row
      IF cornerLeft = TRUE THEN EXIT FOR     'yuk...
   NEXT col

WEND  'End of corners.

CALL RandomStartAndEnd     '--Picks "S" and "E" points in maze array.--
COLOR INT(RND * 5) + 17, 0    'Pick random blinking color
CALL PrintOnMaze(startCol, startRow, "S")
CALL PrintOnMaze(endCol, endRow, "E")
END SUB

SUB ClearMaze

FOR col = 1 TO SCREENCOL - 2     'This should be the array bounds.
   FOR row = 1 TO SCREENROW - 2
   maze(col, row) = " "
   NEXT row
NEXT col

END SUB

FUNCTION CoordInBounds (col AS INTEGER, row AS INTEGER)

result = FALSE

IF col > 0 AND col <= colMax AND row > 0 AND row <= rowMax THEN
   result = TRUE
END IF

CoordInBounds = result

END FUNCTION

SUB DrawBorder

'First Clear the inside of maze.
FOR col = topLeftCol TO topLeftCol + colMax
   FOR row = topLeftRow TO topLeftRow + rowMax
   LOCATE row, col: PRINT " "
   NEXT row
NEXT col

COLOR INT(RND * 15 + 1), 0
wallChar = INT(RND * 3) + 176

row = topLeftRow - 1
FOR col = topLeftCol - 1 TO topLeftCol + colMax
   LOCATE row, col: PRINT CHR$(wallChar)
   LOCATE row + rowMax + 1, col: PRINT CHR$(wallChar)
NEXT

col = topLeftCol - 1
FOR row = topLeftRow - 1 TO topLeftRow + rowMax
   LOCATE row, col: PRINT CHR$(wallChar)
   LOCATE row, col + colMax + 1: PRINT CHR$(wallChar)
NEXT row

END SUB

SUB HandleKeys
   '****** Toggles vsync variables and and the escape key. *******

theKey = INKEY$
IF theKey = "v" OR theKey = "V" THEN
   IF vsync = FALSE THEN
      vsync = TRUE: buildVsync = TRUE: solveVsync = TRUE
   ELSE IF vsync = TRUE THEN vsync = FALSE: buildVsync = FALSE: solveVsync = FALSE
   END IF
END IF

IF theKey = "s" OR theKey = "S" THEN
   IF solveVsync = FALSE THEN
      solveVsync = TRUE
   ELSE IF solveVsync = TRUE THEN solveVsync = FALSE
   END IF
END IF

IF theKey = "b" OR theKey = "B" THEN
   IF buildVsync = FALSE THEN
      buildVsync = TRUE
   ELSE IF buildVsync = TRUE THEN buildVsync = FALSE
   END IF
END IF

IF theKey = CHR$(27) THEN
   done = TRUE: END
END IF

END SUB

SUB InsertCorners

FOR col = 2 TO colMax - 1 STEP 2
   FOR row = 2 TO rowMax - 1 STEP 2
   maze(col, row) = "."
   NEXT row
NEXT col


END SUB

SUB PrintOnMaze (pCol AS INTEGER, pRow AS INTEGER, char AS STRING)

LOCATE topLeftRow + pRow - 1, topLeftCol + pCol - 1
PRINT char
END SUB

SUB RandomSize    'sets size of maze
  
   'Dimensions of maze MUST be odd.
   gotOdd = FALSE
   WHILE gotOdd = FALSE
      colMax = INT(RND * (SCREENCOL - 15)) + 12
      IF colMax MOD 2 <> 0 THEN gotOdd = TRUE
   WEND

   gotOdd = FALSE
   WHILE gotOdd = FALSE
      rowMax = INT(RND * (SCREENROW - 15)) + 12
      IF rowMax MOD 2 <> 0 THEN gotOdd = TRUE
   WEND
 
   topLeftCol = INT((SCREENCOL / 2) - (colMax / 2)) 'Get the top left coor for
   topLeftRow = INT((SCREENROW / 2) - (rowMax / 2)) 'centering.

END SUB

SUB RandomStartAndEnd

'Set random start point...
randSide = INT(RND * 4) + 1   'The side of maze to put it(UP,DOWN,LEFT,RIGHT)
validSpot = FALSE
WHILE validSpot = FALSE
   SELECT CASE randSide
      CASE UP:
         row = 1: col = INT(RND * colMax + 1)
      CASE DOWN:
         row = rowMax: col = INT(RND * colMax + 1)
      CASE LEFT:
         row = INT(RND * rowMax + 1): col = 1
         'LOCATE 1, 1: PRINT row, col: SLEEP
      CASE RIGHT:
         row = INT(RND * rowMax + 1): col = colMax
   END SELECT
   'SLEEP
   IF maze(col, row) <> "X" THEN
      maze(col, row) = "S": startCol = col: startRow = row: validSpot = TRUE
   END IF
WEND

'Set random end point...
randSide = INT(RND * 4) + 1   'The side of maze to put it(UP,DOWN,LEFT,RIGHT)
validSpot = FALSE
WHILE validSpot = FALSE
   SELECT CASE randSide
      CASE UP:
         row = 1: col = INT(RND * colMax + 1)
      CASE DOWN:
         row = rowMax: col = INT(RND * colMax + 1)
      CASE LEFT:
         row = INT(RND * rowMax + 1): col = 1
      CASE RIGHT:
         row = INT(RND * rowMax + 1): col = colMax
   END SELECT
  
   IF maze(col, row) <> "X" AND maze(col, row) <> "S" THEN
      'IF ABS(startCol) - ABS(col) > INT(colMax / 8) THEN     'why doesn't
      '   IF ABS(startRow) - ABS(row) > INT(rowMax / 8) THEN  'this work???
           
            maze(col, row) = "E": endCol = col: endRow = row: validSpot = TRUE
    
     '    END IF
     ' END IF
   END IF

WEND

END SUB

SUB SolveMaze (col AS INTEGER, row AS INTEGER, success AS INTEGER)
   'Derived from psuedo-code (pascal maybe?) at
   'http://www.cs.adfa.oz.au/teaching/studinfo/ada/Recursion/maze.html

CALL HandleKeys   '-- key press detour --

IF maze(col, row) = "E" THEN success = TRUE
maze(col, row) = "+"

CALL PrintOnMaze(col, row, CHR$(4)): IF solveVsync = TRUE THEN WAIT &H3DA, 8

IF success = FALSE AND CoordInBounds(col + 1, row) = TRUE THEN
   IF maze(col + 1, row) = " " OR maze(col + 1, row) = "E" THEN
      CALL SolveMaze(col + 1, row, success)
   END IF
END IF

IF success = FALSE AND CoordInBounds(col - 1, row) = TRUE THEN
   IF maze(col - 1, row) = " " OR maze(col - 1, row) = "E" THEN
      CALL SolveMaze(col - 1, row, success)
   END IF
END IF
IF success = FALSE AND CoordInBounds(col, row - 1) = TRUE THEN
   IF maze(col, row - 1) = " " OR maze(col, row - 1) = "E" THEN
      CALL SolveMaze(col, row - 1, success)
   END IF
END IF
IF success = FALSE AND CoordInBounds(col, row + 1) = TRUE THEN
   IF maze(col, row + 1) = " " OR maze(col, row + 1) = "E" THEN
      CALL SolveMaze(col, row + 1, success)
   END IF
END IF

maze(col, row) = " "
IF success = FALSE THEN CALL PrintOnMaze(col, row, " ")  'To keep it from
                                                         'deleting itself
                                                         'on the final output.

END SUB

