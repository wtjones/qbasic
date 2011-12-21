'Walkthru dev 3.1
'June 1997
'Overhead map view. Each cell is 10*10.
'Here is how it works...
'  The map data is read into the 'grid' array, x and y starting from 1
'to the max of the map. Element 0 and 1 past the max of both x and y are left
'zero so I don't overflow when looking for a possible wall(zero = no wall).
'  All of the cells are translated from the screen origin and scaled to use
'onscreen, then stored in the 'gridVid' array, which has the same dimentions
'as 'grid'. 4 loops first scan the boundarys so see what of the 'gridVid'
'array needs to be filled.
'  Lines are then drawn to make the boxes to represent walls.
'  There are 3 maps. Whatever 'RESTORE' line is uncommented will select it.
'

'numeric keypad moves
'"=" and "-" scale in and out
'"f" toggles fill mode - off/solid/crossed
'---May 97 dev 2
'---May 97 dev 3 localizes translations

DEFINT A-Z
CONST CellXMax = 10        'these are the dimentions of each cell, the number
CONST CellYMax = CellXMax  'of possible positions in a cell
CONST GridOff = CellXMax   'these 3 numbers should all be the same

'-------
TYPE ObjectType
   xCell AS INTEGER
   yCell AS INTEGER
   xGrid AS INTEGER
   yGrid AS INTEGER
   xVid  AS INTEGER
   yVid  AS INTEGER
END TYPE
TYPE Point2dType
   x AS INTEGER
   y AS INTEGER
   clr AS INTEGER    'color
END TYPE
'looping indices, d is usually x and e is usually y
DIM d AS INTEGER, e AS INTEGER, f AS INTEGER
DIM xMax AS INTEGER
DIM yMax  AS INTEGER
'holds map from data, the 0 elements are left a value of 0 for border routine
DIM grid(0 TO 100, 0 TO 100) AS INTEGER
'holds translated 2d coords of each cell, the 4 points of a cell are obtained
'by looking at adjacent cells
DIM gridVid(0 TO 100, 0 TO 100) AS Point2dType
DIM pl AS ObjectType
DIM vidTempX AS INTEGER, vidTempY AS INTEGER
DIM scale AS SINGLE
DIM fill AS INTEGER
DIM printOn AS INTEGER

'--------
pl.xVid = 160: pl.yVid = 100
scale = 2
fill = 0
printOn = 0
'RESTORE small
RESTORE big
'RESTORE huge
READ xMax
READ yMax
READ pl.xGrid
READ pl.yGrid
FOR e = 1 TO yMax
   FOR d = 1 TO xMax
      READ grid(d, e)
   NEXT
NEXT
pl.xCell = 5   'middle of cell
pl.yCell = 5


'|---------------------------------------------|
'|---------- MAIN PROGRAM ---------------------|
'|---------------------------------------------|

SCREEN 7, , 1, 0     'use 0 for visual
WHILE done = 0
   LINE (0, 0)-(319, 199), 0, BF    'clear active page
   LINE (0, 0)-(319, 199), 1, B     'blue border
  
   CIRCLE (pl.xVid, pl.yVid), 1, 4
   GOSUB DrawWalls
  
   IF printOn = 1 THEN GOSUB PrintOut
   PCOPY 1, 0
   GOSUB MovePlayer
WEND
END

'|---------------------------------------------|
'|---------- END MAIN PROGRAM -----------------|
'|---------------------------------------------|


Translate:
   'To use global coords on a local screen you do this...
   'enemyVidX = (enemyGlobalX - playerGlobalX) + playerVidX
     
      'don't multiply if unneccesary
'      IF scale = 1 THEN
'         xScaleOff = ((d * GridOff) - (xConvert))
'         yScaleOff = ((e * GridOff) - (yConvert))
'      ELSE
         xScaleOff = ((d * GridOff) - (xConvert)) * scale
         yScaleOff = ((e * GridOff) - (yConvert)) * scale
'      END IF

'      xWallVid = ((d * GridOff) - (xConvert)) * scale + pl.xVid
'      yWallVid = ((e * GridOff) - (yConvert)) * scale + pl.yVid
      gridVid(d, e).x = xScaleOff + pl.xVid
      gridVid(d, e).y = yScaleOff + pl.yVid
RETURN

TranslateX:
      'don't multiply if unneccesary
'   IF scale = 1 THEN
'      xScaleOff = ((d * GridOff) - (xConvert))
'   ELSE
      xScaleOff = ((d * GridOff) - (xConvert)) * scale
'   END IF

   gridVid(d, e).x = xScaleOff + pl.xVid
   gridVid(d, e).y = yTemp
RETURN
TranslateY:
      'don't multiply if unneccesary
'      IF scale = 1 THEN
'         yScaleOff = ((e * GridOff) - (yConvert))
'      ELSE
         yScaleOff = ((e * GridOff) - (yConvert)) * scale
'      END IF

      gridVid(d, e).y = yScaleOff + pl.yVid
      gridVid(d, e).x = xTemp

RETURN

DrawWalls:
   'get global coordinates from local (grid and cell)
   xConvert = (pl.xGrid * CellXMax) + pl.xCell
   yConvert = (pl.yGrid * CellYMax) + pl.yCell
   xBoundL = 0: xBoundR = 0: yBoundU = 0: yBoundD = 0
   '-------find the four boundarys to fill the gridVid array
   'find left boundary
   e = pl.yGrid   'y is the same for left and right
   d = pl.xGrid
   GOSUB Translate 'translate the players spot
   yTemp = gridVid(d, e).y 'we need only translate the x for the left
                           'boundary and right boundary loops
  FOR d = pl.xGrid - 1 TO 1 STEP -1
      GOSUB TranslateX
      IF gridVid(d, e).x < 0 THEN
         xBoundL = d: EXIT FOR
      END IF
      xBoundL = 1
   NEXT d
  
   'find right boundary
   FOR d = pl.xGrid + 1 TO xMax + 1
      GOSUB TranslateX
         IF gridVid(d, e).x > 319 THEN
            xBoundR = d:
            EXIT FOR
         END IF
      xBoundR = xMax + 1
   NEXT d
   '-top and bottom loops-
   'find top boundary
   d = pl.xGrid   'x is the same for both top and bottom
   e = pl.yGrid
'   GOSUB Translate
  
   xTemp = gridVid(d, e).x 'we need only translate the Y for the top
                           'boundary and bottom boundary loops

   FOR e = pl.yGrid - 1 TO 1 STEP -1
      GOSUB TranslateY
         IF gridVid(d, e).y < 0 THEN
            yBoundU = e: EXIT FOR
         END IF
      yBoundU = 1
   NEXT e
  
   'find bottom boundary
   FOR e = pl.yGrid + 1 TO yMax + 1
      GOSUB TranslateY
         IF gridVid(d, e).y > 199 THEN
            yBoundD = e
            EXIT FOR
         END IF
   yBoundD = xMax + 1
   NEXT e

'-translate rest of necssary points-
'top left corner
FOR e = yBoundU TO pl.yGrid - 1
   GOSUB TranslateY
   yTemp = gridVid(d, e).y
   FOR d = xBoundL TO pl.xGrid - 1
   GOSUB TranslateX
   NEXT
NEXT

'top right corner
FOR e = yBoundU TO pl.yGrid - 1
   GOSUB TranslateY
   yTemp = gridVid(d, e).y
   FOR d = pl.xGrid + 1 TO xBoundR
   GOSUB TranslateX
   NEXT
NEXT

'bottom left corner
FOR e = pl.yGrid + 1 TO yBoundD
   GOSUB TranslateY
   yTemp = gridVid(d, e).y
   FOR d = xBoundL TO pl.xGrid - 1
   GOSUB TranslateX
   NEXT
NEXT

'bottom right corner
FOR e = pl.yGrid + 1 TO yBoundD
   GOSUB TranslateY
   yTemp = gridVid(d, e).y
   FOR d = pl.xGrid + 1 TO xBoundR
   GOSUB TranslateX
   NEXT
NEXT

'FOR e = yBoundU TO yBoundD
'   GOSUB TranslateY
'   yTemp = gridVid(d, e).y
'   FOR d = xBoundL TO xBoundR
'      GOSUB TranslateX
'      'IF d = pl.xGrid THEN EXIT FOR
'   NEXT d
'NEXT e

'draw translated points
FOR e = yBoundU TO yBoundD - 1
   FOR d = xBoundL TO xBoundR - 1
      IF grid(d, e) = 1 THEN
      vidTempX = gridVid(d, e).x    'save temps of what we can (can't when
      vidTempY = gridVid(d, e).y    'looking at adjacent cells ie d+1)
         'clip when possible
'         IF vidTempX < 320 AND vidTempY < 200 AND gridVid(d + 1, e).x > -1 AND gridVid(d, e + 1).y > -1 THEN
            'fill mode
            IF fill = 1 THEN
               LINE (vidTempX, gridVid(d, e).y)-(gridVid(d + 1, e + 1).x, gridVid(d + 1, e + 1).y), 1, BF
            END IF
            IF fill = 2 THEN
               LINE (vidTempX, gridVid(d, e).y)-(gridVid(d + 1, e + 1).x, gridVid(d + 1, e + 1).y), 4
               LINE (gridVid(d, e + 1).x, gridVid(d, e + 1).y - 1)-(gridVid(d + 1, e).x, gridVid(d + 1, e).y), 4
            END IF
           
            '-----draw borders if no neighboring wall
            IF grid(d, e - 1) = 0 THEN    'top
              LINE (gridVid(d, e).x, gridVid(d, e).y)-(gridVid(d + 1, e).x, gridVid(d + 1, e).y)
            END IF
            IF grid(d + 1, e) = 0 THEN    'right
               LINE (gridVid(d + 1, e).x, gridVid(d + 1, e).y)-(gridVid(d + 1, e + 1).x, gridVid(d + 1, e + 1).y)
               END IF
           IF grid(d, e + 1) = 0 THEN     'bottom
               LINE (gridVid(d, e + 1).x, gridVid(d, e + 1).y)-(gridVid(d + 1, e + 1).x, gridVid(d + 1, e + 1).y)
               END IF
           IF grid(d - 1, e) = 0 THEN     'left
               LINE (gridVid(d, e).x, gridVid(d, e).y)-(gridVid(d, e + 1).x, gridVid(d, e + 1).y)
               END IF
       
'lines shortened (disabled)
 'LINE (vidTempX + 1, gridVid(d, e).y + 1)-(gridVid(d + 1, e + 1).x - 1, gridVid(d + 1, e + 1).y - 1), 4
 'LINE (gridVid(d, e + 1).x + 1, gridVid(d, e + 1).y - 1)-(gridVid(d + 1, e).x - 2, gridVid(d + 1, e).y + 1), 4
         END IF
'      END IF
  NEXT
NEXT
RETURN


MovePlayer:
   k$ = INKEY$
   SELECT CASE k$
      CASE "4"
         pl.xCell = pl.xCell - 1
         IF pl.xCell = 0 THEN
            IF grid(pl.xGrid - 1, pl.yGrid) = 0 THEN
               pl.xCell = CellXMax: pl.xGrid = pl.xGrid - 1
            ELSE pl.xCell = 1
            END IF
         END IF
         CLS
      CASE "6"
         pl.xCell = pl.xCell + 1
        IF pl.xCell = CellXMax + 1 THEN
            IF grid(pl.xGrid + 1, pl.yGrid) = 0 THEN
           
            pl.xCell = 1: pl.xGrid = pl.xGrid + 1
         ELSE pl.xCell = CellXMax
         END IF
         END IF
         CLS
      CASE "8"
         pl.yCell = pl.yCell - 1
         IF pl.yCell = 0 THEN
            IF grid(pl.xGrid, pl.yGrid - 1) = 0 THEN
           
            pl.yCell = CellYMax: pl.yGrid = pl.yGrid - 1
         ELSE pl.yCell = 1
         END IF
         END IF
         CLS
      CASE "2"
         pl.yCell = pl.yCell + 1
         IF pl.yCell = CellYMax + 1 THEN
            IF grid(pl.xGrid, pl.yGrid + 1) = 0 THEN
           
            pl.yCell = 1: pl.yGrid = pl.yGrid + 1
         ELSE pl.yCell = CellYMax
         END IF
         END IF
         CLS
      CASE "="
         scale = scale + .1
         CLS
      CASE "-"
         scale = scale - .1
         CLS
      CASE "f"
         fill = fill + 1
         IF fill = 3 THEN fill = 0
      CASE "p"
         IF printOn = 1 THEN
         printOn = 0
         ELSE IF printOn = 0 THEN printOn = 1
         END IF
      CASE CHR$(27)
         done = 1
   END SELECT
FOR d = 1 TO 3
k$ = INKEY$
NEXT
RETURN

PrintOut:
   LOCATE 1, 1: PRINT "Grid :"; pl.xGrid; pl.yGrid
   LOCATE 2, 1: PRINT "Cell :"; pl.xCell; pl.yCell
   LOCATE 4, 1: PRINT "Scale:"; scale
   LOCATE 5, 1: PRINT xBoundL; xBoundR; yBoundU; yBoundD
RETURN
'x any y total

'-----------map data----------
big:
'grid size
DATA 20,20
'player start
DATA 10,10
'grid
DATA 1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1
DATA 1,0,1,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1
DATA 1,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1
DATA 1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1
DATA 1,1,1,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1
DATA 1,1,1,1,1,1,1,1,0,1,1,1,1,1,1,1,1,1,1,1
DATA 1,1,1,1,1,1,1,1,0,1,1,1,1,1,1,1,1,1,1,1
DATA 1,1,1,1,1,1,1,1,0,1,1,1,1,1,1,1,1,1,1,1
DATA 1,1,1,1,1,1,1,1,0,1,1,1,1,1,1,1,1,1,1,1
DATA 1,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1
DATA 1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1
DATA 1,1,1,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1
DATA 1,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,1,1,0,1
DATA 1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,1
DATA 1,1,1,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1
DATA 1,1,1,1,1,1,1,1,0,1,1,1,1,1,1,1,1,1,1,1
DATA 1,1,1,1,1,1,1,1,0,1,1,1,1,1,1,1,1,1,1,1
DATA 1,1,1,1,1,1,1,1,0,1,1,1,1,1,1,1,1,1,1,1
DATA 1,1,1,1,1,1,1,1,0,1,1,1,1,1,1,1,1,1,1,1
DATA 1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1

small:
DATA 5,5
'player start
DATA 3,3
DATA 1,1,1,1,1
DATA 1,0,0,0,1
DATA 1,0,0,0,1
DATA 1,0,0,0,1
DATA 1,1,1,1,1

huge:
DATA 50,50
DATA 49,49

DATA 1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1
DATA 1,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,1
DATA 1,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,1
DATA 1,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,1
DATA 1,0,1,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,1
DATA 1,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,1
DATA 1,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,1
DATA 1,0,1,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,1
DATA 1,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,1
DATA 1,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,1
DATA 1,0,1,0,1,0,0,1,1,1,1,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1
DATA 1,0,0,0,1,0,0,1,0,0,0,0,0,0,0,0,0,0,0,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,1
DATA 1,0,0,0,1,0,0,1,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,1
DATA 1,0,0,0,1,0,0,1,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,1
DATA 1,0,1,0,1,0,0,0,0,0,1,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,1
DATA 1,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,1
DATA 1,0,0,0,1,0,0,0,0,0,1,0,0,0,0,0,0,0,0,1,0,1,1,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,1
DATA 1,0,1,0,1,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,1
DATA 1,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,1
DATA 1,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,1
DATA 1,0,1,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1
DATA 1,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,1
DATA 1,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,1
DATA 1,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,1
DATA 1,0,1,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,1
DATA 1,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,1
DATA 1,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,1
DATA 1,0,1,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,1
DATA 1,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,1
DATA 1,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,1
DATA 1,0,1,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1
DATA 1,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,1
DATA 1,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,1
DATA 1,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,1
DATA 1,0,1,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,1
DATA 1,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,1
DATA 1,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,1
DATA 1,0,1,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,1
DATA 1,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,1
DATA 1,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,1
DATA 1,0,1,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1
DATA 1,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,1
DATA 1,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,1
DATA 1,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,1
DATA 1,0,1,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,1
DATA 1,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,1
DATA 1,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,1
DATA 1,0,1,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,1
DATA 1,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,1
DATA 1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1

