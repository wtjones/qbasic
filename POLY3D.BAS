'                 ********** Polygon 3D dev 3.3 **********
'
'                             December 1997
'                                   by
'                              William Jones
'
'---------- Overview --------              
'  Basically a cube you control with the keyboard. Press "f" to toggle
'between fill-modes.
'  There are countless numbers of this tired type of demo, but I am in the
'learning process and it is my duty to make one. Scroll down a couple of 
'pages to see the controls.

'---------- The story thus far... ------------
'  This demonstration is loosely based on the "VGA Trainer Program 8"
'by Denthor of Asphixia. I first ported the C version of his trainer to
'QBASIC, then wrote this.
'  In order for a program to be a tourial, it must be very readable and easy
'to understand. Because of that, his version is not optimized for speed.
'  The basic elemet was a line, which means the starting 3D point and the
'ending 3D point. There were of course 12 lines to make a Wireframe cube.
'That's 24 points to manipulated each frame of animation, 3 times as many as
'a cube has. While messing around, I connected lines in through the center.
'The more lines to be rendered, the slower it got.
'  I then re-wrote it so the basic element is a point. In the case of a cube
'there are 8 points. That is all that needs to be calculated. Each point is
'perspective projected into 2D screen coordinates and stored in an integer
'array. Then the points can be used for polygons.
'  Here's how the points are indexed:
'
'     1- - - - -  2
'    /|          /|
' 5/ -|- - - -6/  |
' |   |       |   |
' |   |       |   |
' |   3- - - -| - 4
' |  /        |  /
' 7/ - - - - -8/
'
'1 through 4 in the back, 5 through 8 in the front.
'
'  The polygon() array contains the indices of the four points that make up
'each polygon (for use in the cubePoint() array). There is of course one for
'each side of the cube. Having the polygon structure address already made
'point's instead of having it's own is probably against good practice, but
'I don't see the need to keep rendering points that in the same exact spot.
'You will notice that the only data types are for points. That is because
'QBASIC doesn't allow structures to have arrays. If I move this to C it will
'look completely different (and easier to read).

'  Each polygon is
'drawn by connecting its points. The first polygon (array element 1,X) is
'the top of the cube. Its points are 1, 2, 6 and 5. They are in that order
'(clockwise) so that when the normal is found, the outer direction is drawn
'when visible. (It doesn't make much sense to me either)
'
'---------- Controls ----------
'  -num lock must be on
'  -on numerical keypad
'     -"8" and "2" rotate on the x axis
'     -"4" and "6" rotate on the y axis
'     -"7" and "9" rotate on the z axis
'     -"5" stops all motion
'     -"0" resets values
'     -"+" and "-" move along the z perspective
'  -arrow keys change x and y values
'  -"p" toggles printing variables to screen
'  -"h" toggles hidden surface removal
'  -"f" toggles filled polygon mode: wireframe, PAINT mode, or custom filled
'
'---------- Features ----------
'  -Decent speed for QBASIC.
'  -Many aspects can be controlled with keyboard.
'  -Set 'VariableOutput' to 'true' (or press "p") to print the angle and
'     velocity of each axis.
'
'---------- Errata ----------
'  -I want to implement a freely movable 'camera'. That way I can set up a 3d
'   world and use the keyboard to wander around it. It seems the way to do it
'   would be to tranlate all objects using a constant factor when a certan
'   key is pressed, sort of like it is now but instead not having the
'   perspective fixed.
'  -Also planned is matrix mathmatics. Almost all references to 3D
'   programming have matrix content so it seems standard. It's best I learn
'   too.
'  -If you give points decimal values, 'CubePoints()' must be declared of
'   type 'Point3d' instead of 'Point3dInt'. (another picky optimization)
'
'---------- Updates ----------
'  Started out as an ugly and slow hack of Denthor's tut8.
'  Febuary 1997
'     -Finished dev 2
'  April 19, 1997
'     -Cleaned code/typos and such. Now called dev 2.1.
'  November 14, 1997
'     -The line-connecting system is gone. Four points can be declared as
'        a polygon to be drawn (conneced with lines).
'     -Hidden surface removal is added. Only visible polygon surfaces will be
'        drawn/outlined. Press "h" to turn it off.
'     -When moving the cube with the arrow keys and plus and minus, the cube
'        doesn't rotate around the same axis like before. The points are
'        first rotated and then added to a base to fix this. xLoc, yLoc, zLoc
'        is the base. If anybody knows why it moves so jumpy please tell me.
'     -Program is now 3.0
'  November 17, 1997
'     -Added filled polygon mode. Press "f" to use. I might put in a fill
'        routine, but for now the PAINT statement goes outside the lines.
'        I don't know how to fix it, but it's annoying.
'     -The frame rate is now printed at the bottom of the screen, but the
'        numbers don't seem right. When I turn on fill mode it feels
'        sluggish, but I still get an FPS of 90 or more.
'     -Program is now 3.1 (who cares?)
'  November 23, 1997
'     -Added a rasterized polygon filling SUB. I adopted it from Denthor's
'        tut9. Using the regular GOSUB method (like I prefer in BASIC) would
'        be too much of a mess for a routine that needs 9 perimeters. I have
'        never gotten the hang of or been comfortable using SUBs, and I am
'        still not. I kept most of his variable names in the SUB, so it
'        doesn't look like it fits in this program.
'     -Pressing "f" now toggles between the 3 fill modes.
'     -Program is now 3.2...  I apologize for the weird loop indices. I
'        started this quite a while ago and my coding style keeps changing.
'        I also notices that the PAINT statement sometimes leaks and fills
'        the screen. Please email me if you have a fix for it.
'  December 12, 1997
'      -Fixed the frame rate problem. I guess you HAVE to use floating point.


'|---------- CONSTANTS ------------------------|
CONST pi = 3.141592     'single precision for speed
CONST true = -1, false = 0
CONST NumPoints = 8     'the original 8 make a cube, you can easily add more
CONST NumPolygons = 6   '6 faces of a cube
CONST XOff = 160        'VGA sceen offsets for projection
CONST YOff = 100
CONST ZOff = -60

CONST HScale = 256      'change these to make it look really weird
CONST VScale = 256
CONST OutlineColor = 15
CONST Outlined = 0
CONST Filled = 1
CONST Painted = 2

'|---------- DATA TYPES -----------------------|
TYPE Point2DType         'VGA screen coordinates
   x AS INTEGER
   y AS INTEGER
END TYPE
TYPE Point3dType         'floating point values for translation equations
   x AS SINGLE
   y AS SINGLE
   z AS SINGLE
END TYPE
TYPE Point3dIntType  'used for 3d offsets from DATA if they are whole numbers
   x AS INTEGER
   y AS INTEGER
   z AS INTEGER
END TYPE

'|---------- SUBS -----------------------------|
DECLARE SUB PolyFill (x1 AS INTEGER, y1 AS INTEGER, x2 AS INTEGER, y2 AS INTEGER, x3 AS INTEGER, y3 AS INTEGER, x4 AS INTEGER, y4 AS INTEGER, clr AS INTEGER)

'|---------- VARIABLE/ARRAY  DECLARATIONS -----|
DIM n AS INTEGER, m AS INTEGER      'looping indices

DIM polygons(1 TO NumPolygons, 1 TO 4) AS Point2DType    'point references
DIM PolyColors(1 TO NumPolygons) AS INTEGER
DIM visiblePolys(1 TO NumPolygons) AS INTEGER   'true or false
DIM cubePoint(1 TO NumPoints) AS Point3dIntType  'loaded from DATA
DIM cubeVidPoint(1 TO NumPoints) AS Point2DType  'projected  2D points
DIM ccos(0 TO 359) AS SINGLE, csin(0 TO 359) AS SINGLE
DIM trans(1 TO NumPoints) AS Point3dType   'final translated points
DIM boxErase(1 TO 2) AS Point2DType  'holds the immediate box around the cube
DIM xDeg AS INTEGER, yDeg AS INTEGER, zDeg AS INTEGER    'axis degrees
DIM xRate AS INTEGER, yRate AS INTEGER, zRate AS INTEGER 'axis velocities
DIM xLoc AS INTEGER, yLoc AS INTEGER, zLoc AS INTEGER

'-temps
DIM midPoint AS Point2DType   'for finding mid-points of polygons
DIM temp AS Point3dType  'temporary buffer for translation
DIM timeTemp AS SINGLE
DIM nextSec AS SINGLE

'-boolean flags-
DIM done AS INTEGER                 'boolean for escape key
DIM variableOutput AS INTEGER       'boolean to print angle/velocity
DIM wireFrame AS INTEGER            'false for just dots
DIM hsr AS INTEGER               'hidden surface removal
DIM fillMode AS INTEGER          'Outlined, Painted, or Filled

'|---------- SETUP ----------------------------|
DEFINT A-Z  'all variables integers by default
  'load nessesary values of sine and cosine into lookup tables
RANDOMIZE TIMER
PRINT "Creating tables..."
FOR n = 0 TO 359
  ccos(n) = COS(n * pi / 180)
  csin(n) = SIN(n * pi / 180)
NEXT n
   'read in point offsets from data
RESTORE CubeData
FOR n = 1 TO NumPoints
   READ cubePoint(n).x
   READ cubePoint(n).y
   READ cubePoint(n).z
NEXT
   'read in the indices of the 4 points each polygon consists of
RESTORE PolygonData
FOR n = 1 TO NumPolygons
   FOR m = 1 TO 4
      READ polygon(n, m)
   NEXT
NEXT

FOR n = 1 TO NumPolygons   'give the sides random colors for fill modes
   PolyColors(n) = INT(RND * 16 + 1)
NEXT

xDeg = 0: yDeg = 0: zDeg = 0     'degrees to refrence sin/cos tables,
                                 'each will be in range of 0-359

xRate = 0: yRate = 0: zRate = 0  'degree incements
variableOutput = false
wireFrame = false
hsr = true  'hidden surface removal on
fillMode = Outline   'Outline, Painted, or Filled

'|---------------------------------------------|
'|---------- MAIN PROGRAM ---------------------|
'|---------------------------------------------|

SCREEN 7, , 1, 0        'page 0 is visual, page 1 active
timeTemp = INT(TIMER): nextSec = timeTemp + 1
DO
   GOSUB RotatePoints
   GOSUB ProjectPoints  'produce 2D coordinates ready for drawing
   GOSUB FindVisibleSides
  
   '-do all drawing-
   IF variableOutput = true THEN GOSUB PrintOut 'angle/velocity                                                                                  '''''
  
   '-draw the visible sides of the polygon-
   FOR curPoly = 1 TO 6
      IF visiblePolys(curPoly) = true THEN
         SELECT CASE fillMode
            CASE Outlined     'just the basic wireframe
               GOSUB OutlinePolygon
            CASE Filled       'use polygon filling routine
               CALL PolyFill(cubeVidPoint(polygon(curPoly, 1)).x, cubeVidPoint(polygon(curPoly, 1)).y, cubeVidPoint(polygon(curPoly, 2)).x, cubeVidPoint(polygon(curPoly, 2)).y, cubeVidPoint(polygon(curPoly, 3)).x, cubeVidPoint(polygon(curPoly, 3)).y, cubeVidPoint(polygon(curPoly, 4)).x, cubeVidPoint(polygon(curPoly, 4)).y, PolyColors(curPoly))
            CASE Painted      'use PAINT statement
                  GOSUB OutlinePolygon 'PAINT requires an outline
                  GOSUB PaintPolygon
            END SELECT
      END IF
   NEXT
   '-done drawing polygon-

   PCOPY 1, 0           'copy active page to visual
   GOSUB UpdateFrameRate
   GOSUB UpdateFillMode 'print the current fill mode onscreen
   '-do all erasing-
   GOSUB EraseCube    'fill cube's bounding box with black
  
   IF variableOutput = true THEN GOSUB ErasePrint
  
   GOSUB GetKey
   GOSUB Motion         'inc/dec angles based on rates
LOOP WHILE done = false
END

'|---------------------------------------------|
'|---------- END MAIN PROGRAM -----------------|
'|---------------------------------------------|


RotatePoints:
   FOR n = 1 TO NumPoints
      trans(n).x = cubePoint(n).x 'restore original cube data
      trans(n).y = cubePoint(n).y
      trans(n).z = cubePoint(n).z
      GOSUB DoXAxis                     'translation equations
      GOSUB DoYAxis
      GOSUB DoZAxis
      trans(n).x = xLoc + temp.x
      trans(n).y = yLoc + temp.y
      trans(n).z = zLoc + temp.z
  NEXT
RETURN

DoXAxis:
   '-rotate on x axis-
   'x is unaltered
   temp.y = ccos(xDeg) * trans(n).y - csin(xDeg) * trans(n).z
   temp.z = csin(xDeg) * trans(n).y + ccos(xDeg) * trans(n).z
  
   '-put new values into translated array-
   'x is unaltered
   trans(n).y = temp.y
   trans(n).z = temp.z
RETURN

DoYAxis:
   '-rotate on y axis-
   temp.x = ccos(yDeg) * trans(n).x + csin(yDeg) * trans(n).z
   'y is unaltered
   temp.z = (-csin(yDeg)) * trans(n).x + ccos(yDeg) * trans(n).z
  
   '-put new values into translated array-
   trans(n).x = temp.x
   'y is unaltered
   trans(n).z = temp.z
RETURN

DoZAxis:
   '-rotate on z axis-
   temp.x = ccos(zDeg) * trans(n).x - csin(zDeg) * trans(n).y
   temp.y = csin(zDeg) * trans(n).x + ccos(zDeg) * trans(n).y
   'z is unaltered
  
   '-put new values into translated array-
   trans(n).x = temp.x
   trans(n).y = temp.y
   'z is unaltered
RETURN

FindVisibleSides:
   IF hsr = true THEN
      FOR p = 1 TO 6
         zNorm1 = (cubeVidPoint(polygon(p, 2)).x - cubeVidPoint(polygon(p, 1)).x) * (cubeVidPoint(polygon(p, 1)).y - cubeVidPoint(polygon(p, 3)).y)
         zNorm2 = (cubeVidPoint(polygon(p, 2)).y - cubeVidPoint(polygon(p, 1)).y) * (cubeVidPoint(polygon(p, 1)).x - cubeVidPoint(polygon(p, 3)).x)
         zNorm = zNorm1 - zNorm2
            '-10 is used instead of 0 to prevent PaintPolygon from leaking
            'out and filling the screen
         IF zNorm < -10 THEN visiblePolys(p) = true ELSE visiblePolys(p) = false
      NEXT
   ELSE
      FOR curPoly = 1 TO 6
         visiblePolys(curPoly) = true
      NEXT
   END IF
RETURN

PaintPolygon:     'routine to paint poly specified by 'curPoly' with PAINT
                  'statement
   FOR j = 1 TO 4    'get the sum of the x values of the 4 points
      total = total + cubeVidPoint(polygon(curPoly, j)).x
   NEXT
   midPoint.x = total / 4     'divide to get the average
   total = 0
                     'do the same for y
   FOR j = 1 TO 4
      total = total + cubeVidPoint(polygon(curPoly, j)).y
   NEXT
   midPoint.y = total / 4
   total = 0
  
   'paint smack-dab in the center of the polygon
   PAINT (midPoint.x, midPoint.y), PolyColors(curPoly), OutlineColor
RETURN

Motion:     'make sure degrees are in range of look-up tables
   xDeg = xDeg + xRate
   IF xDeg < 0 THEN xDeg = 359 + xDeg           'add the negative value to
   IF xDeg > 359 THEN xDeg = 0 + (xDeg - 359)   'be more accurate
     
   yDeg = yDeg + yRate
   IF yDeg < 0 THEN yDeg = 359 + yDeg
   IF yDeg > 359 THEN yDeg = 0 + (yDeg - 359)
                
   zDeg = zDeg + zRate
   IF zDeg < 0 THEN zDeg = 359 + zDeg
   IF zDeg > 359 THEN zDeg = 0 + (zDeg - 359)
RETURN

ProjectPoints:
   'project 3d coordinates into 2d integer screen coordinates suitible for
   'VGA and store them in an array
   FOR n = 1 TO NumPoints
      project = trans(n).z + ZOff
      cubeVidPoint(n).x = ((HScale * trans(n).x) / project) + XOff
      cubeVidPoint(n).y = ((VScale * trans(n).y) / project) + YOff
   NEXT
RETURN

EraseCube:
   FOR n = 1 TO 2
      boxErase(n).x = 160: boxErase(n).y = 100  'start from center of screen
   NEXT
   FOR n = 1 TO NumPoints
      IF cubeVidPoint(n).x < boxErase(1).x THEN boxErase(1).x = cubeVidPoint(n).x
      IF cubeVidPoint(n).x > boxErase(2).x THEN boxErase(2).x = cubeVidPoint(n).x
      IF cubeVidPoint(n).y < boxErase(1).y THEN boxErase(1).y = cubeVidPoint(n).y
      IF cubeVidPoint(n).y > boxErase(2).y THEN boxErase(2).y = cubeVidPoint(n).y
   NEXT
   'fill in bouding box with black
   LINE (boxErase(1).x, boxErase(1).y)-(boxErase(2).x, boxErase(2).y), 0, BF
RETURN

OutlinePolygon:   'routine to draw polygon specified by 'curpoly' with the
                  'LINE statement
   FOR i = 1 TO 3
      LINE (cubeVidPoint(polygon(curPoly, i)).x, cubeVidPoint(polygon(curPoly, i)).y)-(cubeVidPoint(polygon(curPoly, i + 1)).x, cubeVidPoint(polygon(curPoly, i + 1)).y), OutlineColor
   NEXT
   LINE (cubeVidPoint(polygon(curPoly, 4)).x, cubeVidPoint(polygon(curPoly, 4)).y)-(cubeVidPoint(polygon(curPoly, 1)).x, cubeVidPoint(polygon(curPoly, 1)).y), OutlineColor
RETURN

UpdateFrameRate:  'I don't know if these numbers are right
   frameRate = frameRate + 1
   timeTemp = TIMER
   IF timeTemp > nextSec THEN    'one full second has passed
      LOCATE 23, 1:  PRINT "FPS:"; frameRate
      frameRate = 0
      timeTemp = TIMER
      nextSec = timeTemp + 1
   END IF
RETURN

UpdateFillMode:
   SELECT CASE fillMode
      CASE Outlined
         LOCATE 23, 16: PRINT "                       "
      CASE Filled
         LOCATE 23, 16: PRINT "Filled using:  PolyFill"
      CASE Painted
         LOCATE 23, 16: PRINT "Filled using:  PAINT   "
   END SELECT
RETURN

GetKey:
   k$ = INKEY$
   SELECT CASE k$
      CASE "8"
         xRate = xRate - 1
      CASE "2"
         xRate = xRate + 1
      CASE "4"
         yRate = yRate + 1
      CASE "6"
         yRate = yRate - 1
      CASE "7"
         zRate = zRate - 1
      CASE "9"
         zRate = zRate + 1
      CASE "5"
         xRate = 0: yRate = 0: zRate = 0
      CASE "+"
         FOR n = 1 TO NumPoints
            zLoc = zLoc + 1
         NEXT
      CASE "-"
         FOR n = 1 TO NumPoints
            zLoc = zLoc - 1
         NEXT
      CASE CHR$(0) + "K"     'left
         FOR n = 1 TO NumPoints
            xLoc = xLoc + 1
         NEXT
      CASE CHR$(0) + "M"    'right
         FOR n = 1 TO NumPoints
            xLoc = xLoc - 1
         NEXT
      CASE CHR$(0) + "H"     'up
         FOR n = 1 TO NumPoints
            yLoc = yLoc + 1
         NEXT
      CASE CHR$(0) + "P"     'down
         FOR n = 1 TO NumPoints
            yLoc = yLoc - 1
         NEXT
      CASE "0"
         xRate = 0: yRate = 0: zRate = 0
         xDeg = 0: yDeg = 0: zDeg = 0
         xLoc = 0: yLoc = 0: zLoc = 0
         RESTORE CubeData
         FOR n = 1 TO NumPoints
            READ cubePoint(n).x
            READ cubePoint(n).y
            READ cubePoint(n).z
         NEXT
      CASE "p"
         IF variableOutput = true THEN
            variableOutput = false
         ELSE
         IF variableOutput = false THEN variableOutput = true
         END IF
      CASE "h"
         IF hsr = true THEN
            hsr = false
         ELSE
            IF hsr = false THEN hsr = true
         END IF
      CASE "f"
         fillMode = fillMode + 1
         IF fillMode = 3 THEN
            fillMode = 0: CLS    'in case PAINT leaked
         END IF
      CASE "c"
         CLS
      CASE CHR$(27)
         done = true
   END SELECT
RETURN

PrintOut:
   LOCATE 2, 2: PRINT "X:"
   LOCATE 2, 4: PRINT xDeg
   LOCATE 2, 8: PRINT " at rate of "
   LOCATE 2, 19: PRINT xRate
  
   LOCATE 3, 2: PRINT "Y:"
   LOCATE 3, 4: PRINT yDeg
   LOCATE 3, 8: PRINT " at rate of "
   LOCATE 3, 19: PRINT yRate
  
   LOCATE 4, 2: PRINT "Z:"
   LOCATE 4, 4: PRINT zDeg
   LOCATE 4, 8: PRINT " at rate of "
   LOCATE 4, 19: PRINT zRate
RETURN

ErasePrint:
   LOCATE 2, 2: PRINT "             "
   LOCATE 3, 2: PRINT "             "
   LOCATE 4, 2: PRINT "             "
RETURN

CubeData:
DATA 10,10,-10
DATA -10,10,-10
DATA 10,-10,-10
DATA -10,-10,-10

DATA 10,10,10
DATA -10,10,10
DATA 10,-10,10
DATA -10,-10,10

PolygonData:
'top
DATA 1,2,6,5
'front
DATA 5,6,8,7
'bottom
DATA 7,8,4,3
'back
DATA 2,1,3,4
'left
DATA 1,5,7,3
'right
DATA 6,2,4,8

DEFSNG A-Z
SUB PolyFill (x1 AS INTEGER, y1 AS INTEGER, x2 AS INTEGER, y2 AS INTEGER, x3 AS INTEGER, y3 AS INTEGER, x4 AS INTEGER, y4 AS INTEGER, clr AS INTEGER)
   DEFINT A-Z
  'Ported from tut9 of Asphixia's demos. I am still trying to adopt it into
  'my program and make it faster.

  ' find the maximum y (mny) and minimum y (mny)
              mny = y1
              mxy = y1
  IF y2 < mny THEN mny = y2
  IF y2 > mxy THEN mxy = y2
  IF y3 < mny THEN mny = y3
  IF y3 > mxy THEN mxy = y3
  IF y4 < mny THEN mny = y4
  IF y4 > mxy THEN mxy = y4

 ' if the mimimum or maximum is out of bounds, bring it back in
  IF mny < 0 THEN mny = 0
  IF mxy > 199 THEN mxy = 199

 ' verticle range checking
  IF mny > 199 THEN END
  IF mxy < 0 THEN END

' constants needed for intersection calculations
  mul1 = x1 - x4: div1 = y1 - y4
  mul2 = x2 - x1: div2 = y2 - y1
  mul3 = x3 - x2: div3 = y3 - y2
  mul4 = x4 - x3: div4 = y4 - y3

yc = mny
WHILE yc < mxy
   mnx = 320
   mxx = -1

   IF y4 >= yc OR y1 >= yc THEN
      IF y4 <= yc OR y1 <= yc THEN
         IF y4 <> y1 THEN
            x = ((yc - y4) * mul1 / div1) + x4
            IF x < mnx THEN mnx = x
            IF x > mxx THEN mxx = x
         END IF
      END IF
   END IF


   IF y1 >= yc OR y2 >= yc THEN
      IF y1 <= yc OR y2 <= yc THEN
         IF y1 <> y2 THEN
            x = ((yc - y1) * mul2 / div2) + x1
            IF x < mnx THEN mnx = x
            IF x > mxx THEN mxx = x
         END IF
      END IF
   END IF

   IF y2 >= yc OR y3 >= yc THEN
      IF y2 <= yc OR y3 <= yc THEN
         IF y2 <> y3 THEN
            x = ((yc - y2) * mul3 / div3) + x2
            IF x < mnx THEN mnx = x
            IF x > mxx THEN mxx = x
         END IF
      END IF
   END IF

   IF y3 >= yc OR y4 >= yc THEN
      IF y3 <= yc OR y4 <= yc THEN
         IF y3 <> y4 THEN
            x = ((yc - y3) * mul4 / div4) + x3
            IF x < mnx THEN mnx = x
            IF x > mxx THEN mxx = x
         END IF
      END IF
   END IF
  
   'horizontal range checking
   IF mnx < 0 THEN mnx = 0
   IF mxx > 319 THEN mxx = 319

   IF mnx <= mxx THEN
      LINE (mnx, yc)-(mxx, yc), clr ' draw the horizontal line
   END IF
   yc = yc + 1   'go down a line
   WEND
END SUB

