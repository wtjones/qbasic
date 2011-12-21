'                                 ROOTS.BAS
'                          June '01 by Travis Jones
'
'A simple example of drawing trees using recursion
'Could be a useful tutorial, but my helper function OffsetRadPoint makes
'the program too complicated.
'  notes:
'     -so far RadPoint() isn't being used
'     -uncomment the WAIT statement in Branch() to run at full speed 
'     -even with a depth of 3000 qb didn't have a stack overflow.

DEFINT A-Z
TYPE Point2dType
   x AS INTEGER
   y AS INTEGER
END TYPE

DECLARE SUB Branch (origin AS ANY, angle AS INTEGER, deep AS INTEGER, clr AS INTEGER)
DECLARE SUB OffsetRadPoint (origin AS ANY, radius AS INTEGER, oAngle AS INTEGER, offsetAngle AS INTEGER, newAngle AS INTEGER, newPoint AS ANY)
DECLARE SUB RadPoint (ox AS INTEGER, oy AS INTEGER, radius AS INTEGER, angle AS INTEGER, newPoint AS Point2dType)
DECLARE FUNCTION InBounds% (x AS INTEGER, y AS INTEGER)

CONST TRUE = -1, FALSE = 0
CONST PI = 3.14
CONST NUMSTUMPS = 1
CONST MINLENGTH = 3
CONST MAXLENGTH = 35
CONST MAXOFFSET = 50
CONST MINOFFSET = 20
CONST MAXCHILDS = 3
CONST MAXDEEP = 150
CONST MAXDRAWN = 5   'number of trees drawn before erase

DIM SHARED tcos(0 TO 359) AS SINGLE
DIM SHARED tsin(0 TO 359) AS SINGLE
DIM p AS Point2dType

'************************ SETUP ****************
RANDOMIZE TIMER
PRINT "Creating tables..."
FOR n = 0 TO 359
  tcos(n) = COS(n * PI / 180)
  tsin(n) = SIN(n * PI / 180)
NEXT n

SCREEN 12

drawn = 0
'************************ MAIN LOOP ****************
WHILE INKEY$ = ""
   clr = INT(RND * 5 + 1)
   p.x = INT(RND * 512) + 128: p.y = INT(RND * 384) + 96
   angle = 90     'start out upright 
   CALL Branch(p, angle, MAXDEEP, clr)
   drawn = drawn + 1
   IF drawn = MAXDRAWN THEN
      drawn = 0: CLS
   END IF
WEND

END

SUB Branch (origin AS Point2dType, angle AS INTEGER, deep AS INTEGER, clr AS INTEGER)

DIM i AS INTEGER
DIM branchEnd AS Point2dType
DIM newAngle AS INTEGER
DIM numChilds AS INTEGER

length = INT(RND * MAXLENGTH) + MINLENGTH    'length of this branch, this
                                             'value will be used as a radius

offset = INT((RND * MAXOFFSET) - (MAXOFFSET / 2))
IF offset > -1 THEN
   IF offset < MINOFFSET THEN offset = MINOFFSET
   ELSE IF offset > -MINOFFSET THEN offset = -MINOFFSET
END IF

numChilds = INT(RND * MAXCHILDS) + 1

CALL OffsetRadPoint(origin, length, angle, offset, newAngle, branchEnd)

LINE (origin.x, origin.y)-(branchEnd.x, branchEnd.y), clr

WAIT &H3DA, 8    'wait for retrace

IF INKEY$ <> "" THEN END
IF deep > 0 AND InBounds(branchEnd.x, branchEnd.y) = TRUE THEN
   FOR i = 1 TO numChilds
      CALL Branch(branchEnd, newAngle, INT(RND * deep), clr)
   NEXT i
END IF

END SUB

FUNCTION InBounds (x AS INTEGER, y AS INTEGER)
IF x > 0 AND x < 640 AND y > 0 AND y < 480 THEN InBounds = TRUE ELSE InBounds = FALSE

END FUNCTION

SUB OffsetRadPoint (origin AS Point2dType, radius AS INTEGER, oAngle AS INTEGER, offsetAngle AS INTEGER, newAngle AS INTEGER, newPoint AS Point2dType)
'Takes a start point, radius to move, start angle, and an offset of
'the start angle. The newAngle is figured (purpose of this is the hide the
'loop around of the 0-359 tsin/tcos arrays), and the x and y of a new point
'is found. (using the radius).
'The SUB returns with newAngle and newPoint altered. (pass by ref)

newAngle = oAngle + offsetAngle
IF newAngle > 359 THEN newAngle = newAngle - 359
IF newAngle < 0 THEN newAngle = 359 - ABS(newAngle)

newPoint.x = origin.x + radius * tcos(newAngle)
newPoint.y = origin.y - radius * tsin(newAngle)

END SUB

DEFSNG A-Z
SUB RadPoint (ox AS INTEGER, oy AS INTEGER, radius AS INTEGER, angle AS INTEGER, newPoint AS Point2dType)
'First version of OffsetRadPoint but it isn't used.
'It just takes an origin point and figures a new point based on 'radius'
'and 'angle'.
'newPoint is altered by reference.

newPoint.x = ox + radius * tcos(angle)
newPoint.y = oy - radius * tsin(angle)

END SUB

