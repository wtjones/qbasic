'Sprite masking demo.
'April 1997 William Jones

DEFINT A-Z
RANDOMIZE TIMER
SCREEN 7

CONST NumBugs = 50
CONST Delay = 1
CONST VPage = 0   'visual page
CONST APage = 1   'active page
CONST BPage = 2   'background page
TYPE BugData
   x AS INTEGER
   y AS INTEGER
   dir AS INTEGER
   dist AS INTEGER
   speed AS INTEGER
   wing AS INTEGER
   curFrame AS INTEGER
   clr AS INTEGER
END TYPE
DIM tile(641)
'f is frame, m is mask
DIM bugf1(1 TO 15) AS INTEGER
DIM bugm1(1 TO 15) AS INTEGER

DIM bugf2(1 TO 15) AS INTEGER
DIM bugm2(1 TO 15) AS INTEGER

DIM bugf3(1 TO 15) AS INTEGER
DIM bugm3(1 TO 15) AS INTEGER

DIM bugf4(1 TO 15) AS INTEGER
DIM bugm4(1 TO 15) AS INTEGER

DIM bugf5(1 TO 15) AS INTEGER
DIM bugm5(1 TO 15) AS INTEGER

DIM bug(1 TO NumBugs) AS BugData
DIM curBug AS INTEGER

GOSUB LoadBugSprites
SCREEN , , BPage, VPage
GOSUB DrawBackground
'LINE (0, 0)-(319, 199), 7, BF
FOR curBug = 1 TO NumBugs
   bug(curBug).x = INT(RND * 280) + 10
   bug(curBug).y = INT(RND * 160) + 10
   bug(curBug).wing = INT(RND * 2)
   bug(curBug).curFrame = INT(RND * 5) + 1
NEXT
SCREEN , , APage, VPage
WHILE INKEY$ = ""
   PCOPY APage, VPage
'   FOR curBug = 1 TO NumBugs
'      GOSUB EraseBug
'   NEXT
PCOPY BPage, APage
FOR curBug = 1 TO NumBugs
GOSUB DrawBug
NEXT

FOR curBug = 1 TO NumBugs
IF bug(curBug).wing = 0 THEN
   bug(curBug).curFrame = bug(curBug).curFrame + 1
   IF bug(curBug).curFrame = 6 THEN
      bug(curBug).curFrame = 5: bug(curBug).wing = 1
   END IF
ELSE
   bug(curBug).curFrame = bug(curBug).curFrame - 1
   IF bug(curBug).curFrame = 0 THEN
      bug(curBug).curFrame = 1: bug(curBug).wing = 0
   END IF
END IF
NEXT
FOR curBug = 1 TO NumBugs
GOSUB MoveBug
NEXT
WEND
END

MoveBug:
   IF bug(curBug).dist = 0 THEN
      bug(curBug).dist = INT(RND * 20)
      bug(curBug).dir = INT(RND * 4)
      bug(curBug).speed = INT(RND * 5) + 1
      'in 1 out of 5 they go 2 pixels
      IF bug(curBug).speed < 5 THEN bug(curBug).speed = 1 ELSE bug(curBug).speed = 2
   ELSE
      bug(curBug).dist = bug(curBug).dist - 1
   END IF
   SELECT CASE bug(curBug).dir
      CASE 0
         bug(curBug).y = bug(curBug).y - bug(curBug).speed
      CASE 1
         bug(curBug).y = bug(curBug).y + bug(curBug).speed
      CASE 2
         bug(curBug).x = bug(curBug).x - bug(curBug).speed
      CASE 3
         bug(curBug).x = bug(curBug).x + bug(curBug).speed
   END SELECT
IF bug(curBug).x < 0 THEN bug(curBug).x = 0
IF bug(curBug).x > 315 THEN bug(curBug).x = 315
IF bug(curBug).y < 0 THEN bug(curBug).y = 0
IF bug(curBug).y > 195 THEN bug(curBug).y = 195

RETURN

DrawBug:
   SELECT CASE bug(curBug).curFrame
      CASE 1
         PUT (bug(curBug).x, bug(curBug).y), bugm1, AND
         PUT (bug(curBug).x, bug(curBug).y), bugf1, XOR
      CASE 2
         PUT (bug(curBug).x, bug(curBug).y), bugm2, AND
         PUT (bug(curBug).x, bug(curBug).y), bugf2, XOR
      CASE 3
         PUT (bug(curBug).x, bug(curBug).y), bugm3, AND
         PUT (bug(curBug).x, bug(curBug).y), bugf3, XOR
   CASE 4
         PUT (bug(curBug).x, bug(curBug).y), bugm4, AND
         PUT (bug(curBug).x, bug(curBug).y), bugf4, XOR
   CASE 5
         PUT (bug(curBug).x, bug(curBug).y), bugm5, AND
         PUT (bug(curBug).x, bug(curBug).y), bugf5, XOR
   END SELECT
RETURN

EraseBug:
   LINE (bug(curBug).x, bug(curBug).y)-(bug(curBug).x + 4, bug(curBug).y + 4), 0, BF
RETURN

LoadBugSprites:
FOR m = 1 TO 5
   FOR n = 1 TO 25
      READ clr
      'IF clr = 1 THEN clr = 4
      PSET (vidX, vidY), clr
      vidX = vidX + 1
      IF vidX = 5 THEN
         vidX = 0: vidY = vidY + 1
      END IF
   NEXT
  vidX = 5: vidY = 0
  FOR n = 1 TO 25
      READ clr
      PSET (vidX, vidY), clr
      vidX = vidX + 1
      IF vidX = 10 THEN
         vidX = 5: vidY = vidY + 1
      END IF
   NEXT
'   SLEEP

   SELECT CASE m
      CASE 1
         GET (0, 0)-(4, 4), bugf1
         GET (5, 0)-(9, 4), bugm1
     
      CASE 2
         GET (0, 0)-(4, 4), bugf2
         GET (5, 0)-(9, 4), bugm2
     
      CASE 3
         GET (0, 0)-(4, 4), bugf3
         GET (5, 0)-(9, 4), bugm3
     
      CASE 4
         GET (0, 0)-(4, 4), bugf4
         GET (5, 0)-(9, 4), bugm4
     
      CASE 5
         GET (0, 0)-(4, 4), bugf5
         GET (5, 0)-(9, 4), bugm5
  
   END SELECT
   vidX = 0: vidY = 0
NEXT
CLS
RETURN

WaitRetrace:
FOR n = 1 TO Delay
WAIT &H3DA, 8
NEXT
RETURN
DrawBackground:
FOR n = 0 TO 39
FOR m = 0 TO 63
clr = INT(RND * 450)
SELECT CASE clr
   CASE 0
      clr = 7
   CASE 1
      clr = 15
   CASE 2
      clr = 8
   CASE 3
      clr = 7
   CASE IS > 3
      clr = 0
END SELECT
PSET (m, n), clr
NEXT
NEXT
GET (0, 0)-(63, 39), tile

'SLEEP
CLS
FOR n = 0 TO 24
PUT (vx, vy), tile
vx = vx + 64
IF vx > 289 THEN
   vx = 0
   vy = vy + 40
END IF
NEXT
RETURN

DATA 1,0,0,0,1
DATA 0,1,0,1,0
DATA 0,0,1,0,0
DATA 0,0,0,0,0
DATA 0,0,0,0,0

DATA 0,15,15,15,0
DATA 15,0,15,0,15
DATA 15,15,0,15,15
DATA 15,15,15,15,15
DATA 15,15,15,15,15

DATA 0,0,0,0,0
DATA 1,0,0,0,1
DATA 0,1,1,1,0
DATA 0,0,0,0,0
DATA 0,0,0,0,0

DATA &HF,&HF,&HF,&HF,&HF
DATA &H0,&HF,&HF,&HF,&H0
DATA &HF,&H0,&H0,&H0,&HF
DATA &HF,&HF,&HF,&HF,&HF
DATA &HF,&HF,&HF,&HF,&HF

DATA 0,0,0,0,0
DATA 0,0,0,0,0
DATA 1,1,1,1,1
DATA 0,0,0,0,0
DATA 0,0,0,0,0

DATA &HF,&HF,&HF,&HF,&HF
DATA &HF,&HF,&HF,&HF,&HF
DATA &H0,&H0,&H0,&H0,&H0
DATA &HF,&HF,&HF,&HF,&HF
DATA &HF,&HF,&HF,&HF,&HF

DATA 0,0,0,0,0
DATA 0,0,0,0,0
DATA 0,1,1,1,0
DATA 1,0,0,0,1
DATA 0,0,0,0,0

DATA &HF,&HF,&HF,&HF,&HF
DATA &HF,&HF,&HF,&HF,&HF
DATA &HF,&H0,&H0,&H0,&HF
DATA &H0,&HF,&HF,&HF,&H0
DATA &HF,&HF,&HF,&HF,&HF

DATA 0,0,0,0,0
DATA 0,0,0,0,0
DATA 0,0,1,0,0
DATA 0,1,0,1,0
DATA 1,0,0,0,1

DATA &HF,&HF,&HF,&HF,&HF
DATA &HF,&HF,&HF,&HF,&HF
DATA &HF,&HF,&H0,&HF,&HF
DATA &HF,&H0,&HF,&H0,&HF
DATA &H0,&HF,&HF,&HF,&H0


