'------ Star Scroll ---------
'------ A view of flying in space. ---------
'------ Three layers of stars moving at different speeds. --------
'------ September 1996 by William Travis Jones --------

'------ Press a key to quit.
'------ You can change the number of stars in each layer.

DEFINT A-Z                 'use integers
DECLARE SUB GivePosition ()
DECLARE SUB MoveStars ()
DECLARE SUB DrawStars ()
TYPE star
   x AS INTEGER
   y AS INTEGER
END TYPE
CONST tbk = 60            'total number of stars in the back
CONST tmd = 50             'total number of stars in the middle
CONST tft = 20             'total number of stars in the front
CONST true = -1, false = NOT true
CONST clrdgry = 8, clrlgry = 7, clrwht = 15  'color constants
CONST workpage = 1, visiblepage = 0, blankpage = 2
DIM SHARED back(1 TO tbk) AS star
DIM SHARED midl(1 TO tmd) AS star
DIM SHARED frnt(1 TO tft) AS star


done = false
RANDOMIZE TIMER
SCREEN 7', , workpage, visiblepage
GivePosition      'comment this for a weird effect

'******************** {MAIN LOOP} ********************
WHILE done = false
   MoveStars                  'decrements the x value of all stars
  ' PCOPY blankpage, workpage  'blank out the active page
   DrawStars                  'PSET all of the stars
'LOCATE 12, 15: PRINT "Star Scroll"
  ' PCOPY workpage, visiblepage 'copy the active page to the visual page
  ' SCREEN 7, , workpage, visiblepage      'flip it
IF INKEY$ <> "" THEN done = true
WEND
'******************** {END MAIN LOOP} ********************

END

SUB DrawStars
   FOR workstar = 1 TO tbk
         IF back(workstar).x < 321 THEN
            PSET (back(workstar).x + 1, back(workstar).y), 0
           
            PSET (back(workstar).x, back(workstar).y), clrdgry
         END IF
    NEXT
   FOR workstar = 1 TO tmd
         IF midl(workstar).x < 321 THEN
            PSET (midl(workstar).x + 2, midl(workstar).y), 0
           
            PSET (midl(workstar).x, midl(workstar).y), clrlgry
         END IF
   NEXT
   FOR workstar = 1 TO tft
         IF frnt(workstar).x < 321 THEN
            PSET (frnt(workstar).x + 8, frnt(workstar).y), 0
           
            PSET (frnt(workstar).x, frnt(workstar).y), clrwht
         END IF
   NEXT
END SUB

SUB GivePosition

FOR workstar = 1 TO tbk
   back(workstar).x = INT(RND * 320): back(workstar).y = INT(RND * 200)
NEXT
FOR workstar = 1 TO tmd
   midl(workstar).x = INT(RND * 320): midl(workstar).y = INT(RND * 200)
NEXT
FOR workstar = 1 TO tft
   frnt(workstar).x = INT(RND * 320): frnt(workstar).y = INT(RND * 200)
NEXT

END SUB

SUB MoveStars
   FOR workstar = 1 TO tbk
      IF back(workstar).x < 1 THEN
         back(workstar).x = INT(RND * 50) + 320
         back(workstar).y = INT(RND * 200)
      END IF
   NEXT
   FOR workstar = 1 TO tbk
      back(workstar).x = back(workstar).x - 1
   NEXT
   FOR workstar = 1 TO tmd
      IF midl(workstar).x < 1 THEN
         midl(workstar).x = INT(RND * 50) + 320
         midl(workstar).y = INT(RND * 200)
      END IF
   NEXT
   FOR workstar = 1 TO tmd
      midl(workstar).x = midl(workstar).x - 2
   NEXT
   FOR workstar = 1 TO tft
      IF frnt(workstar).x < 1 THEN
         frnt(workstar).x = INT(RND * 50) + 320
         frnt(workstar).y = INT(RND * 200)
      END IF
   NEXT
   FOR workstar = 1 TO tft
      frnt(workstar).x = frnt(workstar).x - 8
   NEXT

END SUB

