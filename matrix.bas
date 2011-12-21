'Matrix Screensaver Clone
'12/9/00
'William Travis Jones

'Was bored one night, made whole initial program in 2.5 hours or so
'of messing with it.
'The delay code(to make some faster than others) was thrown in at last
'minute and makes the program almost unreadable.
'Haven't figured out how to get an extra shade of darker green without
'screwing with the palette.

DEFINT A-Z: RANDOMIZE TIMER
SCREENCOL = 80: SCREENROW = 50
MAXCOL = SCREENCOL
MAXROW = SCREENROW - 2
NUMDROPS = 50
CLEARDROPS = 60
TYPE dropType
   char AS STRING * 1
   curCol AS INTEGER
   curRow AS INTEGER
   delayMax AS INTEGER
   delayCounter AS INTEGER
END TYPE

DIM drops(1 TO NUMDROPS)  AS dropType

'screen setup
CLS
SCREEN 11
WIDTH SCREENCOL, SCREENROW

'set drops to initial pos
FOR i = 1 TO NUMDROPS
   drops(i).char = CHR$(INT(RND * 255))
   drops(i).curCol = INT(RND * MAXCOL + 1)
   drops(i).curRow = INT(RND * MAXROW + 1)
   drops(i).delayMax = INT(RND * 3)
   drops(i).delayCounter = delayMax
NEXT i

'--------****** MAIN LOOP *********-----
WHILE INKEY$ = ""
GOSUB DrawDrops

GOSUB DeleteDrops 'Randomly erase spots on the screen as many times
                  'as specified by CLEARDROPS.

'Wait for the vertical retrace several times to slow down.
FOR d = 1 TO 1: WAIT &H3DA, 8: NEXT d
WEND
END
'-------******END MAIN LOOP****--------

DrawDrops: 'drop down by 1 and draw
   'Basically the drop only goes if its delayCounter made it to 0.
   FOR j = 1 TO NUMDROPS
      IF drops(j).delayCounter = 0 THEN
      'first take last char value and drop it in current pos as darker green
         LOCATE drops(j).curRow, drops(j).curCol
         COLOR 2, 0 'green
         PRINT drops(j).char
      END IF
   NEXT j

   FOR j = 1 TO NUMDROPS
      IF drops(j).curRow = MAXROW THEN
         dropToReset = j: GOSUB ResetDrop
      ELSE
         IF drops(j).delayCounter = 0 THEN drops(j).curRow = drops(j).curRow + 1
      END IF
     
      'Pick new character....
      'I noticed that setting it to random from 0 to 255 is bad, as some
      'characters make the screen clear for some reason. Makes no sense
      'to me.
      drops(j).char = CHR$(INT(RND * 100) + 150)
      LOCATE drops(j).curRow, drops(j).curCol
      COLOR 10, 0 'light green
      PRINT drops(j).char
   NEXT j

   'Now reset any counters that need it, and decrement the rest.
   FOR j = 1 TO NUMDROPS
      IF drops(j).delayCounter = 0 THEN
         drops(j).delayCounter = drops(j).delayMax
      ELSE drops(j).delayCounter = drops(j).delayCounter - 1
      END IF
   NEXT j
RETURN

ResetDrop:
   'i should use SUBS for this
   drops(dropToReset).curCol = INT(RND * MAXCOL + 1)
   drops(dropToReset).curRow = 1 'INT(RND * MAXROW + 1)
   drops(dropToReset).delayMax = INT(RND * 4)
   drops(dropToReset).delayCounter = delayMax
RETURN

DeleteDrops:
   COLOR 0, 0
   FOR i = 1 TO CLEARDROPS
      delCol = INT(RND * MAXCOL + 1)
      delRow = INT(RND * MAXROW + 1)
      LOCATE delRow, delCol: PRINT " "
   NEXT i
   'Now delete the bottom row to keep the chars from hanging.
   FOR i = 1 TO MAXCOL
      LOCATE MAXROW, i
      PRINT " "
   NEXT i
RETURN

