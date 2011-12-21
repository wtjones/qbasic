DECLARE SUB drawBoard ()

SCREEN 13
drawBoard
WHILE INKEY$ = ""
WEND

SUB drawBoard

'y = 20
'FOR a = 1 TO 9
'   LINE (20, y)-(180, y), 15
'   y = y + 20
'NEXT
'x = 20

'FOR a = 1 TO 9
 '  LINE (x, 20)-(x, 180)
'   x = x + 20
'NEXT

x = 41: x2 = 59: y = 21: y2 = 39

FOR a = 1 TO 16                        'fill in every other row from top
   LINE (x, y)-(x2, y2), , BF
   x = x + 40: x2 = x2 + 40
   IF x > 200 THEN
   x = 41: x2 = 59: y = y + 40: y2 = y2 + 40
END IF
NEXT
x = 61: x2 = 79: y = 41: y2 = 59

FOR a = 1 TO 16                        'fill in every other row from 1 down
   LINE (x, y)-(x2, y2), , BF
   x = x + 40: x2 = x2 + 40
   IF x > 201 THEN
      x = 61: x2 = 79: y = y + 40: y2 = y2 + 40
   END IF
NEXT




tlx = 40: tly = 20: brx = 200: bry = 180
'LINE (40, 25)-(280, 175), , B           'Draw the border
colr = 17
        FOR c = 1 TO 15
                LINE (tlx, tly)-(brx, bry), colr, B
                tlx = tlx - 1
                tly = tly - 1
                brx = brx + 1
                bry = bry + 1
                colr = colr + 1
        NEXT
        colr = 1


END SUB

