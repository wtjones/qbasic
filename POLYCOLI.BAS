DEFINT A-Z

SCREEN 7

TYPE point2d
   x AS INTEGER
   y AS INTEGER
END TYPE

TYPE line2d
   p1 AS point2d
   p2 AS point2d
   facing AS SINGLE
END TYPE

DIM player AS point2d

DIM wall(1 TO 3) AS line2d


wall(1).p1.x = 100
wall(1).p1.y = 100
wall(1).p2.x = 130
wall(1).p2.y = 80

wall(2).p1.x = 130
wall(2).p1.y = 80
wall(2).p2.x = 160
wall(2).p2.y = 110

wall(3).p1.x = 160
wall(3).p1.y = 110
wall(3).p2.x = 100
wall(3).p2.y = 100



player.x = 180
player.y = 150




WHILE k$ <> CHR$(27)

   GOSUB drawit

FOR i = 1 TO 3
   wall(i).facing = (player.y - wall(i).p1.y) * (wall(i).p2.x - wall(i).p1.x) - (player.x - wall(i).p1.x) * (wall(i).p2.y - wall(i).p1.y)
   LOCATE 1, 1: PRINT wall(1).facing
   LOCATE 2, 1: PRINT wall(2).facing
   LOCATE 3, 1: PRINT wall(3).facing
NEXT i

   k$ = INKEY$
   SELECT CASE k$
      CASE "8"
         player.y = player.y - 1
      CASE "2"
         player.y = player.y + 1

      CASE "4"
         player.x = player.x - 1
      CASE "6"
         player.x = player.x + 1
   END SELECT


WEND
END


drawit:
'draw code

FOR i = 1 TO 3
   LINE (wall(i).p1.x, wall(i).p1.y)-(wall(i).p2.x, wall(i).p2.y), 4
NEXT i

PSET (player.x, player.y), 5

RETURN

