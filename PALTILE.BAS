OPTION BASE 0
DEFINT A-Z

CONST TILESIZE = 4
CONST PALMASK = &H3C6
CONST PALREAD = &H3C7
CONST PALWRITE = &H3C8
CONST PALDATA = &H3C9


TYPE RGBColor
   red AS INTEGER
   green AS INTEGER
   blue AS INTEGER
END TYPE

DIM tileShift(4) AS RGBColor
delay = 1
FOR i = 0 TO 3
    READ tileShift(i).red
    READ tileShift(i).green
    READ tileShift(i).blue
NEXT i

SCREEN 13



FOR i = 0 TO TILESIZE
   OUT PALMASK, &HFF
   OUT PALWRITE, i
  
   OUT PALDATA, tileShift(i).red
   OUT PALDATA, tileShift(i).green
   OUT PALDATA, tileShift(i).blue
NEXT


DIM x AS INTEGER
x = 100
LINE (0, 0)-(319, 199), 255, BF
clr = 0
FOR x = 0 TO 319 STEP 1
   LINE (x, 0)-(x, 49), clr, BF
   'LINE (x, 50)-(x , 99), 64 + clr, BF
   'LINE (x, 100)-(x + 4, 149), 128 + clr, BF
   'LINE (x, 150)-(x + 4, 199), 192 + clr, BF
   'x = x + 1
   clr = clr + 1: IF clr = 4 THEN clr = 0
NEXT
COLOR 1

DIM shift AS INTEGER

shift = 0
WHILE INKEY$ = ""

    FOR di = 0 TO delay - 1: WAIT &H3DA, 8: NEXT di

    palCount = shift
    FOR i = 0 TO TILESIZE - 1
        OUT PALMASK, &HFF
        OUT PALWRITE, i
 
        OUT PALDATA, tileShift(palCount).red
        OUT PALDATA, tileShift(palCount).green
        OUT PALDATA, tileShift(palCount).blue
        palCount = palCount + 1
        IF palCount = 4 THEN palCount = 0
    NEXT i
    shift = shift + 1
    IF shift = 4 THEN shift = 0

 '   PRINT shift
WEND

DATA 63,63,63
DATA 63, 0, 0
DATA 0, 63, 0
DATA 0, 0, 63

DATA &HF,&HF,&HF,&HF
DATA &H0,&HF,&HF,&HF
DATA &HF,&H0,&H0,&H0
DATA &HF,&HF,&HF,&HF

