CONST PALMASK = &H3C6
CONST PALREAD = &H3C7
CONST PALWRITE = &H3C8
CONST PALDATA = &H3C9

SCREEN 13

TYPE RGBColor
   red AS INTEGER
   green AS INTEGER
   blue AS INTEGER
END TYPE

FOR i = 0 TO 63
   OUT PALMASK, &HFF

   OUT PALWRITE, i
  
   OUT PALDATA, red
   OUT PALDATA, green
   OUT PALDATA, blue
   red = red + 1
   green = green + 1
   blue = blue + 1
NEXT

red = 0: green = 0: blue = 0
FOR i = 64 TO 127
   OUT PALMASK, &HFF

   OUT PALWRITE, i
 
   OUT PALDATA, red
   OUT PALDATA, green
   OUT PALDATA, blue
   red = red + 1
NEXT

red = 0: green = 0: blue = 0
FOR i = 128 TO 191
   OUT PALMASK, &HFF

   OUT PALWRITE, i

   OUT PALDATA, red
   OUT PALDATA, green
   OUT PALDATA, blue
   green = green + 1
NEXT

red = 0: green = 0: blue = 0
FOR i = 192 TO 256
   OUT PALMASK, &HFF

   OUT PALWRITE, i

   OUT PALDATA, red
   OUT PALDATA, green
   OUT PALDATA, blue
   blue = blue + 1
NEXT


FOR clr = 0 TO 63
   LINE (x, 0)-(x + 4, 49), clr, BF
   LINE (x, 50)-(x + 4, 99), 64 + clr, BF
   LINE (x, 100)-(x + 4, 149), 128 + clr, BF
   LINE (x, 150)-(x + 4, 199), 192 + clr, BF
   x = x + 5
NEXT

i = 64
WHILE INKEY$ = ""
   FOR i = 64 TO 127
      OUT PALMASK, &HFF
      OUT PALREAD, 127
      
	  red = INP(PALDATA)
      green = INP(PALDATA)
      blue = INP(PALDATA)
      red = red + 1
   NEXT
WEND


SCREEN 0: WIDTH 80
END

