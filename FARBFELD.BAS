DECLARE FUNCTION RgbToColor% (red AS INTEGER, green AS INTEGER, blue AS INTEGER)
DECLARE FUNCTION UnsignedLong& (num AS INTEGER)
DECLARE SUB FlipEndian (targetSeg AS INTEGER, targetPtr AS INTEGER, numBytes AS LONG)
DECLARE SUB LoadHeader ()
CONST PALMASK = &H3C6
CONST PALREAD = &H3C7
CONST PALWRITE = &H3C8
CONST PALDATA = &H3C9
CONST PALETTESTEP = 51
TYPE FarbfeldHeader
	magic AS STRING * 8
	imageWidth AS LONG
	imageHeight AS LONG
END TYPE
TYPE FarbfeldPixel
	red AS INTEGER
	green AS INTEGER
	blue AS INTEGER
	alpha AS INTEGER
END TYPE

fileName$ = ".\data\images\tetrom.ff"
fileName$ = ".\data\images\polygon.ff"

OPEN fileName$ FOR BINARY AS #1

DIM SHARED header AS FarbfeldHeader
CLS
CALL LoadHeader
PRINT header.magic
PRINT header.imageWidth
PRINT header.imageHeight

'PRINT UnsignedLong(pixel.red)
'PRINT UnsignedLong(pixel.green)
'PRINT pixel.blue
'PRINT pixel.alpha

bb = RgbToColor(27000, 10000, 5000)
'PRINT "result 1"
'PRINT bb


DEFINT A-Z

SCREEN 13
CLS
DIM i AS INTEGER

i = 0
FOR red = 0 TO 255 STEP PALETTESTEP
	FOR green = 0 TO 255 STEP 51
		FOR blue = 0 TO 255 STEP 51
			OUT PALMASK, &HFF

			OUT PALWRITE, i
						 
			OUT PALDATA, INT(red / 4)
			OUT PALDATA, INT(green / 4)
			OUT PALDATA, INT(blue / 4)
			i = i + 1
			
		NEXT
	NEXT
   ' bbb = INT(red / 4)
	'PRINT red, bbb
NEXT

PRINT " "
PRINT i
PRINT "test.."
PRINT 60 - (60 MOD 51)


FOR x = 0 TO 215
   LINE (x, 0)-(x, 199), x, BF
NEXT

'END
DIM pixel AS FarbfeldPixel
DIM clr AS INTEGER
FOR y = 0 TO header.imageHeight - 1
	FOR x = 0 TO header.imageWidth - 1
		GET #1, , pixel
		'PRINT "pixel"

		CALL FlipEndian(VARSEG(pixel.red), VARPTR(pixel.red), 2)
		CALL FlipEndian(VARSEG(pixel.green), VARPTR(pixel.green), 2)
		CALL FlipEndian(VARSEG(pixel.blue), VARPTR(pixel.blue), 2)
		CALL FlipEndian(VARSEG(pixel.alpha), VARPTR(pixel.alpha), 2)
		clr = RgbToColor(pixel.red, pixel.green, pixel.blue)
		PSET (x, y), clr
		IF INKEY$ <> "" THEN END
	NEXT x
NEXT y

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

SUB FlipEndian (targetSeg AS INTEGER, targetPtr AS INTEGER, numBytes AS LONG)

DIM temp AS STRING * 1
REDIM temp(numBytes)
DIM byte AS INTEGER

FOR i = 0 TO numBytes - 1
	DEF SEG = targetSeg
	byte = PEEK(targetPtr + i)
	DEF SEG = VARSEG(temp)
	POKE VARPTR(temp) + i, byte
NEXT i
FOR i = 0 TO numBytes - 1
	targetOffset = numBytes - 1 - i
	DEF SEG = VARSEG(temp)
	byte = PEEK(VARPTR(temp) + i)
	DEF SEG = targetSeg
	POKE targetPtr + targetOffset, byte
NEXT i

END SUB

DEFSNG A-Z
SUB LoadHeader

GET #1, , header

'DIM temp AS STRING * 4
'DIM byte AS STRING * 1
'GET #1, , temp

'DEF SEG = VARSEG(temp)

PRINT "test before ..."
PRINT header.imageHeight
CALL FlipEndian(VARSEG(header.imageHeight), VARPTR(header.imageHeight), 4)
CALL FlipEndian(VARSEG(header.imageWidth), VARPTR(header.imageWidth), 4)

'FOR i = 0 TO 3

'srcPtr = VARPTR(temp)

'byte = PEEK(srcPtr + i)
'POKE VARPTR(header.imageHeight), byte



IF header.magic <> "farbfeld" THEN
	PRINT "Invalid header."
	CLOSE #1
	END
END IF

END SUB

FUNCTION RgbToColor% (red AS INTEGER, green AS INTEGER, blue AS INTEGER)

RgbToColor = 2
'DIM redByte AS INTEGER
DIM redRounded AS INTEGER
DIM greenRounded AS INTEGER
DIM blueRounded AS INTEGER


'redByte = red / 256
' round to nearest PALETTESTEP

DIM unsignedRed AS LONG
unsignedRed = UnsignedLong(red)
DIM unsignedGreen AS LONG
unsignedGreen = UnsignedLong(green)
DIM unsignedBlue AS LONG
unsignedBlue = UnsignedLong(blue)
'PRINT "uns red"
'PRINT unsignedRed
redRounded = ((unsignedRed / 256) - ((unsignedRed / 256) MOD PALETTESTEP)) / PALETTESTEP
greenRounded = ((unsignedGreen / 256) - ((unsignedGreen / 256) MOD PALETTESTEP)) / PALETTESTEP
blueRounded = ((unsignedBlue / 256) - ((unsignedBlue / 256) MOD PALETTESTEP)) / PALETTESTEP
'redRounded = ((red / 256) - ((red / 256) MOD PALETTESTEP)) / PALETTESTEP
'greenRounded = ((green / 256) - ((green / 256) MOD PALETTESTEP)) / PALETTESTEP
'blueRounded = ((blue / 256) - ((blue / 256) MOD PALETTESTEP)) / PALETTESTEP

'PRINT "rrrr"
'PRINT redRounded
'PRINT greenRounded
'PRINT blueRounded

RgbToColor = (redRounded * 36) + (greenRounded * 6) + blueRounded
END FUNCTION

FUNCTION UnsignedLong& (num AS INTEGER)
	IF num < 0 THEN
		UnsignedLong = num + 65536
	ELSE
		UnsignedLong = num
	END IF
END FUNCTION

