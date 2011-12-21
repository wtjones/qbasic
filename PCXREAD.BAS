'                             PCXReader 1.0
'                             -------------
'
'                            Febuary 4, 1998
'                                  by
'                           William Travis Jones
'
'  This is my first attempt at a PCX viewer, or even a viewer of any format.
'I tried to make it as readable as possible for anyone who wants to learn how
'PCX files work, but it is still poorly structured.
'  So far it only works with 320 x 200 @ 8 bit PCX files. It doesn't even use
'any values from the header except to verify.
'
'  Here is a summary of what's going on:
'  The first 128 bytes of a PCX file is the header. I made a TYPE for it to
'make it easier to locate the different bytes. The more important fields
'are bitsPerPixel, xLength and yLength. If you are writing code to load a
'PCX of an unknow size, I suspect you can get most of what you need from
'those. The numbers that are 1 byte are stored as STRING * 1 because QB
'doesn't have a char data type. As you can see, to get the numbers I PEEKed
'because for some reason I didn't think to use ASC().
'  Ok now for the decompression. First a byte is read... If its < 192 then
'that is the color of a pixel and is put in the image buffer. If is >= 192
'then 192 is subtracted from it and x bytes are written (x being the answer)
'with the value of the next byte in the file. Still with me? If the first two
'bytes in the rle data are '202, 4' then 10 bytes are filled with the value
'of 4. If you have a single pixel with a color from 192-255, then '193, value'
'is written (the value with a run length of 1).

DEFINT A-Z
 
CONST TRUE = 1, FALSE = 0
CONST PALMASK = &H3C6   'VGA port constants for writing to the palette
CONST PALREAD = &H3C7
CONST PALWRITE = &H3C8
CONST PALDATA = &H3C9

TYPE PCXHeaderType                  '128 byte header for all PCX files
   manuf AS STRING * 1              'should be 10
   version AS STRING * 1            '0, 2, 3, or 5
   rle AS STRING * 1                'should be TRUE for run length encoding
   bitsPerPixel AS STRING * 1       'should be 8 for mode 13h
   x AS INTEGER                     'x and y of upper-left corner
   y AS INTEGER
   pcxWidth AS INTEGER              'size of image
   pcxHeight AS INTEGER
   xLength AS INTEGER               'number of horizontal pixels
   yLength AS INTEGER               'number of vertical pixels
   egaPalette AS STRING * 48        'to be ignored
   reserved AS STRING * 1
   numColorPlanes AS STRING * 1
   bytesPerLine AS INTEGER          'number of horizontal bytes per line
   paletteMode AS INTEGER           'to be ignored
   padding AS STRING * 58           'to be ignored
END TYPE

TYPE RGBColor        'this isn't in use right now, GET wouldn't work with it
   red AS INTEGER    'for some reason
   green AS INTEGER
   blue AS INTEGER
END TYPE


'---ALLOCATE ARRAYS---
DIM header AS PCXHeaderType         '128 bytes for header of any PCX file
REDIM buffer(0 TO 31999) AS INTEGER 'this program is for  320 x 200 at 8 bit
                                    'PCX files, so 64000 bytes is needed,
                                    'which is 32000 INTEGERs
                                    'It's declared with REDIM to make the
                                    'array dynamic (so you can dispose of it
                                    'during runtime to make more room).
DIM bufferSeg AS INTEGER            'segment of buffer
DIM bufferPtr AS INTEGER            'offset of buffer
DIM red AS INTEGER, green AS INTEGER, blue AS INTEGER
DIM fileSize AS LONG                'size of of file in bytes
DIM count AS LONG, i AS LONG     'loop variables
DIM j AS INTEGER, k AS INTEGER
DIM dataTemp AS STRING * 1          'each byte of the RLE data is read into
                                    'this as a string...
DIM byte AS INTEGER                 '... and is tranlated into an integer by
                                    'putting it into this
DIM numBytes AS INTEGER             'number of bytes in current run
DIM time AS SINGLE

'---SETUP---
bufferSeg = VARSEG(buffer(0)) 'store the segment and offset of buffer to use
bufferPtr = VARPTR(buffer(0)) 'with PEEK and POKE

fileName$ = ".\data\images\clip.pcx"
OPEN fileName$ FOR BINARY AS #1

fileSize = LOF(1)    'fileSize now equals amount of bytes in file
GET #1, , header    'this will load the entire 128 bytes of the PCX header
                     'into 'header'
CLS
   'verify the file is a PCX file
DEF SEG = VARSEG(header)
IF PEEK(VARPTR(header.manuf)) <> 10 THEN
   PRINT "Not a valid PCX file!": WHILE INKEY$ = "": WEND: END
END IF

LOCATE 1, 1: PRINT fileName$
LOCATE 2, 1: PRINT "size:"; fileSize; "bytes"
LOCATE 4, 1: PRINT "Loading and decompressing file"
xPrint = 31    'position to draw period while loading picture


'---DECOMPRESS RLE DATA AND STORE IN 'buffer'---
count = 0
time = TIMER
WHILE count < 64000
   dataTemp$ = INPUT$(1, 1)       'data is read from file as a string
   DEF SEG = VARSEG(dataTemp$)
   byte = PEEK(VARPTR(dataTemp$)) 'extract the actual number from 'dataTemp'
  
   IF byte >= 192 AND byte <= 255 THEN    'if this is a run...
      numBytes = byte - 192      'subtract 192 to get the amount of the run
      dataTemp$ = INPUT$(1, 1)    'the next byte is the color to duplicate
      byte = PEEK(VARPTR(dataTemp$))
     
      WHILE numBytes <> 0        'put the color in 'buffer' 'numBytes' times
         DEF SEG = bufferSeg
         POKE (bufferPtr + count), byte
         count = count + 1       'another pixel is done
         numBytes = numBytes - 1
      WEND
  
   ELSE              'it's not a run, just use the byte as the color
      DEF SEG = bufferSeg
      POKE (bufferPtr + count), byte
      count = count + 1    'another pixel is done
   END IF
     
      'every 1000 loops print a new period (to pass the time)
   IF count MOD 1000 = 0 THEN
      LOCATE 4, xPrint: PRINT ".": xPrint = xPrint + 1
   END IF
   IF INKEY$ = CHR$(27) THEN
      CLS : PRINT "Image load canceled...": GOSUB ShutDown
   END IF
WEND
time = TIMER - time


'---DRAW 'buffer' TO SCREEN---
SCREEN 13   ' 320 x 200 @ 256 colors
  
   'copy the picture data from the buffer to video memory
FOR i = 0 TO 63999
   DEF SEG = bufferSeg
   byte = PEEK(bufferPtr + i)
   DEF SEG = &HA000
   POKE i, byte
NEXT


'---LOAD AND INPUT PCX FILE'S PALETTE TO VGA CARD---
SEEK #1, fileSize - 767 'move to begining of palette data

FOR j = 0 TO 255  'there are 256 entries in the mode 13 CLUT
   DEF SEG = VARSEG(dataTemp$)
   dataTemp$ = INPUT$(1, 1)
   red = PEEK(VARPTR(dataTemp$))
   dataTemp$ = INPUT$(1, 1)
   green = PEEK(VARPTR(dataTemp$))
   dataTemp$ = INPUT$(1, 1)
   blue = PEEK(VARPTR(dataTemp$))
  
   OUT PALMASK, &HFF
   OUT PALWRITE, j
   OUT PALDATA, INT(red / 4)     'only the first 6 bits are used, so shift
   OUT PALDATA, INT(green / 4)   'right twice
   OUT PALDATA, INT(blue / 4)
NEXT

WHILE INKEY$ = "": WEND


'---FADE PALETTE TO BLACK---
allZero = FALSE         'assume not all color entries are zero
WHILE allZero = FALSE
   FOR j = 0 TO 255
      allZero = TRUE    'this will stay TRUE only if all entries are < 2
     
      OUT PALMASK, &HFF
      OUT PALREAD, j
      r = INP(PALDATA)
      g = INP(PALDATA)
      b = INP(PALDATA)

      IF r >= 2 THEN
         r = r - 2: allZero = FALSE
      END IF
      IF g >= 2 THEN
         g = g - 2: allZero = FALSE
      END IF
      IF b >= 2 THEN
         b = b - 2: allZero = FALSE
      END IF
     
      OUT PALMASK, &HFF
      OUT PALWRITE, j
      OUT PALDATA, r
      OUT PALDATA, g
      OUT PALDATA, b
   NEXT
WEND

GOSUB ShutDown

ShutDown:
   CLOSE #1
   SCREEN 0
   WIDTH 80
   PRINT "Time it took to load and decode:", time
   PRINT : PRINT : PRINT
   PRINT "Free memory available:", FRE(-1); "bytes": PRINT
   ERASE buffer
   PRINT "Buffer deallocated."
   PRINT "Free memory available:", FRE(-1); "bytes"
   END   'I think this ends the program
RETURN

