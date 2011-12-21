TYPE WADHeaderType
   wadType AS STRING * 4
   numLumps AS LONG
   dirPos AS LONG
END TYPE
TYPE lumpEntryType
   lumpPos AS LONG
   lumpSize AS LONG
   lumpName AS STRING * 8
END TYPE
TYPE VERTEX
   x AS INTEGER
   y AS INTEGER
END TYPE
TYPE LINEDEF
   startVert AS INTEGER
   endVert AS INTEGER
   flags AS INTEGER
   types AS INTEGER
   tagNum  AS INTEGER
   rightSIDEDEF AS INTEGER
   leftSIDEDEF AS INTEGER ' -1 if doesn't adjoin 2 SECTORS
END TYPE


DIM header AS WADHeaderType
DIM lumpEntry(20) AS lumpEntryType
DIM VERTEX(1 TO 10) AS VERTEX
DIM LINEDEF(1 TO 10)  AS LINEDEF
DIM numVerts AS LONG
DIM i AS INTEGER
CLS

fileName$ = "DOOM1.WAD"

OPEN fileName$ FOR BINARY AS #1
GET #1, , header

SEEK #1, header.dirPos - 1    'go to start of directory

FOR i = 1 TO 20
   GET #1, , lumpEntry(i)
   PRINT lumpEntry(i).lumpName
NEXT
SLEEP

SEEK #1, lumpEntry(9).lumpPos - 1   'E1M1 vertices

'numVerts = 10'lumpEntry(11).lumpSize / 4
CLS
'FOR i = 1 TO numVerts
'   GET #1, , VERTEX(i)
'NEXT i
FOR i = 1 TO 10
   GET #1, , LINEDEF(i)
NEXT
SCREEN 13
PRINT lumpEntry(9).lumpName

FOR i = 1 TO 10
   PRINT LINEDEF(i).flags
'   PSET (VERTEX(i).x, VERTEX(i).y)
NEXT



CLOSE #1
END

