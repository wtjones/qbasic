DECLARE SUB MemCpy (destSeg AS INTEGER, destPtr AS INTEGER, sourceSeg AS INTEGER, sourcePtr AS INTEGER, numBytes AS LONG)
DIM str AS STRING * 4
DIM num AS SINGLE
DIM real AS LONG
DIM real2 AS LONG

RANDOMIZE TIMER
real = &H6A6F6E65

str = "jone"
'CLS
DEF SEG = VARSEG(str)

'PRINT PEEK(VARPTR(str))
PRINT str

num = 500
PRINT num
'FOR i = 0 TO 3
'   POKE VARPTR(num) + i, PEEK(VARPTR(str) + i)
'NEXT i

CALL MemCpy(VARSEG(num), VARPTR(num), VARSEG(str), VARPTR(str), 4)
'CALL MemCpy(VARSEG(num), VARPTR(num), VARSEG(real), VARPTR(real), 4)



PRINT num
PRINT real
PRINT "---"

DEF SEG = VARSEG(str)
FOR i = 0 TO 3
   PRINT (PEEK(VARPTR(num) + i))
NEXT i

SUB MemCpy (destSeg AS INTEGER, destPtr AS INTEGER, sourceSeg AS INTEGER, sourcePtr AS INTEGER, numBytes AS LONG)

' MemCpy() 9/2001 Travis Jones
' BASIC version of C function (in memory.h) by the same name
' Copies 'numBytes' bytes from location sourceSeg:sourcePtr
' to destSeg:destPtr.

DIM i AS LONG                 'loop index
DIM byte AS INTEGER              'current byte to transfer

FOR i = 0 TO numBytes - 1
   DEF SEG = sourceSeg           'use segment of source
   byte = PEEK(sourcePtr + i)    'copy a byte of source

   DEF SEG = destSeg             'switch to segment of dest
   POKE destPtr + i, byte        'write the byte
NEXT i

END SUB

