'1997 William Jones...
'A snazzier way of waiting for the retrace than just WAIT &H3DA, 8

CONST VGAInpStat = &H3DA
CONST VGARetraceMask = &H8

DIM retrace AS INTEGER
DIM retStatus AS INTEGER

WHILE retrace = 1
   retStatus = INP(VGAInpStat)
   retStatus = (retStatus AND VGARetraceMask)
   IF retStatus <> 1 THEN retrace = 0
WEND

WHILE retrace = 0
   retStatus = INP(VGAInpStat)
   retStatus = (retStatus AND VGARetraceMask)
   IF retStatus <> 0 THEN retrace = 1
WEND

