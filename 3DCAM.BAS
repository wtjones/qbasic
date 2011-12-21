OPTION BASE 0     'Make arrays start numbering at 0.

CONST SCREENWIDTH = 320, SCREENHEIGHT = 200
CONST MAXPOLYLENGTH = 4
CONST PROJECTIONRATIO = -2!
CONST PI! = 3.1459

TYPE Point3d
   x AS DOUBLE
   y AS DOUBLE
   z AS DOUBLE
   w AS DOUBLE
END TYPE

TYPE Point2d
   x AS DOUBLE'INTEGER
   y AS DOUBLE'INTEGER
END TYPE


DECLARE SUB ConcatXforms (sourceXform1() AS DOUBLE, sourceXform2() AS DOUBLE, destXform() AS DOUBLE)
DECLARE SUB XformVec (xform() AS DOUBLE, sourceVec() AS DOUBLE, destVec() AS DOUBLE)
DECLARE SUB DrawPoly (polyVerts() AS Point2d, polyLength AS INTEGER, clr AS INTEGER, x AS INTEGER, y AS INTEGER)
DECLARE SUB XformAndProjectPoly (xform() AS DOUBLE, poly() AS Point3d, polyLength AS INTEGER, clr AS INTEGER)

DIM i AS INTEGER, j AS INTEGER, done AS INTEGER
DIM SHARED polyWorldXform(0 TO 3, 0 TO 3) AS DOUBLE
DIM SHARED worldViewXform(0 TO 3, 0 TO 3) AS DOUBLE
DIM SHARED workingXform(4, 4) AS DOUBLE
DIM testPoly(3) AS Point3d
DIM SHARED vert4x1Temp(4) AS DOUBLE 'vertex 4 x 1 matrix temp
DIM SHARED vert4x1Temp2d(4) AS DOUBLE
DIM SHARED SinLook(360) AS DOUBLE
DIM SHARED CosLook(360) AS DOUBLE
'**** MAIN PROGRAM **************

FOR i = 0 TO 359
   SinLook(i) = SIN(i * PI / 180)
   CosLook(i) = COS(i * PI / 180)
NEXT i

SCREEN 7


RESTORE polyworldxformdata
FOR i = 0 TO 3
   FOR j = 0 TO 3
      READ polyWorldXform(i, j)
   NEXT
NEXT

RESTORE worldviewxformdata
FOR i = 0 TO 3
   FOR j = 0 TO 3
      READ worldViewXform(i, j)
   NEXT
NEXT

RESTORE testpolydata
FOR i = 0 TO 2
   READ testPoly(i).x, testPoly(i).y, testPoly(i).z, testPoly(i).w
NEXT i

rotation! = PI / 60
xrot = PI / 60
xPos = 1: yPos = 1
'********** MAIN LOOP *********************

done = 0
WHILE done = 0
   '--Modify the object space to world space transformation matrix for the
   '--current rotation around the Y axis.
  
   temp = CosLook(rotation)
   polyWorldXform(0, 0) = temp: polyWorldXform(2, 2) = temp
   temp = SinLook(rotation)
   polyWorldXform(2, 0) = temp: polyWorldXform(0, 2) = -temp

   '--Now do some X axis.
 '  temp = COS(xrot)
 '  polyWorldXform(1, 1) = temp: polyWorldXform(2, 2) = temp
 '  temp = SIN(xrot)
'   polyWorldXform(1, 2) = temp: polyWorldXform(2, 1) = -temp

   temp = CosLook(yRot)
   worldViewXform(0, 0) = temp: worldViewXform(2, 2) = temp
   temp = SinLook(yRot)
   worldViewXform(2, 0) = temp: worldViewXform(0, 2) = -temp
  
   temp = CosLook(xrot)
   worldViewXform(1, 1) = temp: worldViewXform(2, 2) = temp
   temp = SinLook(xrot)
   worldViewXform(1, 2) = temp: worldViewXform(2, 1) = -temp
  
   'load the movement
   worldViewXform(0, 3) = xPos
   worldViewXform(2, 3) = zPos

   '--Concatenate the object-to-world and world-to-view transformations
   '--to make a transformation matrix that will convert vertices from object
   '--space to view space in a single operation.
   '----  (workingXform is the result)
   CALL ConcatXforms(worldViewXform(), polyWorldXform(), workingXform())

   CALL XformAndProjectPoly(workingXform(), testPoly(), 3, 4)
   PSET (xPos + 20, zPos + 100)  'sort of draws the camera pos
   PSET (20, 140), 4     'overhead view of the poly
   WAIT &H3DA, 8: WAIT &H3DA, 8, 8

   CALL XformAndProjectPoly(workingXform(), testPoly(), 3, 0)

'rotation = rotation + 1
'IF rotation >= 359 THEN rotation = 0

k$ = INKEY$
IF k$ = "4" THEN yRot = yRot + 1       '    (PI / 120)
IF k$ = "6" THEN yRot = yRot - 1       '(PI / 120)
IF yRot > 359 THEN yRot = 0
IF yRot < 0 THEN yRot = 359

IF k$ = "2" THEN
   xPos = xPos - SinLook(yRot) * 5
   zPos = zPos - CosLook(yRot) * 5
END IF

IF k$ = "8" THEN
   xPos = xPos + SinLook(yRot) * 5
   zPos = zPos + CosLook(yRot) * 5
END IF
LOCATE 15, 1: PRINT xPos, zPos

IF k$ = CHR$(27) THEN END
'xrot = xrot + PI / 120
'IF xrot >= PI * 2 THEN xrot = -(PI * 2)

WEND

END


polyworldxformdata:
DATA 1, 0, 0, 0
DATA 0, 1, 0, 0
DATA 0, 0, 1, -140
DATA 0, 0, 0, 1

worldviewxformdata:
DATA 1,0,0,0
DATA 0,1,0,0
DATA 0,0,1,10
DATA 0,0,0,1

testpolydata:
DATA -20,   -15,  0,    1
DATA 0,      15,  0,    1
DATA 20,    -15,   0,    1

SUB ConcatXforms (sourceXform1() AS DOUBLE, sourceXform2() AS DOUBLE, destXform() AS DOUBLE)

DIM i AS INTEGER, j AS INTEGER, k AS INTEGER

FOR i = 0 TO 3
   FOR j = 0 TO 3
      destXform(i, j) = 0
      FOR k = 0 TO 3
         destXform(i, j) = destXform(i, j) + (sourceXform1(i, k) * sourceXform2(k, j))
      NEXT k
   NEXT j
NEXT i







END SUB

SUB DrawPoly (polyVerts() AS Point2d, polyLength AS INTEGER, clr AS INTEGER, x AS INTEGER, y AS INTEGER)

DIM i AS INTEGER

FOR i = 0 TO polyLength - 2
'   PSET (polyVerts(i).x, polyVerts(i).y), clr
   LINE (polyVerts(i).x, polyVerts(i).y)-(polyVerts(i + 1).x, polyVerts(i + 1).y), clr
NEXT i

'connect the last and the first verts
LINE (polyVerts(i).x, polyVerts(i).y)-(polyVerts(0).x, polyVerts(0).y), clr

END SUB

SUB XformAndProjectPoly (xform() AS DOUBLE, poly() AS Point3d, polyLength AS INTEGER, clr AS INTEGER)

DIM i AS INTEGER
DIM xformedPoly(0 TO MAXPOLYLENGTH - 1) AS Point3d

DIM projectedPoly(0 TO MAXPOLYLENGTH - 1) AS Point2d

'---Transform to viewspace, then project to screen.---
FOR i = 0 TO (polyLength - 1)
   '--Transform to viewspace
      '--First turn the vertex into 4x1 matrix...
   vert4x1Temp(0) = poly(i).x
   vert4x1Temp(1) = poly(i).y
   vert4x1Temp(2) = poly(i).z
   vert4x1Temp(3) = poly(i).w
  
   CALL XformVec(xform(), vert4x1Temp(), vert4x1Temp2d())


   'the vert4x1Temp2d matrix is our x y and z, so move it to xformedPoly()


   xformedPoly(i).x = vert4x1Temp2d(0)
   xformedPoly(i).y = vert4x1Temp2d(1)
   xformedPoly(i).z = vert4x1Temp2d(2)
   xformedPoly(i).w = vert4x1Temp2d(3)
  
'projectedPoly(i).x = xformedPoly(i).x: projectedPoly(i).y = xformedPoly(i).y
   projectedPoly(i).x = INT((xformedPoly(i).x / xformedPoly(i).z * PROJECTIONRATIO * (SCREENWIDTH / 2) + .5)) + SCREENWIDTH / 2
   projectedPoly(i).y = INT((xformedPoly(i).y / xformedPoly(i).z * -1! * PROJECTIONRATIO * (SCREENWIDTH / 2) + .5)) + SCREENHEIGHT / 2

NEXT i

CALL DrawPoly(projectedPoly(), polyLength, clr, 0, 0)

END SUB

SUB XformVec (xform() AS DOUBLE, sourceVec() AS DOUBLE, destVec() AS DOUBLE)

'DIM destVec(0 TO 3)  AS DOUBLE
DIM i AS INTEGER, j AS INTEGER

FOR i = 0 TO 3
   destVec(i) = 0
   FOR j = 0 TO 3
      destVec(i) = destVec(i) + (xform(i, j) * sourceVec(j))
   NEXT j
NEXT i

'FOR i = 0 TO 3: PRINT "newvec"; destVec(i): NEXT i
'SLEEP: CLS


END SUB

