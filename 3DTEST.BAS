'3d world test - 2/9/01 travis jones
'Attempting 2 things at once:
'  1. learn matrix transforms to make coding 3d 'easier'
'  2. understand world to camera-space transform
'
'First ported from abrash's L35-5.c example in 'zen of graphics programming'
'now trying to add a movable camera.
'So far putting the x and z of the into the world-to-view matrix works ok,
'(use WASD keys for that like an FPS)
'but the rotation is all messed up.  (use numpad 4 and 6)
'
'
'inverse explanation from: http://www.longbowdigitalarts.com/seumas/progmatrix.html
'         [0,  0, -1]        T   [ 0, 1,  0]
'matrix = [1,  0,  0]  matrix  = [ 0, 0, -1]
'         [0, -1,  0]            [-1, 0,  0]
'          ^   ^   ^  
'          |   |   |-- Z Axis Vector
'          |   |------ Y Axis Vector
'          |---------- X Axis Vector
'
'Effective cell order change (in RAM):
'
'           [0, 3, 6]            [0, 1, 2]
'    normal [1, 4, 7]  transpose [3, 4, 5]
'           [2, 5, 8]            [6, 7, 8]



OPTION BASE 0     'Make arrays start numbering at 0.

CONST SCREENWIDTH = 320, SCREENHEIGHT = 200
CONST MAXPOLYLENGTH = 4
CONST PROJECTIONRATIO = -2!
CONST PI! = 3.1459
CONST true = 1, false = -1

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

declare sub Inverse4x4(sourceXform as double, destXform() as double)
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
DIM SHARED ccos(0 TO 359) AS SINGLE
DIM SHARED csin(0 TO 359) AS SINGLE
dim shared mxrot(4,4) as double,myrot(4,4) as double,mzrot(4,4) as double
dim shared mTemp1(4,4) as double, mTemp2(4,4) as double
dim shared vright(4) as double, vup(4) as double,vdown(4) AS double

'**** MAIN PROGRAM **************
SCREEN 7, , 0, 1

FOR n = 0 TO 359
  ccos(n) = COS(n * PI / 180)
  csin(n) = SIN(n * PI / 180)
NEXT n

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
 camx = 10: camz = 10
camxrot = 0: camYRot = 0: camzrot = 0
'xrot = PI / 60
'********** MAIN LOOP *********************

done = 0
WHILE done <> true
   '--Modify the object space to world space transformation matrix for the
   '--current rotation around the Y axis.
  
   temp = ccos(yRot)
   polyWorldXform(0, 0) = temp: polyWorldXform(2, 2) = temp
   temp = csin(yRot)
   polyWorldXform(2, 0) = -temp: polyWorldXform(0, 2) = temp

   '--Now do some X axis.
'   temp = cCOS(xrot)
'   polyWorldXform(1, 1) = temp: polyWorldXform(2, 2) = temp
'   temp = cSIN(xrot)
'   polyWorldXform(1, 2) = temp: polyWorldXform(2, 1) = -temp

'restore identity and
'put negative of camera coords into worldview matrix so that the camera
'origin is the origin of the world

RESTORE worldviewxformdata
FOR i = 0 TO 3
   FOR j = 0 TO 3
      READ worldViewXform(i, j)
   NEXT
NEXT

   worldViewXform(0, 3) = (-camx)
   worldViewXform(2, 3) = (-camz)


s=ssin(camYRot)		'yaw
c=ccos(camYRot)
mYRot(0,0)=c
mYRot(0,2)=-s
mYRot(2,0)=s
mYRot(2,2)=c

s=ssin(camXRot)		'pitch
c=ccos(camXRot)
mXRot(1,1)=c
mXRot(1,2)=s
mXRot(2,1)=-s
mXRot(2,2)=c

s=ssin(camZRot)		'roll
c=ccos(camZRot)
mZRot(0,0)=c
mZRot(0,1)=s
mZRot(1,0)=-s
mZRot(1,1)=c

ConcatXForms(mZRot(),mYRot(),mTemp1())
ConcatXForms(mXRot(),mTemp1(),mTemp2())

for i = 0 to 3
vright(i)=mtemp2(0,i)
vup(i)=mtemp2(1,i)
vdown(i)=mtemp2(2,i)
next i

'now that the camera is the center of the world, rotate everything around it
'with the negative of the cameras axices
'--too bad this doesnt work, it just rotates around the world origin
'   temp = ccos(camYRot)
'   worldViewXform(0, 0) = temp: worldViewXform(2, 2) = temp
'   temp = csin(camYRot)
'   worldViewXform(2, 0) = -temp: worldViewXform(0, 2) = temp


LOCATE 21, 1: PRINT camx, camz', worldviewxform(3, 0)
LOCATE 22, 1: PRINT camYRot
  
  
   '--Concatenate the object-to-world and world-to-view transformations
   '--to make a transformation matrix that will convert vertices from object
   '--space to view space in a single operation.
   '----  (workingXform is the result)
   CALL ConcatXforms(worldViewXform(), polyWorldXform(), workingXform())
   CALL XformAndProjectPoly(workingXform(), testPoly(), 3, 4)
   PCOPY 0, 1
  CLS

yRot = yRot + 1: IF yRot > 359 THEN yRot = 0
GOSUB handlekeys



WEND

END

handlekeys:
   k$ = INKEY$
   IF k$ = "w" THEN camz = camz - 3
   IF k$ = "s" THEN camz = camz + 3
   IF k$ = "a" THEN camx = camx - 3
   IF k$ = "d" THEN camx = camx + 3
   IF k$ = "6" THEN camYRot = camYRot + 2
      IF camYRot > 359 THEN camYRot = 0
   IF k$ = "4" THEN camYRot = camYRot - 2
      IF camYRot < 0 THEN camYRot = 359
   IF k$ = CHR$(27) THEN done = true
RETURN

polyworldxformdata:
DATA 1, 0, 0, -20
DATA 0, 1, 0, 0
DATA 0, 0, 1, -150
DATA 0, 0, 0, 1

worldviewxformdata:
DATA 1,0,0,0
DATA 0,1,0,0
DATA 0,0,1,0
DATA 0,0,0,1

testpolydata:
DATA -30,   0,  0,    1
DATA 0,      25,  0,    1
DATA 10,    0,   0,    1

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
   IF xformedPoly(i).z < 1 THEN
   projectedPoly(i).x = INT((xformedPoly(i).x / xformedPoly(i).z * PROJECTIONRATIO * (SCREENWIDTH / 2) + .5)) + SCREENWIDTH / 2
   projectedPoly(i).y = INT((xformedPoly(i).y / xformedPoly(i).z * -1! * PROJECTIONRATIO * (SCREENWIDTH / 2) + .5)) + SCREENHEIGHT / 2
   END IF
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


sub Inverse4x4(sourceXform as double, destXform() as double)
dim temp(0 to 3, 0 to 3) as double
for c = 0 to 3

destXform(0,c)=sourceXform(c,0)
destXform(1,c)=sourceXform(c,1)
destXform(2,c)=sourceXform(c,2)
destXform(3,c)=sourceXform(c,3)
end sub