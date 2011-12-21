'MD2 Model viewer

'1/27/01 Travis Jones
'  file specs from http://www.ugrad.cs.jhu.edu/~dansch/md2/
'  A 'so far' failed attempt at a basic MD2 model viewer. seems that qb can
'  only grab 64k. Maybe I can find some REALLY tiny models.
'  Well I found one on polycount...
'
'1/28/01:
'  Now dev3, rotate and project code is in, slapped a simple cube in for
'  reference. spend over and hour debugging that when it turns out I
'  forgot to define PI!!!
'  The md2 vertex data is still comming in garbled.
'  ... That was because I had the arguments for MID$ backwards.
'
'Still 1/28/01, but hours later....
'  Now dev4, and it draws a wireframe from the triangle data in the file.
'  Press 'm' to switch back to dots.
'
'Few more hours later...
'  Now dev5, and can load very large models now, but insted of overflowing,
'  it will only use the first frame. That's the best workaround I care to
'  do now. Also started on backface removal, looks like normal test doesn't
'  take care of all the cases in most models.
'
'2/7/01: Added sorry clipping code. If the projected value is a crazy huge
'  number it will overflow, so it checks if any vertice is way off screen
'  and clips any triangles that use that vertice.
'
'----- C versions of the data types:
'typdef struct
'{
'   byte vertex[3];
'   byte lightNormalIndex;
'} triangleVertex_t;
'
'typedef struct
'{
'   float scale[3];
'   float translate[3];
'   char name[16];
'   triangleVertex_t vertices[1];
'} frame_t;
'
DEFINT A-Z
CONST TRUE = 1, FALSE = 0
CONST HSCALE = 256
CONST VSCALE = 256
CONST XOFF = 320
CONST YOFF = 175
CONST ZOFF = -60
CONST PALMASK = &H3C6   'VGA port constants for writing to the palette
CONST PALREAD = &H3C7
CONST PALWRITE = &H3C8
CONST PALDATA = &H3C9
CONST PI = 3.1459
CONST FILELIMIT = 25000     'If filesize if too large we will just use
									'the first frame.
CONST STARTXDEG = 270
CONST STARTYDEG = 270
CONST STARTZDEG = 0

TYPE MD2HeaderType
	magic AS LONG
	version AS LONG
	skinWidth AS LONG
	skinHeight AS LONG
	frameSize AS LONG
	numSkins AS LONG
	numVertices AS LONG
	numTexCoords AS LONG
	numTriangles AS LONG
	numGlCommands AS LONG
	numFrames AS LONG
	offsetSkins AS LONG
	offsetTexCoords AS LONG
	offsetTriangles AS LONG
	offsetFrames AS LONG
	offsetGlCommands AS LONG
	offsetEnd AS LONG
END TYPE

TYPE Point3dType
	x AS SINGLE
	y AS SINGLE
	z AS SINGLE
END TYPE

TYPE Point2dType
	x AS INTEGER
	y AS INTEGER
	clip AS INTEGER
END TYPE

TYPE TriangleType
	vert1 AS INTEGER     'vertex indices to make up a triangle
	vert2 AS INTEGER
	vert3 AS INTEGER
	tex1 AS INTEGER
	tex2 AS INTEGER
	tex3 AS INTEGER
END TYPE

DECLARE SUB LoadFrames ()
DECLARE SUB RotateVerts (frame AS INTEGER)
DECLARE SUB ProjAndDraw (numVerts AS LONG, verts() AS Point3dType, clr AS INTEGER)
DECLARE SUB LoadHeader ()
DECLARE SUB LoadFrames ()
DECLARE SUB HandleKeys ()
DECLARE SUB LoadTriangles ()
DECLARE SUB ProjAndDrawLines (numVerts AS LONG, verts() AS ANY, clr AS INTEGER)
DECLARE SUB rotatecube ()
DECLARE SUB Intro ()

'---- OPEN THE FILE ------
DIM SHARED fileName$
fileName$ = ".\data\models\raptor.md2"
fileName$ = ".\data\models\w_mp5.md2"
fileName$ = ".\data\models\w_bfg.md2"
fileName$ = ".\data\models\walker.md2"

OPEN fileName$ FOR BINARY AS #1


'---- ALLOCATE MEMORY ---------
DIM SHARED header AS MD2HeaderType
CALL LoadHeader
DIM SHARED cube(8) AS Point3dType
DIM SHARED nameTable(header.numFrames) AS STRING * 16
DIM SHARED realTrans(header.numVertices) AS Point3dType
DIM SHARED triangles(header.numTriangles) AS TriangleType
DIM SHARED xDeg AS INTEGER, yDeg AS INTEGER, zDeg AS INTEGER    'axis degrees
DIM SHARED xRate AS INTEGER, yRate AS INTEGER, zRate AS INTEGER 'axis velocities
DIM SHARED xLoc AS INTEGER, yLoc AS INTEGER, zLoc AS INTEGER
DIM SHARED ccos(0 TO 359) AS SINGLE
DIM SHARED csin(0 TO 359) AS SINGLE
DIM SHARED done AS INTEGER, mode AS INTEGER
DIM SHARED printOut AS INTEGER
DIM SHARED move AS INTEGER
DIM curFrame AS INTEGER
DIM SHARED backFace AS INTEGER
DIM SHARED fileSize AS LONG
'decide if we only want the first frame
IF header.offsetEnd > FILELIMIT THEN header.numFrames = 12
DIM SHARED realVertTable(header.numFrames, header.numVertices) AS Point3dType
  
'-----MAIN PROGRAM---------
fileSize = LOF(1)    'fileSize now equals amount of bytes in file

FOR n = 0 TO 359
  ccos(n) = COS(n * PI / 180)
  csin(n) = SIN(n * PI / 180)
NEXT n

RESTORE CubeData
FOR n = 1 TO 8
	READ cube(n).x
	READ cube(n).y
	READ cube(n).z
NEXT
CALL LoadFrames
CALL LoadTriangles
CALL Intro
SLEEP 2

SCREEN 9, , 1, 0
printOut = FALSE
backFace = TRUE
move = TRUE
xDeg = STARTXDEG: yDeg = STARTYDEG: zDeg = STARTZDEG
curFrame = 1: lastFrame = 1: zLoc = 5: mode = 1

'--------------------MAIN LOOP-------------------------------
WHILE done <> TRUE


'CALL rotatecube
'CALL ProjAndDraw(8, realTrans(), 4) 'draw the cube

CALL RotateVerts(curFrame)

IF mode = 1 THEN CALL ProjAndDrawLines(header.numVertices, realTrans(), 6)
IF mode = 0 THEN CALL ProjAndDraw(header.numVertices, realTrans(), 4)

'-Print output if wanted-
IF printOut = TRUE THEN
	LOCATE 22, 1: PRINT nameTable(curFrame), "FRAME"
	LOCATE 23, 1: PRINT xDeg, yDeg, zDeg
END IF

PCOPY 1, 0
CLS

CALL HandleKeys

IF move = TRUE THEN
  ' xDeg = xDeg + 10: IF xDeg > 359 THEN xDeg = 0
	yDeg = yDeg + 8: IF yDeg > 359 THEN yDeg = 0
  ' zDeg = zDeg + 1: IF zDeg > 359 THEN zDeg = 0
END IF

lastFrame = curFrame
IF header.numFrames > curFrame THEN curFrame = curFrame + 1
IF curFrame = header.numFrames THEN curFrame = 1
WEND
'-----------------END MAIN LOOP-----------------

CLOSE #1

END

CubeData:
DATA 10,10,-10
DATA -10,10,-10
DATA 10,-10,-10
DATA -10,-10,-10

DATA 10,10,10
DATA -10,10,10
DATA 10,-10,10
DATA -10,-10,10

DEFSNG A-Z
SUB HandleKeys

thekey$ = INKEY$

SELECT CASE thekey$
	CASE "+"
		zLoc = zLoc + 5
	CASE "="
		zLoc = zLoc + 5
	CASE "-"
		zLoc = zLoc - 5
	CASE CHR$(27)
		done = TRUE
	CASE "m"
		IF mode = 0 THEN mode = 1 ELSE IF mode = 1 THEN mode = 0
	CASE "b"
		IF backFace = FALSE THEN backFace = TRUE ELSE IF backFace = TRUE THEN backFace = FALSE
  
	CASE "p"
		IF printOut = FALSE THEN printOut = TRUE ELSE IF printOut = TRUE THEN printOut = FALSE

	CASE "4"
		xDeg = xDeg + 5: IF xDeg > 359 THEN xDeg = 0
  
	CASE "6"
		xDeg = xDeg - 5: IF xDeg < 0 THEN xDeg = 359
  
	CASE "8"
		yDeg = yDeg + 5: IF yDeg > 359 THEN yDeg = 0
 
	CASE "2"
		yDeg = yDeg - 5: IF yDeg < 0 THEN yDeg = 359
  
	CASE "7"
		zDeg = zDeg + 5: IF zDeg > 359 THEN zDeg = 0
 
	CASE "9"
		zDeg = zDeg - 5: IF zDeg < 0 THEN zDeg = 359

	CASE "5"
		xDeg = STARTXDEG: yDeg = STARTYDEG: zDeg = STARTZDEG: move = FALSE

END SELECT

END SUB

SUB Intro

CLS
PRINT fileName$, "is", header.offsetEnd, "bytes"
PRINT header.numVertices, "numverts"
PRINT header.numTriangles, "numtriangles"
PRINT header.offsetFrames, "offsetframes"
PRINT header.numFrames, "numframes"
PRINT header.frameSize, "framesize"
IF header.offsetEnd > FILELIMIT THEN
	LOCATE 15, 10: COLOR 18, 0
	PRINT "FILE TOO LARGE, USING FIRST FRAME ONLY!!!!"
END IF
END SUB

DEFINT A-Z
SUB LoadFrames
DIM scaleTable(header.numFrames, 3) AS SINGLE
DIM translateTable(header.numFrames, 3) AS SINGLE
DIM vertTable(header.numFrames, header.numVertices) AS STRING * 4

DIM scaleTemp AS SINGLE
DIM i AS INTEGER, j AS INTEGER

'move to frame data in file
SEEK #1, header.offsetFrames + 1

'the frames are stored as 4 tables because this is basic and not C.
FOR i = 1 TO header.numFrames    'i is the current frame
	FOR j = 1 TO 3    'x y and z parts
		GET #1, , scaleTable(i, j)
	NEXT j
  
	FOR j = 1 TO 3
		GET #1, , translateTable(i, j)
  NEXT j

	GET #1, , nameTable(i)  ' read in the 16 char name string
	PRINT nameTable(i)
  
	FOR j = 1 TO header.numVertices
		GET #1, , vertTable(i, j)
	NEXT j

NEXT i      'next frame

'now turn scaled coords into 'real' coords
FOR i = 1 TO header.numFrames
	FOR j = 1 TO header.numVertices
		  
			'x
			'PRINT ASC(MID$(vertTable(i, j), 1, 1)): SLEEP
			scaleTemp = ASC(MID$(vertTable(i, j), 1, 1)) * scaleTable(i, 1)
			realVertTable(i, j).x = scaleTemp + translateTable(i, 1)
		  
			'y
			scaleTemp = ASC(MID$(vertTable(i, j), 2, 1)) * scaleTable(i, 2)
			realVertTable(i, j).y = scaleTemp + translateTable(i, 2)
		  
			'z
			scaleTemp = ASC(MID$(vertTable(i, j), 3, 1)) * scaleTable(i, 3)
			realVertTable(i, j).z = scaleTemp + translateTable(i, 3)

  
	NEXT j
NEXT i

REDIM scaleTable(0, 0)
REDIM translateTable(0, 0)
'REDIM nameTable(0)
REDIM vertTable(0, 0)

END SUB

DEFSNG A-Z
SUB LoadHeader

GET #1, , header    'this will load the entire MD2 header into 'header'

'Make sure it's a valid MD2 file.
IF header.magic <> &H32504449 THEN
	PRINT "Invalid MD2 File!!!!": CLOSE #1: END
END IF


END SUB

SUB LoadTriangles

DIM i AS INTEGER
DIM triTemp AS TriangleType

'move to triangle data
SEEK #1, header.offsetTriangles + 1

FOR i = 1 TO header.numTriangles
	GET #1, , triTemp
	'we have to add one to each index because this is not a C array.
	triangles(i).vert1 = triTemp.vert1 + 1
	triangles(i).vert2 = triTemp.vert2 + 1
	triangles(i).vert3 = triTemp.vert3 + 1
NEXT i

END SUB

SUB ProjAndDraw (numVerts AS LONG, verts() AS Point3dType, clr AS INTEGER)
DIM i AS INTEGER, vidx AS INTEGER, vidy AS INTEGER, project AS SINGLE

FOR i = 1 TO numVerts
	project = verts(i).z + ZOFF
	vidx = ((HSCALE * verts(i).x) / project) + XOFF
	vidy = ((VSCALE * verts(i).y) / project) + YOFF
	PSET (vidx, vidy), clr
	'PSET (realVertTable(f, i).x + 160, realVertTable(f, i).z + 100), 8
NEXT i

END SUB

SUB ProjAndDrawLines (numVerts AS LONG, verts() AS Point3dType, clr AS INTEGER)

DIM i AS INTEGER, project AS SINGLE
DIM vidPoints(header.numVertices) AS Point2dType
DIM tempX AS LONG
DIM tempY AS LONG
DIM zNorm1 AS SINGLE, zNorm AS SINGLE, zNorm2 AS SINGLE
DIM what AS DOUBLE
FOR i = 1 TO numVerts
	vidPoints(i).clip = FALSE
NEXT i

'first project all the vertices to screenspace
FOR i = 1 TO numVerts
	project = verts(i).z + ZOFF
  IF project < 0 THEN
  
		' IF project = 0 THEN project = .01
		tempX = INT(((HSCALE * verts(i).x) / project)) + XOFF
		tempY = INT(((VSCALE * verts(i).y) / project)) + YOFF

	IF tempX > 800 OR tempX < -500 OR tempY > 800 OR tempY < -500 THEN
		'crude clipping
		vidPoints(i).clip = TRUE
		ELSE
			vidPoints(i).x = tempX
			vidPoints(i).y = tempY
	END IF

'LOCATE 1, 1: PRINT project: LOCATE 2, 1: PRINT tempX
END IF
NEXT i

IF backFace = TRUE THEN
	FOR i = 1 TO header.numTriangles
		'if one vertex of a triangle is clipped then skip the whole triagnle
		IF vidPoints(triangles(i).vert1).clip = FALSE AND vidPoints(triangles(i).vert2).clip = FALSE AND vidPoints(triangles(i).vert3).clip = FALSE THEN
			'connect vert1 to vert2
			LINE (vidPoints(triangles(i).vert1).x, vidPoints(triangles(i).vert1).y)-(vidPoints(triangles(i).vert2).x, vidPoints(triangles(i).vert2).y), clr
			'vert2 to vert3
			LINE (vidPoints(triangles(i).vert2).x, vidPoints(triangles(i).vert2).y)-(vidPoints(triangles(i).vert3).x, vidPoints(triangles(i).vert3).y), clr
			'vert3 to vert1
			LINE (vidPoints(triangles(i).vert3).x, vidPoints(triangles(i).vert3).y)-(vidPoints(triangles(i).vert1).x, vidPoints(triangles(i).vert1).y), clr
		END IF
	NEXT i
END IF

IF backFace = FALSE THEN
	FOR i = 1 TO header.numTriangles
		mul1 = (vidPoints(triangles(i).vert2).x - vidPoints(triangles(i).vert1).x)'
		mul2 = (vidPoints(triangles(i).vert1).y - vidPoints(triangles(i).vert3).y)
		zNorm1 = mul1 * mul2
		  
			mul1 = (vidPoints(triangles(i).vert2).y - vidPoints(triangles(i).vert1).x)'
			mul2 = (vidPoints(triangles(i).vert1).x - vidPoints(triangles(i).vert3).x)
			zNorm2 = mul1 * mul2
		  
			zNorm = zNorm1 - zNorm2
		  
			IF zNorm > 0 THEN
				'connect vert1 to vert2
				LINE (vidPoints(triangles(i).vert1).x, vidPoints(triangles(i).vert1).y)-(vidPoints(triangles(i).vert2).x, vidPoints(triangles(i).vert2).y), clr
				'  vert2 to vert3
				LINE (vidPoints(triangles(i).vert2).x, vidPoints(triangles(i).vert2).y)-(vidPoints(triangles(i).vert3).x, vidPoints(triangles(i).vert3).y), clr
				'vert3 to vert1
				LINE (vidPoints(triangles(i).vert3).x, vidPoints(triangles(i).vert3).y)-(vidPoints(triangles(i).vert1).x, vidPoints(triangles(i).vert1).y), clr
			END IF
		NEXT i

			END IF

	
END SUB

SUB rotatecube


DIM f AS INTEGER, i AS INTEGER, j AS INTEGER, k AS INTEGER
DIM temp AS Point3dType

FOR i = 1 TO 8
	realTrans(i).x = cube(i).x
	realTrans(i).y = cube(i).y
	realTrans(i).z = cube(i).z

	'--DO X AXIS
	'-rotate on x axis-
	'x is unaltered
	temp.y = ccos(xDeg) * realTrans(i).y - csin(xDeg) * realTrans(i).z
	temp.z = csin(xDeg) * realTrans(i).y + ccos(xDeg) * realTrans(i).z

	'-put new values into translated array-
	'x is unaltered
	realTrans(i).y = temp.y
	realTrans(i).z = temp.z


'--DO Y AXIS
	'-rotate on y axis-
	temp.x = ccos(yDeg) * realTrans(i).x + csin(yDeg) * realTrans(i).z
	'y is unaltered
	temp.z = (-csin(yDeg)) * realTrans(i).x + ccos(yDeg) * realTrans(i).z

	'-put new values into translated array-
	realTrans(i).x = temp.x
	'y is unaltered
	realTrans(i).z = temp.z


'--DO Z AXIS
	'-rotate on z axis-
	temp.x = ccos(zDeg) * realTrans(i).x - csin(zDeg) * realTrans(i).y
	temp.y = csin(zDeg) * realTrans(i).x + ccos(zDeg) * realTrans(i).y
	'PRINT temp.x: SLEEP
	'z is unaltered

	'-put new values into translated array-
	realTrans(i).x = temp.x
	realTrans(i).y = temp.y
	'z is unaltered

	realTrans(i).x = xLoc + temp.x
	realTrans(i).y = yLoc + temp.y
	realTrans(i).z = zLoc + temp.z
NEXT

END SUB

SUB RotateVerts (frame AS INTEGER)
  'rotates and places 'frame' vertices into realtrans

DIM i AS INTEGER, j AS INTEGER, k AS INTEGER
DIM temp AS Point3dType

FOR i = 1 TO header.numVertices
	realTrans(i).x = realVertTable(frame, i).x
	realTrans(i).y = realVertTable(frame, i).y
	realTrans(i).z = realVertTable(frame, i).z

	'--DO X AXIS
	'-rotate on x axis-
	'x is unaltered
	temp.y = ccos(xDeg) * realTrans(i).y - csin(xDeg) * realTrans(i).z
	temp.z = csin(xDeg) * realTrans(i).y + ccos(xDeg) * realTrans(i).z
 
	'-put new values into translated array-
	'x is unaltered
	realTrans(i).y = temp.y
	realTrans(i).z = temp.z


'--DO Y AXIS
	'-rotate on y axis-
	temp.x = ccos(yDeg) * realTrans(i).x + csin(yDeg) * realTrans(i).z
	'y is unaltered
	temp.z = (-csin(yDeg)) * realTrans(i).x + ccos(yDeg) * realTrans(i).z
 
	'-put new values into translated array-
	realTrans(i).x = temp.x
	'y is unaltered
	realTrans(i).z = temp.z


'--DO Z AXIS
	'-rotate on z axis-
	temp.x = ccos(zDeg) * realTrans(i).x - csin(zDeg) * realTrans(i).y
	temp.y = csin(zDeg) * realTrans(i).x + ccos(zDeg) * realTrans(i).y
	'z is unaltered
 
	'-put new values into translated array-
	realTrans(i).x = temp.x
	realTrans(i).y = temp.y
	'z is unaltered

	realTrans(i).x = xLoc + temp.x
	realTrans(i).y = yLoc + temp.y
	realTrans(i).z = zLoc + temp.z
NEXT i

END SUB

