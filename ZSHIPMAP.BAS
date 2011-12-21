'-------------------- Zero-Gravity Flight Algorithm --------------------
'
'                             January 1997                          
'                                   by                               
'                          William Travis Jones  
'
'Basically a space ship that works like asteroids with multiple shots.
'I want to turn it into a game where you are flying around a sector in space
'and there are ships attacking you. There is lots more left to do.
'
'***************************** CREDITS: **************************
'  - Keyboard handler by Steven Sensarn.
'  - Some ideas by Marco Koegler and Brent P. Newhall
'
'***************************** FEATURES: *************************
'  - 360 degree movement.
'  - Decent speed on a decent computer (more optimizations to come).
'  - Supports multiple keys.
'  - Enable 'GOSUB printout' in the main loop to see what's going on.
'
'***************************** CONTROLS: *************************
'  -Left and right arrow keys rotate.
'  -Up arrow for thrust.
'  -Control shoots. (duh)
'
'***************************** ERRATA: ***************************
'  -The ship data type is called 'tnk' because it used to be a tank and I
'     haven't fixed it yet.
'  -I am trying to have global coordinates for making a game with enemies
'     but came across a few problems (I am actually still writing the
'     algorithms for handling a sector that the player can roam around in).
'     The planet circle is just for testing.
'  -Uses floating point but I don't know any better.
'  -The ship is still a plain triangle. I will eventually make a real
'     polygon figure.

'****************************  UPDATES: **************************
'  -The ship is now stationary with the stars moving around.
'     The stars are randomly generated and loop over the screen.
'  -January 2, 1997 :   Ship is now rendered in polygons with shipvert(1) as
'                       the front vertex.
'  -March 31, 1997 :    Added option to just erase the objects drawn instead
'                       of the whole screen.
DEFINT A-Z

'******************* SUBS *******************
DECLARE SUB SETVECT (S AS INTEGER, O AS INTEGER, I AS INTEGER)
DECLARE SUB GETVECT (S AS INTEGER, O AS INTEGER, I AS INTEGER)
DECLARE SUB KEYBOARD.IN (OLDSEG AS INTEGER, OLDOFF AS INTEGER)
DECLARE SUB KEYBOARD.OUT (OLDSEG AS INTEGER, OLDOFF AS INTEGER)

'******************* CONSTANTS **************
CONST NUM.KEYS = 10
CONST INDEX.UP = 0
CONST INDEX.DOWN = 1
CONST INDEX.LEFT = 2
CONST INDEX.RIGHT = 3
CONST INDEX.CTRL = 4
CONST INDEX.ALT = 5
CONST INDEX.SPACE = 6
CONST INDEX.ESC = 7
CONST INDEX.ENTER = 8
CONST INDEX.RSHIFT = 9
CONST false = 0, true = -1
CONST pi = 3.141592654#, centx = 160, centy = 100
CONST up = 0, down = 1, left = 2, right = 3, still = 4
CONST xboul = 0, xbour = 319, ybouu = 0, yboud = 199  'screen boundaries
CONST backgrnd = 2            'page used for blanking out screen
CONST tshots = 10             'total visible shots
CONST shotdelayloops = 5      'loops to wait till new shot fired
CONST rotatespeed = 6         '1, 3 or 6 works good
CONST maxspd = 30             'maximum velocity
CONST maxspdneg = (maxspd - maxspd * 2)   'the negative value
CONST tstars = 10             'affects speed greatly
CONST filled = false          'true for a solid-looking ship
CONST shipcolor = 1, shipfill = 1
'world constants
CONST maxworldx = 3000, maxworldy = 3000
CONST centworldx = 1500, centworldy = 1500

'******************* TYPES ******************
TYPE tnk
	x AS SINGLE
	y AS SINGLE
	dir AS INTEGER
	tdir AS INTEGER   'turrent direction
	tang AS INTEGER   'turrent angle
	turx AS INTEGER
	tury AS INTEGER
	spd AS SINGLE
	tspd AS SINGLE
	worldx AS SINGLE
	worldy AS SINGLE
END TYPE
TYPE shot
	x AS INTEGER
	y AS INTEGER
	ang AS INTEGER    'angle
	active AS INTEGER
	tspd AS INTEGER
	spd AS INTEGER
END TYPE
TYPE velocity
	x AS SINGLE
	y AS SINGLE
	thrust AS INTEGER
END TYPE
TYPE stardata
	x AS SINGLE
	y AS SINGLE
	clr AS INTEGER
END TYPE
TYPE vertex
	x AS INTEGER
	y AS INTEGER
	ang AS INTEGER
	radius AS INTEGER
END TYPE

'******************* DIMS *******************
DIM SHARED KEY.TABLE(0 TO (NUM.KEYS - 1)) AS INTEGER
DIM SHARED RAWKEY AS INTEGER
DIM SHARED OLD.ISR.SEG AS INTEGER, OLD.ISR.OFF AS INTEGER
DIM shot(1 TO tshots) AS shot
DIM shotdelay AS INTEGER               'counter for shot delay timer
DIM tnk AS tnk
DIM velocity AS velocity
DIM n AS INTEGER                       'looping integer
DIM quit AS INTEGER                    'used as boolean
DIM go AS INTEGER                      'boolean for 'UP' keypress
DIM ccos(0 TO 359) AS SINGLE, csin(0 TO 359) AS SINGLE
DIM star(1 TO tstars)  AS stardata
DIM addfrntx AS SINGLE, addfrnty AS SINGLE
DIM addbackx AS SINGLE, addbacky AS SINGLE
DIM shipvert(1 TO 3)  AS vertex
DIM planetvidx AS SINGLE, planetvidy AS SINGLE

'******************* Variable Set-Up ********
RANDOMIZE TIMER
'I only use 360 different values of sine and cosine, so I just load them into
'an array. The values are also converted to pi so it doesn't have to be done
'at runtime.
PRINT "Creating tables..."
FOR n = 0 TO 359
  ccos(n) = COS(n * pi / 180)
  csin(n) = SIN(n * pi / 180)
NEXT n
velocity.thrust = 1        'acceleration rate
tnk.dir = still
tnk.spd = 1: tnk.tspd = 6  'turrent speed should be 1, 3, 6 or 9
tnk.tdir = still
tnk.tang = 90   'point upward
tnk.x = centx: tnk.y = centy
tnk.worldx = centworldx: tnk.worldy = centworldy
planetx = 1500: planety = 1400
planetvidx = 160: planetvidy = 0
FOR n = 1 TO 3
	READ shipvert(n).ang: READ shipvert(n).radius
NEXT
FOR n = 1 TO tshots
	shot(n).spd = 4         'shot speed
NEXT
FOR n = 1 TO 3             'calculate ship polygon for first frame
	shipvert(n).x = tnk.x + shipvert(n).radius * (ccos(shipvert(n).ang))
	shipvert(n).y = tnk.y - shipvert(n).radius * (csin(shipvert(n).ang))
NEXT

CALL KEYBOARD.IN(OLD.ISR.SEG, OLD.ISR.OFF)
FOR n = 1 TO tstars     'generate the stars
	star(n).x = INT(RND * xbour)
	star(n).y = INT(RND * yboud)
	star(n).clr = 6 + INT(RND * 2) + 1
NEXT
SCREEN 7, , 1, 0               'use pages 0 (visual) and 1 (active)

'****************************************************************************
DO
	'CLS
	'LINE (0, 0)-(319, 199), 0, BF
	GOSUB erasestars
	GOSUB eraseship
	GOSUB eraseshots
	CIRCLE (planetvidx, planetvidy), 12, 0
  
	GOSUB getkey
	IF go = true THEN GOSUB calcvelocity
	IF tnk.tdir <> still THEN GOSUB rotship
	GOSUB movship
	GOSUB movshot
	'GOSUB printout      'interesting, but VERY slow
	GOSUB drawstars
	GOSUB drawship
	GOSUB drawshots
	CIRCLE (planetvidx, planetvidy), 12, 5
	PCOPY 1, 0           'copy complete screen to visualpage...
LOOP UNTIL quit = true
CALL KEYBOARD.OUT(OLD.ISR.SEG, OLD.ISR.OFF)
END
'****************************************************************************

drawtnk:
	IF filled = true THEN
		CIRCLE (tnk.x, tnk.y), 8: PAINT (tnk.x, tnk.y), 15
	ELSE CIRCLE (tnk.x, tnk.y), 8
	END IF
	'IF ((tnk.x - tnk.turx) < 50) AND ((tnk.x + tnk.turx) > 50) THEN
	LINE (tnk.x, tnk.y)-(tnk.turx, tnk.tury), 4
RETURN

drawshots:
	FOR n = 1 TO tshots
		IF shot(n).active = true THEN PSET (shot(n).x, shot(n).y), 14
	NEXT
RETURN
eraseshots:
	FOR n = 1 TO tshots
		IF shot(n).active = true THEN PSET (shot(n).x, shot(n).y), 0
	NEXT
RETURN

drawship:
	LINE (shipvert(1).x, shipvert(1).y)-(shipvert(2).x, shipvert(2).y), shipcolor
	LINE (shipvert(2).x, shipvert(2).y)-(shipvert(3).x, shipvert(3).y), shipcolor
	LINE (shipvert(3).x, shipvert(3).y)-(shipvert(1).x, shipvert(1).y), shipcolor
	IF filled = true THEN PAINT (160, 100), shipfill, shipcolor
RETURN
eraseship:
	LINE (shipvert(1).x, shipvert(1).y)-(shipvert(2).x, shipvert(2).y), 0
	LINE (shipvert(2).x, shipvert(2).y)-(shipvert(3).x, shipvert(3).y), 0
	LINE (shipvert(3).x, shipvert(3).y)-(shipvert(1).x, shipvert(1).y), 0
	IF filled = true THEN PAINT (160, 100), 0, 0
RETURN

drawstars:
	FOR n = 1 TO tstars
		PSET (star(n).x, star(n).y), star(n).clr
	NEXT
RETURN
erasestars:
	FOR n = 1 TO tstars
		PSET (star(n).x, star(n).y), 0
	NEXT
RETURN

getkey:
'RAWKEY = 0: WHILE RAWKEY = 0: WEND
go = false: tnk.tdir = still   'in case not holding down a key
IF KEY.TABLE(INDEX.RIGHT) THEN tnk.tdir = right
IF KEY.TABLE(INDEX.LEFT) THEN tnk.tdir = left
IF KEY.TABLE(INDEX.ESC) THEN quit = true
IF KEY.TABLE(INDEX.UP) THEN go = true
IF KEY.TABLE(INDEX.CTRL) THEN
	IF shotdelay < 0 THEN
		shotdelay = shotdelayloops    'amount of loops to wait till next shot
		FOR n = 1 TO tshots  'loop until we find an inactive shot, or not
			IF shot(n).active = false THEN
				shot(n).active = true: shot(n).x = shipvert(1).x
				shot(n).y = shipvert(1).y: shot(n).ang = shipvert(1).ang: EXIT FOR
			END IF
		NEXT
	END IF
END IF
	shotdelay = shotdelay - 1
RETURN

rotship:
	SELECT CASE tnk.tdir    'works for rotation speeds of 1, 3 or 6
		CASE left
			FOR n = 1 TO 3
			IF shipvert(1).ang <> 360 THEN
				shipvert(n).ang = shipvert(n).ang + rotatespeed
			ELSE shipvert(n).ang = 0 + rotatespeed
			END IF
			IF shipvert(n).ang = 360 THEN shipvert(n).ang = 0 'Didn't find a better way, so...
			NEXT
		CASE right
			FOR n = 1 TO 3
				IF shipvert(n).ang <> 0 THEN
					shipvert(n).ang = shipvert(n).ang - rotatespeed
				ELSE shipvert(n).ang = 360 - rotatespeed
				END IF
			NEXT
	END SELECT
	FOR n = 1 TO 3
		shipvert(n).x = tnk.x + shipvert(n).radius * (ccos(shipvert(n).ang))
		shipvert(n).y = tnk.y - shipvert(n).radius * (csin(shipvert(n).ang))
	NEXT
	tnk.tdir = still
RETURN

movship:     'moves in direction of turrent
	addfrntx = velocity.x / 10     'do this ahead of time for the loop
	addfrnty = velocity.y / 10
	addbackx = addfrntx / 1.5
	addbacky = addfrnty / 1.5
'   DIM jmp AS SINGLE
	'tnk.worldx = tnk.worldx + star(1).x - addfrntx
	'this is for the global coordinates but its not written how I want them
	'yet
	tnk.worldx = tnk.worldx + addfrntx
	planetvidx = planetvidx - addfrntx
	tnk.worldy = tnk.worldy + addfrnty
	planetvidy = planetvidy - addfrnty
	FOR n = 1 TO tstars        'yuk very slow
		IF star(n).clr = 7 THEN          'close stars
			star(n).x = star(n).x - addfrntx
			star(n).y = star(n).y - addfrnty
		ELSE                             'far stars that move slower
			star(n).x = star(n).x - addbackx
			star(n).y = star(n).y - addbacky
		END IF
	  
		IF tnk.worldx < 0 THEN tnk.worldx = 0
		IF tnk.worldx > maxworldx THEN tnk.worldx = maxworldx
		IF tnk.worldy < 0 THEN tnk.worldy = 0
		IF tnk.worldy > maxworldy THEN tnk.worldy = maxworldy
		IF star(n).x > xbour THEN star(n).x = xboul
		IF star(n).x < xboul THEN star(n).x = xbour
		IF star(n).y > yboud THEN star(n).y = ybouu
			'jmp = star(n).y + addy: star(n).y = ybouu + jmp
	  ' END IF
		IF star(n).y < ybouu THEN star(n).y = yboud
	NEXT
RETURN

calcvelocity:
	velocity.x = velocity.x + velocity.thrust * ccos(shipvert(1).ang)
	IF velocity.x > maxspd THEN velocity.x = maxspd
	IF velocity.x < maxspdneg THEN velocity.x = maxspdneg
	velocity.y = velocity.y - velocity.thrust * csin(shipvert(1).ang)
	IF velocity.y > maxspd THEN velocity.y = maxspd
	IF velocity.y < maxspdneg THEN velocity.y = maxspdneg
RETURN

movshot:
	FOR n = 1 TO tshots
		IF shot(n).active = true THEN
		'add = shot(n).ang * (pi / 180)
		shot(n).x = shot(n).x + shot(n).spd * (ccos(shot(n).ang))
		shot(n).y = shot(n).y - shot(n).spd * (csin(shot(n).ang))
		IF shot(n).x < xboul THEN shot(n).active = false
		IF shot(n).x > xbour THEN shot(n).active = false
		IF shot(n).y < ybouu THEN shot(n).active = false
		IF shot(n).y > yboud THEN shot(n).active = false
		END IF
	NEXT
RETURN

waitretrace:
FOR n = 1 TO delay
	WAIT &H3DA, 8: WAIT &H3DA, 8, 8
NEXT
RETURN

printout:
LOCATE 2, 2: PRINT "Angle:      "; shipvert(1).ang
LOCATE 3, 2: PRINT "Velocity X: "; velocity.x
LOCATE 4, 2: PRINT "Velocity Y: "; velocity.y
LOCATE 7, 2: PRINT "Worldx    : "; tnk.worldx
LOCATE 8, 2: PRINT "Worldy    : "; tnk.worldy
IF go = true THEN
LOCATE 6, 2: PRINT "Thrust:      "; "ON"
ELSE LOCATE 6, 2: PRINT "Thrust:      "; "OFF"
END IF
RETURN

'ship vertex data        - angle and radius of front, then left then right
DATA 90,10
DATA 210,6
DATA 330,6

DEFSNG A-M, O-Z
SUB GETVECT (S AS INTEGER, O AS INTEGER, I AS INTEGER)

	 'GETVECT RETURNS THE ADDRESS OF A FUNCTION POINTED TO IN THE
	 'INTERRUPT VECTOR TABLE (STARTS AT 0000:0000H)

	 STATIC ASM AS STRING 'THE CODE FOR GETVECT

	 STATIC INI AS INTEGER 'USED TO DETECT WHETHER GETVECT HAS PREVIOUSLY
								  'BEEN CALLED
	 IF INI = 0 THEN
		  
		  'CREATE ML FUNCTION IF NOT ALREADY CREATED

		  ASM = ASM + CHR$(&H55)                          'PUSH    BP
		  ASM = ASM + CHR$(&H89) + CHR$(&HE5)             'MOV     BP,SP
		  ASM = ASM + CHR$(&H8B) + CHR$(&H5E) + CHR$(&H6) 'MOV     BX,[BP+06]
		  ASM = ASM + CHR$(&H8A) + CHR$(&H7)              'MOV     AL,[BX]
		  ASM = ASM + CHR$(&HB4) + CHR$(&H35)             'MOV     AH,35
		  ASM = ASM + CHR$(&HCD) + CHR$(&H21)             'INT     21
		  ASM = ASM + CHR$(&H53)                          'PUSH    BX
		  ASM = ASM + CHR$(&H8B) + CHR$(&H5E) + CHR$(&HA) 'MOV     BX,[BP+0A]
		  ASM = ASM + CHR$(&H8C) + CHR$(&H7)              'MOV     [BX],ES
		  ASM = ASM + CHR$(&H8B) + CHR$(&H5E) + CHR$(&H8) 'MOV     BX,[BP+08]
		  ASM = ASM + CHR$(&H58)                          'POP     AX
		  ASM = ASM + CHR$(&H89) + CHR$(&H7)              'MOV     [BX],AX
		  ASM = ASM + CHR$(&H5D)                          'POP     BP
		  ASM = ASM + CHR$(&HCA) + CHR$(&H6) + CHR$(&H0)  'RETF    0006
		  INI = 1 'FLAG CREATION
	 END IF

	 DEF SEG = VARSEG(ASM)
	 CALL ABSOLUTE(S, O, I, SADD(ASM)) 'RUN FUNCTION
END SUB

SUB KEYBOARD.IN (OLDSEG AS INTEGER, OLDOFF AS INTEGER)
	 DIM RSGL AS INTEGER, RSGH AS INTEGER 'SEGMENT OF RAWKEY
	 DIM ROFL AS INTEGER, ROFH AS INTEGER 'OFFSET OF RAWKEY

	 DIM KSGL AS INTEGER, KSGH AS INTEGER 'SEGMENT OF KEY.TABLE
	 DIM KOFL AS INTEGER, KOFH AS INTEGER 'OFFSET OF KEY.TABLE
	
	 DIM BYTE AS STRING * 1 'USED TO ACTIVATE IRQ 1 IN PIC

	 STATIC ASM AS STRING 'HOLDS ISR

	 RSGL = VARSEG(RAWKEY) AND &HFF 'LOAD LOW "BYTE" SEGMENT
	 RSGH = INT(VARSEG(RAWKEY) / 256) AND &HFF 'LOAD HIGH "BYTE" SEGMENT

	 ROFL = VARPTR(RAWKEY) AND &HFF 'LOAD LOW "BYTE" OFFSET
	 ROFH = INT(VARPTR(RAWKEY) / 256) AND &HFF 'LOAD HIGH "BYTE" OFFSET
	 
	 KSGL = VARSEG(KEY.TABLE(0)) AND &HFF 'LOAD LOW "BYTE" SEGMENT
	 KSGH = INT(VARSEG(KEY.TABLE(0)) / 256) AND &HFF 'LOAD HIGH "BYTE" SEGMENT

	 KOFL = VARPTR(KEY.TABLE(0)) AND &HFF 'LOAD LOW "BYTE" OFFSET
	 KOFH = INT(VARPTR(KEY.TABLE(0)) / 256) AND &HFF 'LOAD HIGH "BYTE" OFFSET

	 'THIS IS THE ISR.  IT READS A SCANCODE FROM THE KEYBOARD BUFFER
	 'AND RESETS IT.  THE BEST PART IS, BIOS CAN'T TOUCH IT!

	 ASM = ""
	 ASM = ASM + CHR$(&H52)                          'PUSH DX
	 ASM = ASM + CHR$(&H51)                          'PUSH CX
	 ASM = ASM + CHR$(&H53)                          'PUSH BX
	 ASM = ASM + CHR$(&H50)                          'PUSH AX
	 ASM = ASM + CHR$(&H6)                           'PUSH ES
	 ASM = ASM + CHR$(&H57)                          'PUSH DI
	 ASM = ASM + CHR$(&H1E)                          'PUSH DS
	 ASM = ASM + CHR$(&H56)                          'PUSH SI
	 ASM = ASM + CHR$(&HFB)                          'STI
	 ASM = ASM + CHR$(&HBA) + CHR$(&H60) + CHR$(&H0) 'MOV DX,0060
	 ASM = ASM + CHR$(&HEC)                          'IN AL,DX
	 ASM = ASM + CHR$(&H30) + CHR$(&HE4)             'XOR AH,AH
	 ASM = ASM + CHR$(&HBA) + CHR$(RSGL) + CHR$(RSGH)'MOV DX,SEG RAWKEY
	 ASM = ASM + CHR$(&H8E) + CHR$(&HDA)             'MOV DS,DX
	 ASM = ASM + CHR$(&HBE) + CHR$(ROFL) + CHR$(ROFH)'MOV SI,OFFSET RAWKEY
	 ASM = ASM + CHR$(&H88) + CHR$(&H4)              'MOV [SI],AL
	 ASM = ASM + CHR$(&H50)                          'PUSH AX
	 ASM = ASM + CHR$(&HBA) + CHR$(&H61) + CHR$(&H0) 'MOV DX,0061
	 ASM = ASM + CHR$(&HEC)                          'IN AL,DX
	 ASM = ASM + CHR$(&HC) + CHR$(&H82)              'OR AL,82
	 ASM = ASM + CHR$(&HEE)                          'OUT DX,AL
	 ASM = ASM + CHR$(&H24) + CHR$(&H7F)             'AND AL,7F
	 ASM = ASM + CHR$(&HEE)                          'OUT DX,AL
	 ASM = ASM + CHR$(&HB0) + CHR$(&H20)             'MOV AL,20
	 ASM = ASM + CHR$(&HBA) + CHR$(&H20) + CHR$(&H0) 'MOV DX,0020
	 ASM = ASM + CHR$(&HEE)                          'OUT DX,AL
	 ASM = ASM + CHR$(&HBA) + CHR$(KSGL) + CHR$(KSGH)'MOV DX,SEG KEY.TABLE
	 ASM = ASM + CHR$(&H8E) + CHR$(&HDA)             'MOV DS,DX
	 ASM = ASM + CHR$(&HBE) + CHR$(KOFL) + CHR$(KOFH)'MOV SI,OFFSET KEY.TABLE
	 ASM = ASM + CHR$(&H58)                          'POP AX
	 ASM = ASM + CHR$(&HBB) + CHR$(&H1) + CHR$(&H0)  'MOV BX,0001--MAKE
	 ASM = ASM + CHR$(&HB4) + CHR$(&H48)             'MOV AH,48--UP
	 ASM = ASM + CHR$(&H38) + CHR$(&HC4)             'CMP AH,AL
	 ASM = ASM + CHR$(&H75) + CHR$(&H3)              'JNZ
	 ASM = ASM + CHR$(&H89) + CHR$(&H5C) + CHR$(&H0) 'MOV [SI+00],BX
	 ASM = ASM + CHR$(&HB4) + CHR$(&H50)             'MOV AH,50--DOWN
	 ASM = ASM + CHR$(&H38) + CHR$(&HC4)             'CMP AH,AL
	 ASM = ASM + CHR$(&H75) + CHR$(&H3)              'JNZ
	 ASM = ASM + CHR$(&H89) + CHR$(&H5C) + CHR$(&H2) 'MOV [SI+02],BX
	 ASM = ASM + CHR$(&HB4) + CHR$(&H4B)             'MOV AH,4B--LEFT
	 ASM = ASM + CHR$(&H38) + CHR$(&HC4)             'CMP AH,AL
	 ASM = ASM + CHR$(&H75) + CHR$(&H3)              'JNZ
	 ASM = ASM + CHR$(&H89) + CHR$(&H5C) + CHR$(&H4) 'MOV [SI+04],BX
	 ASM = ASM + CHR$(&HB4) + CHR$(&H4D)             'MOV AH,4D--RIGHT
	 ASM = ASM + CHR$(&H38) + CHR$(&HC4)             'CMP AH,AL
	 ASM = ASM + CHR$(&H75) + CHR$(&H3)              'JNZ
	 ASM = ASM + CHR$(&H89) + CHR$(&H5C) + CHR$(&H6) 'MOV [SI+06],BX
	 ASM = ASM + CHR$(&HB4) + CHR$(&H1D)             'MOV AH,1D--CTRL
	 ASM = ASM + CHR$(&H38) + CHR$(&HC4)             'CMP AH,AL
	 ASM = ASM + CHR$(&H75) + CHR$(&H3)              'JNZ
	 ASM = ASM + CHR$(&H89) + CHR$(&H5C) + CHR$(&H8) 'MOV [SI+08],BX
	 ASM = ASM + CHR$(&HB4) + CHR$(&H38)             'MOV AH,38--ALT
	 ASM = ASM + CHR$(&H38) + CHR$(&HC4)             'CMP AH,AL
	 ASM = ASM + CHR$(&H75) + CHR$(&H3)              'JNZ
	 ASM = ASM + CHR$(&H89) + CHR$(&H5C) + CHR$(&HA) 'MOV [SI+0A],BX
	 ASM = ASM + CHR$(&HB4) + CHR$(&H39)             'MOV AH,39--SPACE
	 ASM = ASM + CHR$(&H38) + CHR$(&HC4)             'CMP AH,AL
	 ASM = ASM + CHR$(&H75) + CHR$(&H3)              'JNZ
	 ASM = ASM + CHR$(&H89) + CHR$(&H5C) + CHR$(&HC) 'MOV [SI+0C],BX
	 ASM = ASM + CHR$(&HB4) + CHR$(&H1)              'MOV AH,01--ESC
	 ASM = ASM + CHR$(&H38) + CHR$(&HC4)             'CMP AH,AL
	 ASM = ASM + CHR$(&H75) + CHR$(&H3)              'JNZ
	 ASM = ASM + CHR$(&H89) + CHR$(&H5C) + CHR$(&HE) 'MOV [SI+0E],BX
	 ASM = ASM + CHR$(&HB4) + CHR$(&H1C)             'MOV AH,1C--ENTER
	 ASM = ASM + CHR$(&H38) + CHR$(&HC4)             'CMP AH,AL
	 ASM = ASM + CHR$(&H75) + CHR$(&H3)              'JNZ
	 ASM = ASM + CHR$(&H89) + CHR$(&H5C) + CHR$(&H10)'MOV [SI+10],BX
	 ASM = ASM + CHR$(&HB4) + CHR$(&H36)             'MOV AH,36--RSHIFT
	 ASM = ASM + CHR$(&H38) + CHR$(&HC4)             'CMP AH,AL
	 ASM = ASM + CHR$(&H75) + CHR$(&H3)              'JNZ
	 ASM = ASM + CHR$(&H89) + CHR$(&H5C) + CHR$(&H12)'MOV [SI+12],BX

	 ASM = ASM + CHR$(&HBB) + CHR$(&H0) + CHR$(&H0)  'MOV BX,0000--BREAK
	 ASM = ASM + CHR$(&HB4) + CHR$(&HC8)             'MOV AH,C8--UP
	 ASM = ASM + CHR$(&H38) + CHR$(&HC4)             'CMP AH,AL
	 ASM = ASM + CHR$(&H75) + CHR$(&H3)              'JNZ
	 ASM = ASM + CHR$(&H89) + CHR$(&H5C) + CHR$(&H0) 'MOV [SI+00],BX
	 ASM = ASM + CHR$(&HB4) + CHR$(&HD0)             'MOV AH,D0--DOWN
	 ASM = ASM + CHR$(&H38) + CHR$(&HC4)             'CMP AH,AL
	 ASM = ASM + CHR$(&H75) + CHR$(&H3)              'JNZ
	 ASM = ASM + CHR$(&H89) + CHR$(&H5C) + CHR$(&H2) 'MOV [SI+02],BX
	 ASM = ASM + CHR$(&HB4) + CHR$(&HCB)             'MOV AH,CB--LEFT
	 ASM = ASM + CHR$(&H38) + CHR$(&HC4)             'CMP AH,AL
	 ASM = ASM + CHR$(&H75) + CHR$(&H3)              'JNZ
	 ASM = ASM + CHR$(&H89) + CHR$(&H5C) + CHR$(&H4) 'MOV [SI+04],BX
	 ASM = ASM + CHR$(&HB4) + CHR$(&HCD)             'MOV AH,CD--RIGHT
	 ASM = ASM + CHR$(&H38) + CHR$(&HC4)             'CMP AH,AL
	 ASM = ASM + CHR$(&H75) + CHR$(&H3)              'JNZ
	 ASM = ASM + CHR$(&H89) + CHR$(&H5C) + CHR$(&H6) 'MOV [SI+06],BX
	 ASM = ASM + CHR$(&HB4) + CHR$(&H9D)             'MOV AH,9D--CTRL
	 ASM = ASM + CHR$(&H38) + CHR$(&HC4)             'CMP AH,AL
	 ASM = ASM + CHR$(&H75) + CHR$(&H3)              'JNZ
	 ASM = ASM + CHR$(&H89) + CHR$(&H5C) + CHR$(&H8) 'MOV [SI+08],BX
	 ASM = ASM + CHR$(&HB4) + CHR$(&HB8)             'MOV AH,B8--ALT
	 ASM = ASM + CHR$(&H38) + CHR$(&HC4)             'CMP AH,AL
	 ASM = ASM + CHR$(&H75) + CHR$(&H3)              'JNZ
	 ASM = ASM + CHR$(&H89) + CHR$(&H5C) + CHR$(&HA) 'MOV [SI+0A],BX
	 ASM = ASM + CHR$(&HB4) + CHR$(&HB9)             'MOV AH,B9--SPACE
	 ASM = ASM + CHR$(&H38) + CHR$(&HC4)             'CMP AH,AL
	 ASM = ASM + CHR$(&H75) + CHR$(&H3)              'JNZ
	 ASM = ASM + CHR$(&H89) + CHR$(&H5C) + CHR$(&HC) 'MOV [SI+0C],BX
	 ASM = ASM + CHR$(&HB4) + CHR$(&H81)             'MOV AH,81--ESC
	 ASM = ASM + CHR$(&H38) + CHR$(&HC4)             'CMP AH,AL
	 ASM = ASM + CHR$(&H75) + CHR$(&H3)              'JNZ
	 ASM = ASM + CHR$(&H89) + CHR$(&H5C) + CHR$(&HE) 'MOV [SI+0E],BX
	 ASM = ASM + CHR$(&HB4) + CHR$(&H9C)             'MOV AH,9C--ENTER
	 ASM = ASM + CHR$(&H38) + CHR$(&HC4)             'CMP AH,AL
	 ASM = ASM + CHR$(&H75) + CHR$(&H3)              'JNZ
	 ASM = ASM + CHR$(&H89) + CHR$(&H5C) + CHR$(&H10)'MOV [SI+10],BX
	 ASM = ASM + CHR$(&HB4) + CHR$(&HB6)             'MOV AH,B6--RSHIFT
	 ASM = ASM + CHR$(&H38) + CHR$(&HC4)             'CMP AH,AL
	 ASM = ASM + CHR$(&H75) + CHR$(&H3)              'JNZ
	 ASM = ASM + CHR$(&H89) + CHR$(&H5C) + CHR$(&H12)'MOV [SI+12],BX

	 ASM = ASM + CHR$(&HFA)                          'CLI
	 ASM = ASM + CHR$(&H5E)                          'POP SI
	 ASM = ASM + CHR$(&H1F)                          'POP DS
	 ASM = ASM + CHR$(&H5F)                          'POP DI
	 ASM = ASM + CHR$(&H7)                           'POP ES
	 ASM = ASM + CHR$(&H58)                          'POP AX
	 ASM = ASM + CHR$(&H5B)                          'POP BX
	 ASM = ASM + CHR$(&H59)                          'POP CX
	 ASM = ASM + CHR$(&H5A)                          'POP DX
	 ASM = ASM + CHR$(&HCF)                          'IRET

	 BYTE = CHR$(INP(&H21)) 'LOAD IRQ ENABLE REGISTER IN PIC

	 OUT &H21, (ASC(BYTE) AND (255 XOR 2)) 'CLEAR BIT 2 (IRQ 1)

	 CALL GETVECT(OLDSEG, OLDOFF, &H9) 'LOAD OLD ISR
	 CALL SETVECT(VARSEG(ASM), SADD(ASM), &H9) 'STORE NEW ISR
END SUB

SUB KEYBOARD.OUT (OLDSEG AS INTEGER, OLDOFF AS INTEGER)
	 CALL SETVECT(OLDSEG, OLDOFF, &H9) 'RESTORE OLD ISR
END SUB

SUB SETVECT (S AS INTEGER, O AS INTEGER, I AS INTEGER)

	 'SETVECT CHANGES THE ADDRESSES IN THE INTERRUPT VECTOR TABLE
	 'TO POINT TO NEW FUNCTIONS

	 STATIC ASM AS STRING 'HOLDS THE SETVECT FUNCTION
	 STATIC INI AS INTEGER 'USED TO TEST WHETHER OR NOT FUNCTION HAS PREVOUSLY
								  'BEEN CALLED
	 IF INI = 0 THEN

		  'CREATE FUNCTION IF NOT ALREADY CREATED

		  ASM = ""
		  ASM = ASM + CHR$(&H55)                          'PUSH BP
		  ASM = ASM + CHR$(&H89) + CHR$(&HE5)             'MOV BP,SP
		  ASM = ASM + CHR$(&H8B) + CHR$(&H5E) + CHR$(&H8) 'MOV BX,[BP+08]
		  ASM = ASM + CHR$(&H8B) + CHR$(&H17)             'MOV DX,[BX]
		  ASM = ASM + CHR$(&H8B) + CHR$(&H5E) + CHR$(&H6) 'MOV BX,[BP+06]
		  ASM = ASM + CHR$(&H8A) + CHR$(&H7)              'MOV AL,[BX]
		  ASM = ASM + CHR$(&H8B) + CHR$(&H5E) + CHR$(&HA) 'MOV BX,[BP+0A]
		  ASM = ASM + CHR$(&H1E)                          'PUSH DS
		  ASM = ASM + CHR$(&H8E) + CHR$(&H1F)             'MOV DS,[BX]
		  ASM = ASM + CHR$(&HB4) + CHR$(&H25)             'MOV AH,25
		  ASM = ASM + CHR$(&HCD) + CHR$(&H21)             'INT 21
		  ASM = ASM + CHR$(&H1F)                          'POP DS
		  ASM = ASM + CHR$(&H5D)                          'POP BP
		  ASM = ASM + CHR$(&HCA) + CHR$(&H6) + CHR$(&H0)  'RETF 0006
		  INI = 1 'FLAG CREATION
	 END IF
	 DEF SEG = VARSEG(ASM)
	 CALL ABSOLUTE(S, O, I, SADD(ASM)) 'RUN SETVECT
END SUB

