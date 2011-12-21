'-------------------- Zero-Gravity Flight Algorithm --------------------
'December 1996 by William Jones
'space ship that works like asteroids with multiple shots
'now has Steve Sensarns Keyboard ISR
'some code by Marco Koegler
'left and right arrow keys to rotate
'up arrow for thrust
'control shoots, even rapid fire

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
CONST darker = 0, lighter = 1, inward = 0, outward = 1
CONST up = 0, down = 1, left = 2, right = 3, still = 4
CONST backgrnd = 2            'page used for blanking out screen
CONST tshots = 10             'total visible shots
CONST shotdelayloops = 5      'loops to wait till new shot fired
CONST maxspd = 20             'maximum velocity
CONST maxspdneg = (maxspd - maxspd * 2)   'the negative value
CONST filled = false          'true for a solid-looking ship

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
	thrust AS SINGLE
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

'******************* Variable Set-Up ********
PRINT "Creating tables..."
FOR n = 0 TO 359
  ccos(n) = COS(n * pi / 180)
  csin(n) = SIN(n * pi / 180)
NEXT n
velocity.thrust = 1
tnk.dir = still
tnk.spd = 1: tnk.tspd = 6  'turrent speed should be 1, 3, 6 or 9
tnk.tdir = still
tnk.tang = 90   'point upward
tnk.x = centx: tnk.y = centy
FOR n = 1 TO tshots
	shot(n).spd = 4         'shot speed
NEXT
CALL KEYBOARD.IN(OLD.ISR.SEG, OLD.ISR.OFF)
SCREEN 7, , backgrnd, 0       'use a page for the background
FOR n = 1 TO 100              'fill the background page with stars
	PSET (INT(RND * 320), INT(RND * 199)), 6 + INT(RND * 2) + 1
NEXT
'LINE (0, 0)-(319, 199), , B   'video mode border for testing
'LINE (19, 19)-(300, 180), 4, B
SCREEN , , 1, 0               'use pages 0 and 1

'****************************************************************************
DO
	PCOPY backgrnd, 1    'revert to background, thus erasing everything else
	GOSUB getkey
	IF go = true THEN GOSUB calcvelocity
	GOSUB rottur
	GOSUB movtnk
	'GOSUB rottur   'this is now called after movtnk because of problems
	'GOSUB printout
	GOSUB movshot
	GOSUB drawtnk
	PCOPY 1, 0           'copy complete screen to visualpage...
	SCREEN , , 1, 0      '...and display it
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
	FOR n = 1 TO tshots
		IF shot(n).active = true THEN PSET (shot(n).x, shot(n).y)
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
				shot(n).active = true: shot(n).x = tnk.turx
				shot(n).y = tnk.tury: shot(n).ang = tnk.tang: EXIT FOR
			END IF
		NEXT
	END IF
END IF
shotdelay = shotdelay - 1
RETURN

rottur:
	SELECT CASE tnk.tdir    'works for turrent speeds of 1, 3, 6 and 9
		CASE left
			IF tnk.tang <> 360 THEN tnk.tang = tnk.tang + tnk.tspd ELSE tnk.tang = 0 + tnk.tspd
			IF tnk.tang = 360 THEN tnk.tang = 0 'Didn't find a better way, so...
		CASE right
			IF tnk.tang <> 0 THEN tnk.tang = tnk.tang - tnk.tspd ELSE tnk.tang = 360 - tnk.tspd
	END SELECT
	'theta = tnk.tang * (pi / 180)
	tnk.turx = tnk.x + 11 * (ccos(tnk.tang))
	tnk.tury = tnk.y - 11 * (csin(tnk.tang))
RETURN

movtnk:     'moves in direction of turrent
tnk.x = tnk.x + velocity.x / 10
tnk.y = tnk.y + velocity.y / 10
'tnk.x = tnk.x + tnk.spd * (COS(theta))
	'tnk.y = tnk.y - tnk.spd * (SIN(theta))
	IF tnk.x < -5 THEN tnk.x = 325
	IF tnk.x > 325 THEN tnk.x = -5
	IF tnk.y < -5 THEN tnk.y = 205
	IF tnk.y > 205 THEN tnk.y = -5
  
	'IF tnk.x < 31 THEN tnk.x = 31
	'IF tnk.x > 288 THEN tnk.x = 288
	'IF tnk.y < 31 THEN tnk.y = 31
	'IF tnk.y > 168 THEN tnk.y = 168
RETURN

calcvelocity:
	velocity.x = velocity.x + velocity.thrust * ccos(tnk.tang)
	IF velocity.x > maxspd THEN velocity.x = maxspd
	IF velocity.x < maxspdneg THEN velocity.x = maxspdneg
	velocity.y = velocity.y - velocity.thrust * csin(tnk.tang)
	IF velocity.y > maxspd THEN velocity.y = maxspd
	IF velocity.y < maxspdneg THEN velocity.y = maxspdneg
RETURN

movshot:
	FOR n = 1 TO tshots
		IF shot(n).active = true THEN
		'add = shot(n).ang * (pi / 180)
		shot(n).x = shot(n).x + shot(n).spd * (ccos(shot(n).ang))
		shot(n).y = shot(n).y - shot(n).spd * (csin(shot(n).ang))
		IF shot(n).x < -5 THEN shot(n).active = false
		IF shot(n).x > 325 THEN shot(n).active = false
		IF shot(n).y < -5 THEN shot(n).active = false
		IF shot(n).y > 205 THEN shot(n).active = false
		END IF
	NEXT
RETURN

erasetnk:
'   CIRCLE (tnk.x, tnk.y), 8, 0
'   LINE (tnk.x, tnk.y)-(tnk.turx, tnk.tury), 0
'   IF shot.active = 1 THEN PSET (shot.x, shot.y), 0
RETURN

waitretrace:
FOR n = 1 TO delay
	WAIT &H3DA, 8: WAIT &H3DA, 8, 8
NEXT
RETURN

printout:
LOCATE 2, 2: PRINT "Angle:      "; tnk.tang
LOCATE 3, 2: PRINT "Velocity X: "; velocity.x
LOCATE 4, 2: PRINT "Velocity Y: "; velocity.y
IF go = 1 THEN
LOCATE 6, 2: PRINT "Thrust:      "; "ON"
ELSE LOCATE 6, 2: PRINT "Thrust:      "; "OFF"
END IF
RETURN

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

