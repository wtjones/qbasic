'                                CRON.BAS
'                              May 2001 by Travis Jones
'
'  CRON = Crappy TRON for up to 3 people
'  Simple light cycle game, plays with OR without humans
'***********************************************************************
'May 10, 2001 -- First version. Everything works but the CPU players
'                are controlled with 7 lines of code and they mostly kill
'                themselves.
'May 11, 2001 -- CPU beams can actually be tough opponents. They seem to do
'                wierd things that I can't explain sometimes.
'                Why do they usually take a turn on there first move?
'                I may never know.
'                It's hard to make an interesting CPU opponent when there is
'                so much arena space. The random barriers seem to help.
'May 11, 2001 - LATER:
'                Seems done as far as I can tell. Everything her is from
'                start to finish within 24 hours. (Maybe 4 hours of writing/
'                debugging total. Because of that, the logic is VERY sloppy.
'                Thats what happens when you don't have an overall program
'                flow in mind.

OPTION BASE 0     'Use zero-based arrays like C
DEFINT A-Z        'Any variables declared on the fly (sloppy)
                  'will default to an integer.
DECLARE SUB MoveCPU ()
DECLARE SUB CheckWinner ()
DECLARE SUB WaitRetrace ()
DECLARE SUB MoveBeams ()
DECLARE SUB ClearArena ()
DECLARE SUB DrawBeams ()
DECLARE SUB ResetBeams ()
DECLARE SUB CheckKeys ()
DECLARE SUB AskNumPlayers ()
DECLARE SUB SetRandomBeams ()
DECLARE SUB DrawBorder ()
DECLARE SUB DrawBarriers ()
DECLARE SUB ClearColor (clr AS INTEGER)
DECLARE FUNCTION UnitsToWall% (dir AS INTEGER, x AS INTEGER, y AS INTEGER)

CONST TRUE = -1
CONST FALSE = 0
CONST VGAInpStat = &H3DA      'input status word register on VGA card
CONST VGARetraceMask = &H8    'mask to get the fourth bit (AND operation)
CONST SCREENX = 319
CONST SCREENY = 199
CONST BORDERWIDTH = 30
CONST RANDOMBORDER = FALSE    'not really working yet
CONST BORDERVARY = 60         'how random the border thickness can be
CONST BORDERBOTTOM = SCREENY - BORDERWIDTH
CONST BORDERTOP = BORDERWIDTH
CONST BORDERLEFT = BORDERWIDTH
CONST BORDERRIGHT = SCREENX - BORDERWIDTH
CONST UP = 0
CONST DOWN = 2
CONST LEFT = 1
CONST RIGHT = 3
CONST HUMAN = 1
CONST CPU = 2
CONST MAXBEAMS = 4
CONST MAXHUMANS = 3
CONST FLOORCOLOR = 0
CONST CHOOSETIME = 3
CONST MUSTTURN = 50
CONST MAXBARRIERS = 15
CONST MINBARRIERS = 8
CONST BARRIERWIDTH = 30
CONST CLEARDEAD = TRUE
CONST FASTONCPU = TRUE     'speed up when no humans left?

TYPE BeamType
   x AS INTEGER
   y AS INTEGER
   dir AS INTEGER       'direction
   clr AS INTEGER       'color
   control AS INTEGER   'cpu or player?
   alive AS INTEGER
   lastTurn AS INTEGER  'amount of moves since last turn (CPU only)
   roomInFront AS INTEGER  'info for CPUs only
END TYPE

DIM SHARED beams(MAXBEAMS) AS BeamType
DIM SHARED numHumans AS INTEGER
DIM SHARED done AS INTEGER
DIM SHARED running AS INTEGER
DIM SHARED humansAlive AS INTEGER

'  --------- MAIN PROGRAM START ---------
RANDOMIZE TIMER
done = FALSE
running = FALSE
CALL SetRandomBeams
CALL ResetBeams

SCREEN 13
CALL AskNumPlayers

'--------------MAIN LOOP-------------------------------------|

WHILE done = FALSE
   CALL CheckKeys
   IF running = TRUE THEN
      CALL DrawBeams
      CALL MoveBeams
      CALL CheckWinner
      CALL MoveCPU
   ELSE
      LOCATE 12, 9: PRINT "Press a Key to Begin!!!"
   END IF
    
   IF humansAlive > 0 OR FASTONCPU = FALSE THEN CALL WaitRetrace
WEND

'--------------END MAIN LOOP---------------------------------|

END

DEFSNG A-Z
SUB AskNumPlayers

ok = FALSE
startTime = TIMER

LOCATE 11, 11: COLOR INT(RND * 6 + 2)
PRINT "How Many Players?"

FOR i = 1 TO 10
   k$ = INKEY$ 'flush key buffer
NEXT i

WHILE ok = FALSE  'done when we get numHumans filled properly
   LOCATE 14, 11: PRINT "Time to choose:"; INT(CHOOSETIME - (TIMER - startTime))
   k$ = INKEY$
   IF k$ <> "" THEN
      realNum = ASC(k$) - 48
      IF realNum > -1 AND realNum <= MAXHUMANS THEN
         numHumans = realNum: ok = TRUE
      END IF
   END IF

   IF TIMER - startTime > CHOOSETIME THEN   'if user takes too long noone plays
      numHumans = 0: ok = TRUE
      running = TRUE       'also might as well start
   END IF
WEND

IF numHumans > 0 THEN
   FOR i = 0 TO numHumans - 1
      beams(i).control = HUMAN
   NEXT i
END IF

FOR i = 0 TO MAXBEAMS - 1
   IF beams(i).control <> HUMAN THEN beams(i).control = CPU
NEXT i

CLS : CALL DrawBorder
IF numHumans = 0 THEN CALL ClearArena  'Because of yuky logic
                                       'already in the code.
END SUB

SUB CheckKeys

theKey$ = INKEY$

IF theKey$ <> "" AND running = FALSE THEN
   CALL ClearArena: CALL SetRandomBeams: running = TRUE
END IF

IF theKey$ = CHR$(27) THEN done = TRUE

IF numHumans > 0 THEN
   SELECT CASE theKey$
      '--- check input for player 1
      CASE "w"
         IF beams(0).dir <> DOWN THEN beams(0).dir = UP
      CASE "s"
         IF beams(0).dir <> UP THEN beams(0).dir = DOWN
      CASE "a"
         IF beams(0).dir <> RIGHT THEN beams(0).dir = LEFT
      CASE "d"
         IF beams(0).dir <> LEFT THEN beams(0).dir = RIGHT
   END SELECT
END IF

IF numHumans > 1 THEN
   SELECT CASE theKey$
      '--- check input for player 2
      CASE CHR$(0) + "H"     'up
         IF beams(1).dir <> DOWN THEN beams(1).dir = UP
      CASE CHR$(0) + "P"     'down
         IF beams(1).dir <> UP THEN beams(1).dir = DOWN
      CASE CHR$(0) + "K"     'left
         IF beams(1).dir <> RIGHT THEN beams(1).dir = LEFT
      CASE CHR$(0) + "M"    'right
         IF beams(1).dir <> LEFT THEN beams(1).dir = RIGHT
   END SELECT
END IF

IF numHumans > 2 THEN
   SELECT CASE theKey$
      '--- check input for player 3
      CASE "8"
         IF beams(2).dir <> DOWN THEN beams(2).dir = UP
      CASE "2"
         IF beams(2).dir <> UP THEN beams(2).dir = DOWN
      CASE "5"
         IF beams(2).dir <> UP THEN beams(2).dir = DOWN
      CASE "4"
         IF beams(2).dir <> RIGHT THEN beams(2).dir = LEFT
      CASE "6"
         IF beams(2).dir <> LEFT THEN beams(2).dir = RIGHT
   END SELECT
END IF




END SUB

SUB CheckWinner

numAlive = 0
startTime = TIMER
humansAlive = 0
FOR i = 0 TO MAXBEAMS - 1
   IF beams(i).alive = TRUE THEN
      numAlive = numAlive + 1: winner = i
      IF beams(i).control = HUMAN THEN humansAlive = humansAlive + 1
   END IF
NEXT i

IF numAlive = 1 THEN
   LOCATE 11, 12: PRINT "Winner is Player"; winner + 1: running = FALSE
   'SLEEP 1
   IF numHumans = 0 THEN
      CALL SetRandomBeams
      WHILE TIMER - startTime < CHOOSETIME
         IF INKEY$ = CHR$(27) THEN END
      WEND
      running = TRUE: CLS : CALL DrawBorder: CALL ClearArena
   ELSE SLEEP 1: CALL SetRandomBeams   'The beam resetting depends on key
                                       'press, so we need this ugly hack.
   END IF
   CALL ResetBeams
END IF

END SUB

SUB ClearArena
'Fills the arena bounds with the FLOORCOLOR.
'For convience it also fills new barriers back in.

LINE (BORDERLEFT, BORDERTOP)-(BORDERRIGHT, BORDERBOTTOM), FLOORCOLOR, BF
CALL DrawBarriers

END SUB

DEFINT A-Z
SUB ClearColor (clr AS INTEGER)
'Scans the arena area and replaces any color 'clr' to the floor color.
'Useful for removing a dead beam.

IF CLEARDEAD = TRUE THEN
   FOR y = BORDERTOP TO BORDERBOTTOM
      FOR x = BORDERLEFT TO BORDERRIGHT
         IF POINT(x, y) = clr THEN PSET (x, y), FLOORCOLOR
      NEXT x
   NEXT y
END IF

END SUB

SUB DrawBarriers

edge = 10
numBarriers = INT(RND * MAXBARRIERS - MINBARRIERS) + MINBARRIERS

FOR i = 1 TO numBarriers
  
   x1 = INT(RND * (BORDERRIGHT - BORDERLEFT)) + BORDERLEFT + edge
   y1 = INT(RND * (BORDERBOTTOM - BORDERTOP)) + BORDERTOP + edge
   x2 = INT(RND * (BORDERRIGHT - BORDERLEFT)) + BORDERLEFT
   y2 = INT(RND * (BORDERBOTTOM - BORDERTOP)) + BORDERTOP

   WHILE x2 - x1 > BARRIERWIDTH OR y2 - y1 > BARRIERWIDTH OR x2 < x1 OR y2 < y1

      WHILE x1 < BORDERLEFT OR x1 > BORDERRIGHT - edge OR y1 < BORDERTOP OR y1 > BORDERBOTTOM - edge
         x1 = INT(RND * (BORDERRIGHT - BORDERLEFT)) + BORDERLEFT + edge
         y1 = INT(RND * (BORDERBOTTOM - BORDERTOP)) + BORDERTOP + edge
      WEND

      x2 = INT(RND * BARRIERWIDTH) + 1
      y2 = INT(RND * BARRIERWIDTH) + 1
  
      WHILE x2 < BORDERLEFT OR x2 > BORDERRIGHT - edge OR y2 < BORDERTOP OR y2 > BORDERBOTTOM - edge
         x2 = INT(RND * (BORDERRIGHT - BORDERLEFT)) + BORDERLEFT + edge
         y2 = INT(RND * (BORDERBOTTOM - BORDERTOP)) + BORDERTOP + edge
      WEND
   WEND
   LINE (x1, y1)-(x2, y2), INT(RND * 10) + 20, BF

NEXT i

END SUB

DEFSNG A-Z
SUB DrawBeams

FOR i = 0 TO MAXBEAMS - 1
   IF beams(i).alive = TRUE THEN PSET (beams(i).x, beams(i).y), beams(i).clr
NEXT i

END SUB

DEFINT A-Z
SUB DrawBorder
'Draws a pretty arena border, assuming the default mode 13h palette.

x1 = 0: y1 = 0: x2 = SCREENX: y2 = SCREENY
clr = 20
IF RANDOMBORDER = TRUE THEN
   thick = INT(RND * BORDERVARY + 10)
ELSE thick = BORDERWIDTH
END IF

FOR i = 1 TO thick
   LINE (x1, y1)-(x2, y2), clr, B
   x1 = x1 + 1: y1 = y1 + 1
   x2 = x2 - 1: y2 = y2 - 1
   clr = clr + 1: IF clr > 30 THEN clr = 20 'Don't want yuky rainbow colors
                                            'from default palette.
NEXT i

END SUB

DEFSNG A-Z
SUB MoveBeams

FOR i = 0 TO MAXBEAMS - 1
   IF beams(i).alive = TRUE THEN
      SELECT CASE beams(i).dir
         CASE UP
            IF POINT(beams(i).x, beams(i).y - 1) = FLOORCOLOR THEN
               beams(i).y = beams(i).y - 1
            ELSE
               beams(i).alive = FALSE: CALL ClearColor(beams(i).clr)
            END IF
         CASE DOWN
            IF POINT(beams(i).x, beams(i).y + 1) = FLOORCOLOR THEN
               beams(i).y = beams(i).y + 1
            ELSE
               beams(i).alive = FALSE: CALL ClearColor(beams(i).clr)
            END IF
         CASE LEFT
            IF POINT(beams(i).x - 1, beams(i).y) = FLOORCOLOR THEN
               beams(i).x = beams(i).x - 1
            ELSE
               beams(i).alive = FALSE: CALL ClearColor(beams(i).clr)
            END IF
         CASE RIGHT
            IF POINT(beams(i).x + 1, beams(i).y) = FLOORCOLOR THEN
               beams(i).x = beams(i).x + 1
            ELSE
               beams(i).alive = FALSE: CALL ClearColor(beams(i).clr)
            END IF
      END SELECT
   END IF
NEXT i

END SUB

SUB MoveCPU
'The sole module handling the cpu players. Basically the CPUs may or may
'not turn based on:
'        1. Distance to hitting a wall.
'        2. How long its been since a turn (don't know why I thought of that).
'        3. If a CPU beam hits a 'goofChance' then they will turn the
'           opposite way than intended.

DIM x AS INTEGER, y AS INTEGER, cpuGrace AS INTEGER     'temps
DIM goofChance AS INTEGER, goof AS INTEGER
cpuGrace = 20
goofChance = 7
goof = FALSE
thinkQuick = INT(RND * 3) + 10  'if space is tight cpu shouldnt be
                                 'able to turn so quick
minTurn = 3

FOR i = 0 TO MAXBEAMS - 1
   x = beams(i).x: y = beams(i).y
   IF beams(i).control = CPU AND beams(i).alive = TRUE THEN
      IF UnitsToWall(beams(i).dir, x, y) < INT(RND * cpuGrace) AND beams(i).lastTurn > thinkQuick OR beams(i).roomInFront < INT(RND * 2) + minTurn THEN
         beams(i).lastTurn = 0
         SELECT CASE beams(i).dir
            CASE UP
               d1 = UnitsToWall(LEFT, x, y)
               d2 = UnitsToWall(RIGHT, x, y)
               IF d1 < d2 THEN
                  beams(i).dir = RIGHT
               ELSE beams(i).dir = LEFT
               END IF
            CASE DOWN
               d1 = UnitsToWall(RIGHT, x, y)
               d2 = UnitsToWall(LEFT, x, y)
               IF d1 < d2 THEN
                  beams(i).dir = LEFT
               ELSE beams(i).dir = RIGHT
               END IF
            CASE LEFT
               d1 = UnitsToWall(DOWN, x, y)
               d2 = UnitsToWall(UP, x, y)
               IF d1 < d2 THEN
                  beams(i).dir = UP
               ELSE beams(i).dir = DOWN
               END IF
            CASE RIGHT
               d1 = UnitsToWall(UP, x, y)
               d2 = UnitsToWall(DOWN, x, y)
               IF d1 < d2 THEN
                  beams(i).dir = DOWN
               ELSE beams(i).dir = UP
               END IF
         END SELECT
              
         'Maybe the CPU beam "goofs" and went the opposite way that is
         'optimal.
         IF INT(RND * goofChance) = 1 THEN 'goof=TRUE
            SELECT CASE beams(i).dir
               CASE UP
                  beams(i).dir = DOWN
               CASE DOWN
                  beams(i).dir = UP
               CASE LEFT
                  beams(i).dir = RIGHT
               CASE RIGHT
                  beams(i).dir = LEFT
            END SELECT
         END IF
     ' ELSE beams(i).lastTurn = beams(i).lastTurn + 1
      END IF
   END IF
   IF beams(i).alive = TRUE THEN beams(i).lastTurn = beams(i).lastTurn + 1
NEXT i

'update the amount of room in front of cpu beam
FOR i = 0 TO MAXBEAMS - 1
   beams(i).roomInFront = UnitsToWall(beams(i).dir, beams(i).x, beams(i).y)
NEXT i

END SUB

SUB ResetBeams
  
   FOR i = 0 TO MAXBEAMS - 1
      beams(i).alive = TRUE
   NEXT i

   beams(0).x = ((BORDERRIGHT - BORDERLEFT) / 2) + BORDERLEFT
   beams(0).y = BORDERBOTTOM
   beams(0).dir = UP

   beams(1).x = BORDERRIGHT
   beams(1).y = ((BORDERBOTTOM - BORDERTOP) / 2) + BORDERTOP
   beams(1).dir = LEFT

   beams(2).x = ((BORDERRIGHT - BORDERLEFT) / 2) + BORDERLEFT
   beams(2).y = BORDERTOP
   beams(2).dir = DOWN

   beams(3).x = BORDERLEFT
   beams(3).y = ((BORDERBOTTOM - BORDERTOP) / 2) + BORDERTOP
   beams(3).dir = RIGHT
END SUB

SUB SetRandomBeams
'Sets all the beams to a random color, making sure that they are all
'different colors.

DIM taken(0 TO MAXBEAMS - 1) AS INTEGER
DIM isTaken AS INTEGER
DIM try AS INTEGER

'This is an awful way to do it but it's simple and simple is good.
beams(0).clr = INT(RND * 25 + 30)

beams(1).clr = INT(RND * 25 + 30)
WHILE beams(1).clr = beams(0).clr
   beams(1).clr = INT(RND * 25 + 30)
WEND

beams(2).clr = INT(RND * 25 + 30)
WHILE beams(2).clr = beams(0).clr OR beams(2).clr = beams(1).clr
   beams(2).clr = INT(RND * 25 + 30)
WEND

beams(3).clr = INT(RND * 25 + 30)
WHILE beams(3).clr = beams(0).clr OR beams(3).clr = beams(1).clr OR beams(3).clr = beams(2).clr
   beams(3).clr = INT(RND * 25 + 30)
WEND

'This is the more 'elegant' code that I wanted to use, but it doesn't work
'right and seems to set all the beams to the same color.
'FOR I = 0 TO MAXBEAMS - 1
'   isTaken = FALSE
'   WHILE try = FLOORCOLOR OR isTaken = TRUE       'can't be same as floor
'      '  beams(i).clr =
'      try = INT(RND * 25 + 30)
'      FOR j = 0 TO MAXBEAMS - 1
'         IF try = taken(j) THEN isTaken = TRUE
'      NEXT j
'   WEND
'   beams(I).clr = try: taken(I) = try
'NEXT I

END SUB

DEFINT A-Z
FUNCTION UnitsToWall (dir AS INTEGER, x AS INTEGER, y AS INTEGER)
units = 0
xCast = x: yCast = y

SELECT CASE dir
   CASE UP
      WHILE POINT(xCast, yCast - 1) = FLOORCOLOR
         yCast = yCast - 1: units = units + 1
      WEND
   CASE DOWN
      WHILE POINT(xCast, yCast + 1) = FLOORCOLOR
         yCast = yCast + 1: units = units + 1
      WEND
   CASE LEFT
      WHILE POINT(xCast - 1, yCast) = FLOORCOLOR
         xCast = xCast - 1: units = units + 1
      WEND
   CASE RIGHT
      WHILE POINT(xCast + 1, yCast) = FLOORCOLOR
         xCast = xCast + 1: units = units + 1
      WEND
END SELECT

UnitsToWall = units

END FUNCTION

DEFSNG A-Z
SUB WaitRetrace
'------- 1997 by W Travis Jones ----------

DIM retrace AS INTEGER
DIM retStatus AS INTEGER

WHILE retrace = 1
   retStatus = INP(VGAInpStat)
   retStatus = (retStatus AND VGARetraceMask)   'if bit 4 is set we are in
                                                'a retrace
   IF retStatus <> 1 THEN retrace = 0
WEND

WHILE retrace = 0                      'now wait till a new retrace starts
   retStatus = INP(VGAInpStat)
   retStatus = (retStatus AND VGARetraceMask)
   IF retStatus <> 0 THEN retrace = 1
WEND

END SUB

