'------ VPong 1.1 ---------
'------ A vertical PONG game. ---------
'------ October 1996 Mike Rayl and William Travis Jones --------

'------ Player 1 layout: 4 is left / 6 is right / 5 is stop
'------ Player 2 layout: a is left / d is right / s is stop
'------ Just tap the keys when playing instead of holding them down.
'------ "9" turns on sound, "0" turns it off.
'------ "=" increases speed, "-" decreases speed.
'Version 1.1 now has flicker-free graphics and non-cpu dependent speed.
'Too bad the game is still just as lame as before.

SCREEN 13                               '320 x 200 resolution @ 256 colors.
RANDOMIZE TIMER                         'Use the clock for randomization.
DEFINT A-Z                              'Use less memory for more speed.
LOCATE 2, 2: INPUT "Play to how many"; result: CLS
CONST true = 1, false = 0: done = 0     'Use true and false for boolean
soundon = false                         'values.
target = 160: target2 = 160: delay = 18000
speed = 1   'only really works if either 1 or 2
newball = true                          'We need a new ball.
                      
'---------------- main ----------
        GOSUB drawborder
WHILE done <> true
        IF advmode = true THEN GOSUB advan
        IF newball = true THEN GOSUB startball  'Pick a direction and start.
        GOSUB drawsc                    'Update the screen.
        GOSUB getkey                    'Read keyboard.
        GOSUB calc                      'Calculate the paddles.
        GOSUB ballcalc                  'Calculate the ball.
WEND
END
'--------------- end main -------

drawsc:
        LINE (target - 20, 167)-(target + 20, 173), 4, B
        PAINT (target, 168), 4
        LINE (target2 - 20, 27)-(target2 + 20, 33), 1, B
        PAINT (target2, 28), 1
        CIRCLE (ballx, bally), 5, 15
       
       ' FOR z = 1 TO delay: NEXT        'Slow things down.
        WAIT &H3DA, 8: WAIT &H3DA, 8, 8
        LINE (target - 20, 167)-(target + 20, 173), 0, B
        PAINT (target, 168), 0
        LINE (target2 - 20, 27)-(target2 + 20, 33), 0, B
        PAINT (target2, 28), 0
        CIRCLE (ballx, bally), 5, 0
RETURN

getkey:
        thekey$ = INKEY$
        IF thekey$ = "4" THEN d1 = 4
        IF thekey$ = "5" THEN d1 = 5
        IF thekey$ = "6" THEN d1 = 6
        IF thekey$ = "a" THEN d2 = 4
        IF thekey$ = "s" THEN d2 = 5
        IF thekey$ = "d" THEN d2 = 6
        IF thekey$ = "\" THEN advmode = true
        IF thekey$ = "q" THEN done = true
        'IF thekey$ = "=" THEN speed = speed - 1
        'IF thekey$ = "-" THEN speed = speed + 1
        IF thekey$ = "9" THEN soundon = true
        IF thekey$ = "0" THEN soundon = false
RETURN

calc:
       SELECT CASE d1
                CASE IS = 4
                        target = target - speed
                        r = true
                CASE IS = 6
                        target = target + speed
                        r = true
                CASE IS = 5
                        r = false
        END SELECT
        SELECT CASE d2
                CASE IS = 4
                        target2 = target2 - speed
                        r2 = true
                CASE IS = 6
                        target2 = target2 + speed
                        r2 = true
                CASE IS = 5
                        r2 = false
        END SELECT
       
        IF target < 62 THEN target = 62         'These four lines make sure
        IF target > 258 THEN target = 258       'that the paddles dont hit
        IF target2 < 62 THEN target2 = 62       'the border.
        IF target2 > 258 THEN target2 = 258
RETURN

ballcalc:
      SELECT CASE balld                         'The layout of the numeric
                CASE IS = 8                     'is used to visulize
                        bally = bally - speed      'the direction of the ball.
                CASE IS = 2
                        bally = bally + speed
                CASE IS = 7
                        ballx = ballx - speed
                        bally = bally - speed
                CASE IS = 9
                        ballx = ballx + speed
                        bally = bally - speed
                CASE IS = 1
                        ballx = ballx - speed
                        bally = bally + speed
                CASE IS = 3
                        ballx = ballx + speed
                        bally = bally + speed
        END SELECT
        SELECT CASE bally       'To find out if ball hit bottom paddle.
                CASE IS = 162
                IF ballx > target - 23 THEN onpadl = true
                IF ballx < target + 23 THEN onpadr = true
                IF onpadl = true AND onpadr = true THEN GOSUB padhit
        END SELECT
       
        SELECT CASE bally       'To find out if ball hit top paddle.
                CASE IS = 38
                IF ballx > target2 - 23 THEN onpadl2 = true
                IF ballx < target2 + 23 THEN onpadr2 = true
                IF onpadl2 = true AND onpadr2 = true THEN GOSUB padhit2
        END SELECT
        onpadl = false
        onpadr = false
        onpadl2 = false
        onpadr2 = false
        SELECT CASE ballx       'This CASE detects hits on the sides.
                CASE IS < 47
                        IF balld = 7 THEN balld = 9
                        IF balld = 1 THEN balld = 3
                        IF soundon = true THEN PLAY "b"
                CASE IS > 273
                        IF balld = 9 THEN balld = 7
                        IF balld = 3 THEN balld = 1
                        IF soundon = true THEN PLAY "b"
        END SELECT
        
        IF bally > 170 THEN newball = true      'The ball hit the bottom.
        IF bally > 170 THEN p2 = p2 + 1         'Add one for player 2.
        IF bally < 30 THEN newball = true       'The ball hit the top.
        IF bally < 30 THEN p1 = p1 + 1          'Add one for player 1.
        
RETURN

padhit:         'This routine finds out which part of bottom paddle is hit
                'and sends the ball rebounding in the appropriate direction.
IF soundon = true THEN PLAY "a"
lpadrange = target - 7
rpadrange = target + 7

IF ballx < lpadrange THEN
        balld = 7
ELSEIF ballx > rpadrange THEN
        balld = 9
ELSE balld = 8
END IF

RETURN

padhit2:        'This routine finds out which part of top paddle is hit
                'and sends the ball rebounding in the appropriate direction.
IF soundon = true THEN PLAY "g"
lpadrange2 = target2 - 7
rpadrange2 = target2 + 7

IF ballx < lpadrange2 THEN
        balld = 1
ELSEIF ballx > rpadrange2 THEN
        balld = 3
ELSE balld = 2
END IF

RETURN

startball:
        LOCATE 3, 20: PRINT p1                  'Update player 1 score.
        LOCATE 23, 20: PRINT p2                 'Update player 2 score.
        IF p1 = result THEN GOSUB doResult
        IF p2 = result THEN GOSUB doResult

        FOR abc = 1 TO 3000
                LOCATE 13, 15: PRINT "Get Ready!!!"
        NEXT
        LOCATE 13, 15: PRINT "            "
        ballx = 160: bally = 100: first = INT(RND * 2)
        IF first = 1 THEN balld = 8
        IF first = 0 THEN balld = 2
        newball = false
        target = 160: target2 = 160: d1 = d: d2 = 5
RETURN

doResult:
        IF p1 = result THEN
        CLS : FOR ab = 1 TO 4000
        LOCATE 13, 14: PRINT "player 1 wins": NEXT
        END IF
        IF p2 = result THEN
        CLS : FOR ab = 1 TO 4000
        LOCATE 13, 14: PRINT "player 2 wins": NEXT
        END IF
        END
RETURN
drawborder:
        tlx = 39: tly = 24: brx = 281: bry = 176
        LINE (40, 25)-(280, 175), , B           'Draw the border
        colr = 16
        FOR c = 1 TO 15
                LINE (tlx, tly)-(brx, bry), colr, B
                tlx = tlx - 1
                tly = tly - 1
                brx = brx + 1
                bry = bry + 1
                colr = colr + 1
        NEXT
        colr = 1
RETURN

advan:
        CLS
        LOCATE 3, 3: PRINT "Pause"
        LOCATE 7, 3: PRINT "Hit '\' return to game"
        WHILE INKEY$ <> "\"
       
        WEND
        advmode = false
        CLS
        LINE (40, 25)-(280, 175), , B: GOSUB drawborder
RETURN

cpu:                            'This routine is disabled.
        SELECT CASE l1
                CASE IS = 0
                        d1 = INT(RND * 3) + 1
                        l1 = INT(RND * 30) + 1
        END SELECT
        SELECT CASE l2
                CASE IS = 0
                        d2 = INT(RND * 3) + 1
                        l2 = INT(RND * 30) + 1
        END SELECT

        IF l1 <> 0 THEN l1 = l1 - 1
        IF l2 <> 0 THEN l2 = l2 - 1

                'Put the following 2 lines before the main loop if you want
                'to use this routine.
         d1 = 2: l1 = 0
         d2 = 2: l2 = 0
RETURN

