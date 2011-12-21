'------ VBlock ---------
'------ A block-bashing game. ---------
'------ July 1996 William Travis Jones --------

'------ Keyboard layout: 4 is left / 6 is right / 5 is stop / f is fire
'------ Just tap the keys when playing instead of holding them down.
'------ "9" turns on sound, "0" turns it off.
'------ "=" increases speed, "-" decreases speed.

'  Destroying a block gives you 100 points. If it hits the player you lose
'  a life.
'  This game is sort of a remake of QB Space 1.0 by Enrique A. Compa¤ Gzz.

'  There are several diferences from QB Space:
'     1. I used 640 x 480 with standard QBASIC graphics instead of ASCII.
'        This alows for smoother, faster, and more acurate play. Some may
'        disagree, but I have had no end of trouble with QBASIC's ASCII
'        routines.
'     2. The control is completely different due to a QBASIC key input
'        problem that I could not find a work-around for. I would rather have
'        the player move as a key is held down, but instead QBASIC gives the
'        keyboard a null (false) every once in a while for text input. I
'        still haven't seen a fix for this. If someone knows how to directly
'        read the keyboard, please let me know.
'     3. The game continues while a shot is active.
'     4. You die if hit by a block.       (this isn't true yet)
'     5. Completely structured and commented code.   (sort of true)

'  Speaking of comments, I try to explain what is going on for beginners
'  that want to learn to program. Learning how to make a computer work for
'  you is hard, but really fun when you understand something new.

'  This program may throw you off with the timecount and enemycount stuff,
'  but the bottom-line is that the only thing that gets a chance to move
'  every loop is
'  the shot. The player has to wait for the shot to move 8 pixels, and the
'  enemy has to wait for 3 pixels. If I didn't do this, all three objects
'  would move at the same speed.
                                                                    
TYPE shot         'This is a data-type for the bullet things. Don't let the
   x AS INTEGER   'syntax scare you, it's just exactly the same as if I
   y AS INTEGER   'used "shotx" and "shoty". I don't normally use this form
END TYPE          'but I thought I might try it.

TYPE player       'This is a data-type for the triangular player thing.
   x AS INTEGER
   y AS INTEGER
   direction AS INTEGER    '3 possible values, left, right, or still
   score AS INTEGER
   lives AS INTEGER
END TYPE

TYPE enemy
   x AS INTEGER
   y AS INTEGER
   alive AS INTEGER     'This will be used as boolean value.
END TYPE

DEFINT A-Z              'Make all variables integers for speed increase.
CONST true = -1, false = NOT true: left = 0: right = 1: still = 2
CONST leftmaximum = 211, rightmaximum = 429
done = false: shotactive = false
player.x = 320: player.y = 430: player.direction = still
player.score = 0: player.lives = 5

CONST delay = 1400   'The higher the number, the slower the game.

'------------------main--------------
SCREEN 12            '640 x 480 in 16 (4-bit) colors.
RANDOMIZE TIMER      'Use clock for randomization.
GOSUB drawlayout
GOSUB newenemy       'Start with a block.
WHILE done = false           'game loop
   GOSUB updatescreen
   GOSUB calculate
   
   timecount = timecount + 1: IF timecount = 9 THEN timecount = 0
   enemycount = enemycount + 1: IF enemycount = 3 THEN enemycount = 0
WEND
END
'------------------end main----------

getkey:
   userkey$ = INKEY$    'Only read INKEY$ once for speed.
   IF userkey$ = "4" THEN player.direction = left
   IF userkey$ = "6" THEN player.direction = right
   IF userkey$ = "5" THEN player.direction = still
   IF userkey$ = "f" AND shotactive = false THEN GOSUB newshot
   IF userkey$ = "q" THEN done = true
RETURN

updatescreen:
   IF shotactive = true THEN PSET (shot.x, shot.y)
   GOSUB drawplayer
   IF enemy.alive = true THEN GOSUB drawenemy
   FOR a = 1 TO delay: NEXT
   IF shotactive = true THEN PSET (shot.x, shot.y), 0
   IF timecount = 8 THEN GOSUB eraseplayer
   IF enemycount = 2 THEN GOSUB eraseenemy
RETURN

calculate:
   IF shotactive = true THEN shot.y = shot.y - 1   'The only object to move
                                                   'each loop
   IF shot.y < 41 THEN shotactive = false          'Shot only goes to top
                                                   'of screen
  
      'Let's see if the shot is at the enemy's base.
   IF shot.y = enemy.y + 10 AND enemy.alive = true THEN GOSUB checkhit

   IF timecount = 8 THEN
      GOSUB getkey
      IF player.direction = left THEN player.x = player.x - 1
      IF player.direction = right THEN player.x = player.x + 1
   END IF

   IF player.x < leftmaximum THEN player.x = leftmaximum    'Keep the player
   IF player.x > rightmaximum THEN player.x = rightmaximum  'in bounds.
   IF enemycount = 2 THEN enemy.y = enemy.y + 1
   IF enemy.y > 421 AND enemy.alive = true THEN    'A block went through.
      player.lives = player.lives - 1: GOSUB updatestatus
   END IF
   IF enemy.y > 421 THEN GOSUB newenemy   'So a block does not immediatly
                                          'appear after destroying one.
RETURN

drawplayer:    'Draw the ugly triangle.
   LINE (player.x - 10, player.y)-(player.x + 10, player.y)
   LINE (player.x - 10, player.y)-(player.x, player.y - 10)
   LINE (player.x + 10, player.y)-(player.x, player.y - 10)
RETURN

eraseplayer:   'Erase the ugly triagle.
   LINE (player.x - 10, player.y)-(player.x + 10, player.y), 0
   LINE (player.x - 10, player.y)-(player.x, player.y - 10), 0
   LINE (player.x + 10, player.y)-(player.x, player.y - 10), 0
RETURN

drawenemy:
   LINE (enemy.x - 8, enemy.y - 10)-(enemy.x + 8, enemy.y + 10), , B
RETURN

eraseenemy:
   LINE (enemy.x - 8, enemy.y - 10)-(enemy.x + 8, enemy.y + 10), 0, B
RETURN

checkhit:   'This checks to see if the shot is in boundaries of a hit.
   leftok = false: rightok = false        'Let's assume it missed.
   IF shot.x > enemy.x - 10 THEN leftok = true
   IF shot.x < enemy.x + 10 THEN rightok = true
   IF leftok = true AND rightok = true THEN     'A direct hit.
      enemy.alive = false: shotactive = false
      player.score = player.score + 100: GOSUB updatestatus
   END IF
RETURN

newshot:
   shotactive = true
   shot.x = player.x: shot.y = player.y - 10
RETURN

newenemy:
   DO
      enemyok = true
      enemy.x = INT(RND * 440)
      IF enemy.x < 210 THEN enemyok = false
      IF enemy.x > 430 THEN enemyok = false
   LOOP UNTIL enemyok = true
   enemy.y = 51
   enemy.alive = true
RETURN

updatestatus:
   LOCATE 4, 68: PRINT player.score
   LOCATE 5, 68: PRINT player.lives
RETURN

drawlayout:
   LINE (200, 40)-(440, 40)            'draw top
   LINE (200, 40)-(200, 440)           'draw left
   LINE (440, 40)-(440, 440)           'draw right
   LINE (200, 440)-(440, 440)          'draw bottom
   b = 439                             'these 4 lines draw the red
   FOR a = 1 TO 8                      'at bottom of screen
      LINE (201, b)-(439, b), 4
   b = b - 1
   NEXT
   LOCATE 4, 60: PRINT "Score:"
   LOCATE 5, 60: PRINT "Lives:"
   GOSUB updatestatus
RETURN

