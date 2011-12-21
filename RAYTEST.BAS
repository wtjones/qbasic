'      Sector Raycast

'      January 1997 by William Jones
'---------
'  My first implementation of a Wolfenstien Raycaster. I wrote it in about
'3-4 days.
'---------
Features:
'  * VERY ugly graphics.
'  * VERY slow performance. Not even barely sutible for real-time motion.
'    Takes about 1 second per frame on my P90
'  * Nasty sloppy code. Not my usual taste. I will clean it up when I
'    Actually HAVE something worth cleaning. I normally comment everything.
'---------
'  The 'Sector' in 'Sector Raycast' just refers to the whole sector jumping
'I am trying to perfect to increase the speed of the program. Although it
'does not work very well yet, It means that instead of sending the ray to
'each positon of the grid, just send it the the border of each sector and
'check if there is a wall. So far It seems If I send the ray more than 10
'pixels, things get weird.
'---------
'  Here is how I set this up:
'  The map is a grid of 10x8 sectors. Each sector has 20x20 positions in it
'(the floating point math kind of messes that up). When you leave a sector,
'it puts you at a whole number of the border.
'---------
'  Errata:
'  In general I am supposed to send out 320 rays (or how many horizontal
'pixels in the screen mode you are using), but the speed is so horrible all
'ready that I just stuck with 60. I tried 320 but it didn't help the graphics
'much anyway. Everything still looked blocky. Increment the angle at .18
'instead of 1 and fiddle around with the DrawScreen to try it out yourself.
'----------
'  Even with just drawing lines, I would like the walls to look more like
'polygons that big legos. If anyone has a suggestion or 2 about improving
'this program, PLEASE help me.
'----------
'  Controls:
'     TAB- toggles between view(first person) or map(overhead) modes
'     8  - move forward
'     4  - rotate left
'     6  - rotate right

'***********************CONSTANTS**********************
CONST pi = 3.14159
CONST left = 0, right = 1, still = 2   'for rotating
CONST rotatespeed = 6   'this should be 1, 3, 6, or 9 for best results
CONST walkspeed = 4    'should be 2 or less (best is 1)
CONST true = -1, false = 0
CONST activepage = 1, visualpage = 0, backgroundpage = 2
CONST raypage = 3 'all the ray drawing
CONST maxdistance = 256          'max distance of ray (c2 = a2 + b2)
CONST macdisfloat = 256.12496#   'just in case
CONST mapmode = 0, viewmode = 1  'screen modes  'hit tab to change
'***********************END CONSTANTS**********************

'***********************DATA TYPES**********************
TYPE playerdata
   mapx AS INTEGER      'position on the map grid (10x8)
   mapy AS INTEGER
   locx AS SINGLE       'position in the 20x20 of each square
   locy AS SINGLE
   glbx AS SINGLE       'global position (200x160)
   glby AS SINGLE
   arrx AS SINGLE       'to draw an arrow to show angle
   arry AS SINGLE
   rotate AS INTEGER    'left, right, or still(stop)
   forward AS INTEGER   'true or false
   angle AS INTEGER    'from 0 to 359
END TYPE
TYPE raydata
   mapx AS INTEGER
   mapy AS INTEGER
   locx AS SINGLE
   locy AS SINGLE
   glbx AS SINGLE
   glby AS SINGLE
   angle AS INTEGER
END TYPE
TYPE mapdata
   clr AS INTEGER
END TYPE
'***********************END DATA TYPES**********************

'***********************ARRAYS**********************
DIM n AS INTEGER           'looping variable
DIM plyr AS playerdata     'the player   (example: plyr.mapx = 31)
DIM ray AS raydata
DIM map(0 TO 9, 0 TO 7) 'map array - 10 x 8
DIM wallsize(0 TO 200)
DIM ccos(0 TO 359) AS SINGLE, csin(0 TO 359) AS SINGLE
DIM mode AS INTEGER  'map or view
'***********************END ARRAYS**********************


'-----------------------------------------------------------------
'***********************MAIN PROGRAM**********************
'-----------------------------------------------------------------
GOSUB HeightTable
'GOSUB AltHeightTable       'for messing around

PRINT "creating tables..."
FOR n = 0 TO 359
  ccos(n) = COS(n * pi / -180)
  csin(n) = SIN(n * pi / -180)
NEXT n

GOSUB ReadMapData
plyr.mapx = 5: plyr.mapy = 6
plyr.locx = 10: plyr.locy = 10   'coords in local square
plyr.glbx = (plyr.mapx * 20) + plyr.locx
plyr.glby = (plyr.mapy * 20) + plyr.locy
plyr.rotate = still
plyr.forward = false
plyr.angle = 270

'we now draw the walls and the grid to the background page
SCREEN 7, , backgroundpage, visualpage    'background page is active
GOSUB DrawWalls         'draws the solid shapes
GOSUB DrawMap           'draws the grid lines
SCREEN 7, , activepage, visualpage        'back to regular
rayonce = 0
DO                      'main program loop
'PCOPY backgroundpage, activepage    'start with background
IF rayonce = 0 THEN GOSUB SendRay: rayonce = 1
GOSUB GetKey
IF plyr.forward = true THEN GOSUB Move
GOSUB RotatePlayer
IF mode = mapmode THEN GOSUB DrawPlayer
'GOSUB SendRay
GOSUB PrintOut
IF mode = mapmode THEN
   PCOPY activepage, visualpage
   SCREEN 7, , activepage, visualpage
END IF
LOOP UNTIL done = true'exit on esc key

END
'-----------------------------------------------------------------
'***********************END MAIN PROGRAM*******************
'-----------------------------------------------------------------


'***********************SUBROUTINES**********************
Move:
   lx = plyr.locx: ly = plyr.locy      'save old values in case of wall
   gx = plyr.glbx: gy = plyr.glby
  
   plyr.glbx = plyr.glbx + walkspeed * (ccos(plyr.angle))
   plyr.glby = plyr.glby - walkspeed * (csin(plyr.angle))
   
   plyr.locx = plyr.locx + walkspeed * (ccos(plyr.angle))
   plyr.locy = plyr.locy - walkspeed * (csin(plyr.angle))
     
      'check if moving to another sector and make sure there is no wall
      'also will change coors to fixed numbers for alignment
  
   IF plyr.locx < 0 THEN      'leaving sector to the left
      IF map(plyr.mapx - 1, plyr.mapy) = 0 THEN    'if next sector is clear
         plyr.locx = 20: plyr.mapx = plyr.mapx - 1
         plyr.glbx = (plyr.mapx * 20) + plyr.locx  'recaclulate global
      ELSE
         plyr.locx = lx: plyr.glbx = gx            'or just leave them there
      END IF
   END IF
   IF plyr.locx > 20 THEN
      IF map(plyr.mapx + 1, plyr.mapy) = 0 THEN
         plyr.locx = 0: plyr.mapx = plyr.mapx + 1
         plyr.glbx = (plyr.mapx * 20) + plyr.locx
      ELSE
         plyr.locx = lx: plyr.glbx = gx
      END IF
   END IF
   IF plyr.locy < 0 THEN
      IF map(plyr.mapx, plyr.mapy - 1) = 0 THEN
         plyr.locy = 20: plyr.mapy = plyr.mapy - 1
         plyr.glby = (plyr.mapy * 20) + plyr.locy
      ELSE
         plyr.locy = ly: plyr.glby = gy
      END IF
   END IF
   IF plyr.locy > 20 THEN
      IF map(plyr.mapx, plyr.mapy + 1) = 0 THEN
         plyr.locy = 0: plyr.mapy = plyr.mapy + 1
         plyr.glby = (plyr.mapy * 20) + plyr.locy
      ELSE
         plyr.locy = ly: plyr.glby = gy
      END IF
   END IF
GOSUB SendRay
plyr.forward = false
RETURN

SendRay:    '********************SCANS ENTIRE FIELD OF VISION**********
PCOPY backgroundpage, activepage    'start with background
  
   vx = 0   'start at left edge of screen when drawing ray screen
PCOPY 4, raypage  'erase ray page
IF mode = viewmode THEN SCREEN , , raypage, raypage
  
   ' set initial ray degree (30 to the left of player angle)
   IF plyr.angle > 29 THEN ray.angle = plyr.angle - 30 ELSE ray.angle = 330 + plyr.angle
   LOCATE 10, 2: PRINT ray.angle': SLEEP
   FOR raydeg = 1 TO 60    'will end up at 30 degrees to right of player
      ray.mapx = plyr.mapx: ray.mapy = plyr.mapy   'make the same as player
      ray.locx = plyr.locx: ray.locy = plyr.locy
      ray.glbx = plyr.glbx: ray.glby = plyr.glby
      gotwall = false         'assume no wall
  
      FOR n = 1 TO 20   'loop for distance of ray till a hit
         lx = ray.locx: ly = ray.locy
         gx = ray.glbx: gy = ray.glby
        
         ray.glbx = ray.glbx + 10 * (ccos(ray.angle))    'over 10 seems
         ray.glby = ray.glby - 10 * (csin(ray.angle))    'too high
  
         ray.locx = ray.locx + 10 * (ccos(ray.angle))
         ray.locy = ray.locy - 10 * (csin(ray.angle))
     
      'check if moving to another sector and make sure there is no wall
      'also will change coors to fixed numbers for alignment
 
   IF ray.locx < 0 THEN      'leaving sector to the left
      IF map(ray.mapx - 1, ray.mapy) = 0 THEN    'if next sector is clear
         ray.locx = 20: ray.mapx = ray.mapx - 1
         ray.glbx = (ray.mapx * 20) + ray.locx  'recaclulate global
      ELSE
         ray.locx = 20: ray.mapx = ray.mapx - 1
         ray.glbx = (ray.mapx * 20) + ray.locx  'recaclulate global
         clr = map(ray.mapx, ray.mapy)
        
a = (ray.glbx - plyr.glbx)    'use c2=a2+b2 to find distance
B = a * a
C = (ray.glby - plyr.glby)
d = C * C
e = B + d
        
         gotwall = true: raydist = CINT(SQR(e)): EXIT FOR   'hit a wall!
      END IF
   END IF
   IF ray.locx > 20 THEN
      IF map(ray.mapx + 1, ray.mapy) = 0 THEN
         ray.locx = 0: ray.mapx = ray.mapx + 1
         ray.glbx = (ray.mapx * 20) + ray.locx
      ELSE
         ray.locx = 0: ray.mapx = ray.mapx + 1
         ray.glbx = (ray.mapx * 20) + ray.locx
         clr = map(ray.mapx, ray.mapy)

a = (ray.glbx - plyr.glbx)
B = a * a
C = (ray.glby - plyr.glby)
d = C * C
e = B + d
'PRINT SQR(e)
        
         gotwall = true: raydist = CINT(SQR(e)): EXIT FOR
      END IF
   END IF
   IF ray.locy < 0 THEN
      IF map(ray.mapx, ray.mapy - 1) = 0 THEN
         ray.locy = 20: ray.mapy = ray.mapy - 1
         ray.glby = (ray.mapy * 20) + ray.locy
      ELSE
         ray.locy = 20: ray.mapy = ray.mapy - 1
         ray.glby = (ray.mapy * 20) + ray.locy
         clr = map(ray.mapx, ray.mapy)
        
a = (ray.glbx - plyr.glbx)
B = a * a
C = (ray.glby - plyr.glby)
d = C * C
e = B + d
        
         gotwall = true: raydist = CINT(SQR(e)): EXIT FOR
      END IF
   END IF
   IF ray.locy > 20 THEN
      IF map(ray.mapx, ray.mapy + 1) = 0 THEN
         ray.locy = 0: ray.mapy = ray.mapy + 1
         ray.glby = (ray.mapy * 20) + ray.locy
      ELSE
         ray.locy = 0: ray.mapy = ray.mapy + 1
         ray.glby = (ray.mapy * 20) + ray.locy
         clr = map(ray.mapx, ray.mapy)
     
a = (ray.glbx - plyr.glbx)
B = a * a
C = (ray.glby - plyr.glby)
d = C * C
e = B + d
     
      gotwall = true: raydist = CINT(SQR(e)): EXIT FOR
      END IF
   END IF
     
         'IF INT(RND * 2) = 1 THEN
    IF mode = mapmode THEN PSET (ray.glbx, ray.glby)
  
   NEXT
   IF mode = viewmode THEN GOSUB DrawScreen
   IF ray.angle <> 0 THEN ray.angle = ray.angle + 1 ELSE ray.angle = 0 + 1
   IF ray.angle = 360 THEN ray.angle = 0
   LOCATE 8, 2: PRINT ray.angle
   IF INKEY$ = CHR$(27) THEN done = true
NEXT
RETURN

DrawScreen:
   'draw to ray page
   LINE (vx, 100 - (2 * wallsize(raydist)))-(vx + 2, 100 + (2 * wallsize(raydist))), clr, BF
   vx = vx + 5
   'SCREEN 7, activepage, visualpage
   
RETURN

RotatePlayer:
  SELECT CASE plyr.rotate    'works for rotation speeds of 1, 3, 6 and 9
      CASE left
         IF plyr.angle <> 0 THEN plyr.angle = plyr.angle - rotatespeed ELSE plyr.angle = 360 - rotatespeed
         GOSUB SendRay
         plyr.rotate = still
      CASE right
        IF plyr.angle <> 360 THEN plyr.angle = plyr.angle + rotatespeed ELSE plyr.angle = 0 + rotatespeed
        IF plyr.angle = 360 THEN plyr.angle = 0
        GOSUB SendRay
         plyr.rotate = still
   END SELECT
  
   plyr.arrx = plyr.glbx + 6 * (ccos(plyr.angle))     'stupid line coming out of
   plyr.arry = plyr.glby - 6 * (csin(plyr.angle))     'to show angle
RETURN

DrawPlayer:
   'PSET (plyr.locx, plyr.locy), 4
   CIRCLE (plyr.glbx, plyr.glby), 2, 4
   LINE (plyr.glbx, plyr.glby)-(plyr.arrx, plyr.arry), 4
RETURN

ErasePlayer:
   CIRCLE (plyr.locx, plyr.locy), 2, 0
   LINE (plyr.locx, plyr.locy)-(plyr.arrx, plyr.arry), 0
RETURN

GetKey:
   keyp$ = INKEY$
   SELECT CASE keyp$
      CASE "8"
         plyr.forward = true
      CASE "2"
         plyr.forward = false
      CASE "4"
         plyr.rotate = left
      CASE "6"
         plyr.rotate = right
      CASE "5"
         plyr.rotate = still
      CASE CHR$(9)
         IF mode = mapmode THEN
            mode = viewmode
            toggle = 1
         GOSUB SendRay
         END IF
         IF toggle = 0 THEN
            IF mode = viewmode THEN
            mode = mapmode: GOSUB SendRay
            END IF
         END IF
         toggle = 0
      CASE CHR$(27)
         done = true
   END SELECT
RETURN

ReadMapData:
   mapdatax = 0: mapdatay = 0
   FOR n = 1 TO 80
      READ map(mapdatax, mapdatay)
      mapdatax = mapdatax + 1
      IF mapdatax = 10 THEN
         mapdatax = 0: mapdatay = mapdatay + 1': PRINT mapdatay: SLEEP
      END IF
   NEXT
RETURN

DrawMap:
   screenx = 0: screeny = 0   'local coor for VGA
   FOR n = 1 TO 11            'add 1 to close off grid
      LINE (screenx, 0)-(screenx, 159), 8
      screenx = screenx + 20
   NEXT
   FOR n = 1 TO 9             'add 1 to close off grid
      LINE (0, screeny)-(199, screeny), 8
      screeny = screeny + 20
   NEXT
RETURN

DrawWalls:
   screenx = 0: screeny = 0   'local coor for VGA
   mapx = 0: mapy = 0         'x and y of map array
   FOR n = 1 TO 80
      IF map(mapx, mapy) <> 0 THEN LINE (screenx, screeny)-(screenx + 20, screeny + 20), map(mapx, mapy), BF
      mapx = mapx + 1
      screenx = screenx + 20
      IF mapx = 10 THEN
         mapx = 0: mapy = mapy + 1
         screenx = 0: screeny = screeny + 20
      END IF
   NEXT
RETURN

HeightTable:
   size = 30: inc = 10        'set up wall height table
   altsize = 200
   FOR n = 0 TO 200
      wallsize(n) = size
      inc = inc - 1
      IF inc = 0 THEN
         inc = 10
         size = size - 2
      END IF
NEXT
RETURN

AltHeightTable:
   size = 50: inc = 4        'set up wall height table
   FOR n = 0 TO 200
      wallsize(n) = size
      inc = inc - 1
      IF inc = 0 THEN
         inc = 4
         size = size - 2
      END IF
   NEXT
RETURN

PrintOut:
   LOCATE 1, 27: PRINT "mapx"
   LOCATE 1, 32: PRINT plyr.mapx
   LOCATE 2, 27: PRINT "mapy"
   LOCATE 2, 32: PRINT plyr.mapy
   LOCATE 3, 27: PRINT "locx"
   LOCATE 3, 32: PRINT CINT(plyr.locx)
   LOCATE 4, 27: PRINT "locy"
   LOCATE 4, 32: PRINT CINT(plyr.locy)
   LOCATE 5, 27: PRINT "glbx"
   LOCATE 5, 32: PRINT CINT(plyr.glbx)
   LOCATE 6, 27: PRINT "glby"
   LOCATE 6, 32: PRINT CINT(plyr.glby)
   LOCATE 7, 27: PRINT "angle"
   LOCATE 7, 32: PRINT plyr.angle
   LOCATE 8, 27: PRINT "raydist"
   LOCATE 8, 32: PRINT raydist
RETURN
'***********************END SUBROUTINES**********************

'***********************DATA**********************
'map data - 1 for wall and 0 for no wall
DATA 1,1,1,1,1,1,1,1,6,1
DATA 1,0,0,0,0,0,0,0,0,1
DATA 7,0,0,0,0,4,7,0,0,3
DATA 1,0,0,0,0,0,0,0,0,1
DATA 1,0,0,0,0,0,0,0,0,1
DATA 4,0,0,0,0,0,0,0,0,1
DATA 8,0,0,0,0,0,0,0,0,1
DATA 1,7,1,1,1,1,1,1,1,1
'***********************END DATA**********************

'scale = (COS(ray) * 15000) / (1E-10 + distance)

