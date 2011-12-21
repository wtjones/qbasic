'                 Linked-list example by William T Jones
'                    
'-- Started July 28, 1998
'-- Updated September 16, 1998
'-- Tinkered with it December 10, 2000 since it never did work right.
'-- Still December 10, but a few minutes later....
'     Well, actually after all this time it did work right, I just  forgot to
'     call AddStar after removing them from the bottom of the screen.
'     I feel stupid.

DEFINT A-Z
CONST NULL = 0
CONST BUFFERSIZE = 50

TYPE nodetype
   x AS INTEGER
   y AS INTEGER
   velocity  AS INTEGER
   nextNode AS INTEGER
END TYPE

DECLARE SUB AddStar (x AS INTEGER, y AS INTEGER, velocity AS INTEGER)
DECLARE SUB DeleteNextStar (star AS INTEGER)
DECLARE SUB DeleteHeadStar ()

DIM SHARED listBuffer(1 TO BUFFERSIZE) AS nodetype
DIM SHARED vacantList AS INTEGER
DIM SHARED starList AS INTEGER
DIM SHARED current AS INTEGER

RANDOMIZE TIMER
starList = NULL   'list is empty

'------------------------
'Build vacant list for available array indices.
vacantList = 1
current = 1
FOR current = 1 TO BUFFERSIZE
   listBuffer(current).nextNode = current + 1
NEXT
'the last one points to NULL
   listBuffer(BUFFERSIZE).nextNode = NULL
'vacant list is now built, all allocation for lists is taken from this list
'------------------------

FOR I = 1 TO BUFFERSIZE - 20
   CALL AddStar(INT(RND * 320), INT(RND * 200), INT(RND * 3 + 1))
NEXT I

SCREEN 13

'---------------MAIN LOOP--------------
WHILE INKEY$ = ""
   'draw stars
   current = starList
   WHILE current <> NULL
      PSET (listBuffer(current).x, listBuffer(current).y), 15
      current = listBuffer(current).nextNode
      'LOCATE 1, 1: PRINT current
   WEND
   WAIT &H3DA, 8     'Wait for retrace

   'erase stars
   current = starList
   WHILE current <> NULL
      PSET (listBuffer(current).x, listBuffer(current).y), 0
      current = listBuffer(current).nextNode
   WEND

   'move stars based on velocity
   current = starList
   WHILE current <> NULL
      listBuffer(current).y = listBuffer(current).y + listBuffer(current).velocity
      IF listBuffer(current).y > 199 THEN
         'listBuffer(current).y = 0 'This is strange... If you uncomment this
                                    'line and comment the next 7, the gfx
                                    'are much smoother.
         IF current <> starList THEN
            DeleteNextStar (last)
            CALL AddStar(INT(RND * 320), 0, INT(RND * 3 + 1))
         ELSE
            DeleteHeadStar
            CALL AddStar(INT(RND * 320), 0, INT(RND * 3 + 1))
         END IF
      END IF
      last = current
      current = listBuffer(current).nextNode
   WEND
WEND
'----------------END MAIN LOOP------------------------

ERASE listBuffer
END

DEFSNG A-Z
SUB AddStar (x AS INTEGER, y AS INTEGER, velocity AS INTEGER)

'------------------------------------------------------------------------
'  This SUB takes a node from the vacant list and add to star list and
'  initilizes it.
'------------------------------------------------------------------------

DIM newNode AS INTEGER

IF vacantList <> NULL THEN
   newNode = vacantList    'take the head node
   vacantList = listBuffer(vacantList).nextNode 'the next vacant is now head
ELSE
   PRINT "Out of list space!": END
END IF

IF starList = NULL THEN 'list is empty
   starList = newNode
   listBuffer(starList).nextNode = NULL
ELSE
   listBuffer(newNode).nextNode = starList
   starList = newNode
END IF
'PRINT starList: SLEEP 1
listBuffer(starList).x = x
listBuffer(starList).y = y
listBuffer(starList).velocity = velocity

END SUB

SUB DeleteHeadStar

'------------------------------------------------------------------------
'  This SUB gives head star back to vacant list.
'------------------------------------------------------------------------

DIM deadStar AS INTEGER

deadStar = starList
starList = listBuffer(starList).nextNode
listBuffer(deadStar).nextNode = vacantList
vacantList = deadStar

END SUB

SUB DeleteNextStar (star AS INTEGER)

'---------------------------------------------------------------------------
'  This SUB removes next star from starList and returns it to vacantList.
'---------------------------------------------------------------------------

deadStar = listBuffer(star).nextNode   'star to be removed

'unlink dead star
listBuffer(star).nextNode = listBuffer(deadStar).nextNode

nextVacant = vacantList    'save current head
vacantList = deadStar          'deleted star is now head
listBuffer(vacantList).nextNode = nextVacant 'link new head to old head

END SUB

