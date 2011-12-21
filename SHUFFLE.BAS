'April 14, 1999
'Builds a deck of poker cars, shuffles them by swapping 2 at a time,
'then deals out 7 to display. Original aim was to figure probability of
'getting royal flush, etc....

DEFINT A-Z: CLS

CONST JACK = 11
CONST QUEEEN = 12
CONST KING = 13
CONST ACE = 14

CONST HEARTS = 1
CONST DIAMONDS = 2
CONST CLUBS = 3
CONST SPADES = 4

TYPE Card
   face AS INTEGER         '2 thru 14
   suit AS INTEGER
END TYPE

'build deck

DIM deck(1 TO 52) AS Card
DIM stack(1 TO 52) AS Card
dIndex = 1
FOR i = 1 TO 4    'each suit
   FOR j = 2 TO 14
      deck(dIndex).suit = i
      deck(dIndex).face = j
      dIndex = dIndex + 1
   NEXT j
NEXT i

'shuffle
RANDOMIZE TIMER

DIM tempCard AS Card
FOR i = 1 TO (52 * 52)
   swap1 = INT(RND * 52) + 1
   swap2 = INT(RND * 52) + 1
   WHILE swap2 = swap1
      swap2 = INT(RND * 52) + 1
   WEND
   tempCard.face = deck(swap2).face
   tempCard.suit = deck(swap2).suit
   deck(swap2).face = deck(swap1).face
   deck(swap2).suit = deck(swap1).suit
   deck(swap1).face = tempCard.face
   deck(swap1).suit = tempCard.suit

NEXT i

'deal

deckTop = 52
stackTop = 0   'empty

'deal 7 cards to stack


FOR i = 1 TO 7
   stackTop = stackTop + 1
   stack(stackTop).face = deck(deckTop).face
   stack(stackTop).suit = deck(deckTop).suit
   deckTop = deckTop - 1
NEXT i

FOR i = 1 TO stackTop
   PRINT stack(i).face, CHR$(stack(i).suit + 2)
NEXT

