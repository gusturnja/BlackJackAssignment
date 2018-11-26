--A0
--size hand2
-- = size Add (Card (Numeric 2) Hearts) (Add (Card Jack Spades) Empty)
-- = 1 + size (Add (Card Jack Spades) Empty)
-- = 1 + 1 + size Empty
-- = 1 + 1 + 0
-- = 2 + 0
-- = 2

module BlackJack where
import Cards
import RunGame
import System.Random

--A1
-- | Returns and empty hand
empty :: Hand
empty = Empty

--A2
-- | Calculates the lowest possible value of a hand decided on the value of the ace card
value :: Hand -> Integer
value hand = if iniValue > 21 then finalValue else iniValue
  where iniValue   = valueWithValueOfAces 11 hand
        finalValue = valueWithValueOfAces 1 hand

-- | Calculates the value of a hand using a specified value for ace cards
valueWithValueOfAces :: Integer -> Hand -> Integer
valueWithValueOfAces n Empty                = 0
valueWithValueOfAces n (Add (Card Ace _) x) = n + valueWithValueOfAces n x
valueWithValueOfAces n (Add card x)         = valueCard card + valueWithValueOfAces n x

-- | Returns the value of any card. Ace is valued at 11 by default
valueRank :: Rank -> Integer
valueRank (Numeric y) = y
valueRank Jack        = 10
valueRank Queen       = 10
valueRank King        = 10
valueRank Ace         = 11

-- | Returns the value of a card
valueCard :: Card -> Integer
valueCard (Card rank _) = valueRank rank

--A3
-- | Determines whether the player is bust
gameOver :: Hand -> Bool
gameOver hand = value hand > 21

--A4
-- | Decides a winner based on two players hand's value and the type of player
winner :: Hand -> Hand -> Player
winner handP handB
  | vP == vB                         = Bank
  | vP <= 21 && (vP > vB || vB > 21) = Guest
  | vB <= 21 && (vB > vP || vP > 21) = Bank
  | vB > 21 && vP > 21               = Bank
  where vP = value handP
        vB = value handB

--B1
-- | Add a second hand to the end of the first hand
(<+) :: Hand -> Hand -> Hand
(<+) Empty hand2            = hand2
(<+) (Add card x ) hand2    = Add card ((<+) x hand2)

prop_onTopOf_assoc :: Hand -> Hand -> Hand -> Bool
prop_onTopOf_assoc p1 p2 p3 =
  p1<+(p2<+p3) == (p1<+p2)<+p3

prop_size_onTopOf :: Hand -> Hand -> Bool
prop_size_onTopOf hand1 hand2 = size hand1 + size hand2 == size (hand1 <+ hand2)

--B2
-- | Generates a full deck of all 52 possible cards
fullDeck :: Hand
fullDeck = fullSuit Hearts <+ fullSuit Spades <+ fullSuit Diamonds <+ fullSuit Clubs

-- | Generates all 13 cards belonging to a specified suit
fullSuit :: Suit -> Hand
fullSuit suit = fullNonNumericSuit suit <+ numericSuit 2 10 suit

-- | Generates a hand of all cards with a non-numeric rank of a specified suit
fullNonNumericSuit :: Suit -> Hand
fullNonNumericSuit suit = Add (Card King suit) (Add (Card Queen suit) (Add (Card Jack suit) (Add (Card Ace suit) Empty)))

-- | Generates a hand of numeric cards between specified boundaries of a specified suit
numericSuit :: Integer -> Integer -> Suit -> Hand
numericSuit x y suit
  | x > y         = Empty
  | otherwise     = Add (Card (Numeric x) suit) (numericSuit (x+1) y suit)

--B3
-- | Draw the top card off of a deck
draw :: Hand -> Hand -> (Hand,Hand)
draw Empty hand           = error "draw : The deck is empty"
draw (Add (card) x) Empty = (x, Add card Empty)
draw (Add (card) x) hand  = (x, hand <+ Add card Empty)

--B4
-- | Play as the bank player, starting with an empty hand
playBank :: Hand -> Hand
playBank deck = drawBank deck Empty

-- | Draw cards from a specified deck until the value of the hand is at least 16
drawBank :: Hand -> Hand -> Hand
drawBank deck bankHand
  | value bankHand' >= 16 = bankHand'
  | otherwise             = drawBank deck' bankHand'
  where (deck',bankHand') = draw deck bankHand

--B5
-- | Shuffles a given deck
shuffle :: StdGen -> Hand -> Hand
shuffle g deck = shuffle' g deck Empty

-- | Helper function to shuffle
shuffle' :: StdGen -> Hand -> Hand -> Hand
shuffle' g Empty newDeck = newDeck
shuffle' g deck newDeck  = shuffle' g1 (handleCard n1 True deck) (newDeck <+ (handleCard n1 False deck))
  where (n1,g1) = randomR (0,(size deck) -1) g

---- | Removes a card from a deck at a specified location
--removeCard :: Integer -> Integer -> Hand -> Hand
--removeCard _ _ Empty  = Empty
--removeCard index removeAt (Add card x)
--  | index == removeAt = x
--  | otherwise  = Add card (removeCard (index+1) removeAt x)

---- | Returns a card from a deck at a specified location
--returnCard :: Integer -> Integer -> Hand -> Hand
--returnCard _ _ Empty  = Empty
--returnCard index location (Add card x)
--  | index == location = Add card Empty
--  | otherwise  = returnCard (index+1) location x

-- | Combine removeCard and returnCard
handleCard :: Integer -> Bool -> Hand -> Hand
handleCard _ _ Empty = Empty
handleCard index remove (Add card hand) | index == 0 && remove = hand
                                        | index == 0           = Add card Empty
                                        | remove               = Add card (handleCard (index-1) remove hand)
                                        | otherwise            = handleCard (index-1) remove hand



-- | Return either an added or a removed card along with the updated deck
handleCard :: Integer -> Bool -> Hand -> (Hand, Card)
handleCard _ _ Empty = error "Error: No hand given"
handleCard index remove (Add card hand)
  | index == 0 && remove = (hand, card)
  | index == 0           = (Add card hand, card)
  | otherwise            = (removeCard' (index-1) hand, getCardAt' (index-1) hand)

-- | (Help function) Removes a card from a deck
removeCard' :: Integer -> Hand -> Hand
removeCard' index (Add card hand)
  | index == 0 = hand
  | otherwise  = Add card (removeCard' (index-1) hand)

-- | (Help function) Returns card at an index in a deck
getCardAt' :: Integer -> Hand -> Card
getCardAt' index (Add card hand)
  | index == 0 = card
  | otherwise  = getCardAt' (index-1) hand


-- | Checks whether a card is within a hand before and after a shuffle
prop_shuffle_sameCards :: StdGen -> Card -> Hand -> Bool
prop_shuffle_sameCards g c h =
    c `belongsTo` h == c `belongsTo` shuffle g h

-- | Checks as to whether a card is within a hand
belongsTo :: Card -> Hand -> Bool
c `belongsTo` Empty = False
c `belongsTo` (Add c' h) = c == c' || c `belongsTo` h

-- | Makes sure the size is preserved
prop_size_shuffle :: StdGen -> Hand -> Bool
prop_size_shuffle g h = size h == size (shuffle g h)


--B6
--Sets up the interface
implementation :: Interface
implementation = Interface
  { iEmpty    = empty
  , iFullDeck = fullDeck
  , iValue    = value
  , iGameOver = gameOver
  , iWinner   = winner
  , iDraw     = draw
  , iPlayBank = playBank
  , iShuffle  = shuffle
  }

main :: IO ()
main = runGame implementation
