data AVL a
  = Empty
  | Node a Int (AVL a) (AVL a)
  deriving (Show, Eq, Ord)

leaf :: Ord a => a -> AVL a
leaf val = Node val 0 Empty Empty

height :: AVL a -> Int
height Empty = -1
height (Node _ h _ _) = h

balance :: AVL a -> Int
balance Empty = 0
balance (Node _ _ l r) = height l - height r

leftLeftRotation :: AVL a -> AVL a
leftLeftRotation (Node vA hA (Node vB hB (Node vC hC lC rC) rB) rA) =
  Node vB (max (height left) (height right) + 1) left right
  where
    left  = Node vC (height lC - height rC) lC rC
    right = Node vA (height rB - height rA) rB rA

rightRightRotation :: AVL a -> AVL a
rightRightRotation (Node vA hA lA (Node vB hB lB (Node vC hC lC rC))) =
  Node vB (max (height left) (height right) + 1) left right
  where
    left  = Node vA (max (height lA) (height lB) + 1) lA lB
    right = Node vC (max (height lC) (height rC) + 1) lC rC

leftRightRotation :: AVL a -> AVL a
leftRightRotation (Node vA hA (Node vB hB lB (Node vC hC lC rC)) rA) =
  Node vC (height left - height right) left right
  where
    left  = Node vB (height lB - height lC) lB lC
    right = Node vA (height rC - height rA) rC rA

rightLeftRotation :: AVL a -> AVL a
rightLeftRotation (Node vA hA lA (Node vB hB (Node vC hC lC rC) rB)) =
  Node vC (height left - height right) left right
  where
    left  = Node vA (height lA - height lC) lA lC
    right = Node vB (height rC - height rB) rC rB

fixNode :: AVL a -> AVL a
fixNode Empty = Empty
fixNode node@(Node _ _ left right)
  | balance node >= 2 =
    case balance left of
      1 -> leftLeftRotation node
      -1 -> leftRightRotation node
      _ -> error "b(l) not in {-1, 1}"
  | balance node <= -2 =
    case balance right of
      1 -> rightLeftRotation node
      -1 -> rightRightRotation node
      _ -> error "b(r) not in {-1, 1}"
  | otherwise = node

avlInsert :: Ord a => a -> AVL a -> AVL a
avlInsert v Empty = leaf v
avlInsert v (Node val h l r)
  | v < val =
    let left = avlInsert v l
     in fixNode (Node val (max (height left) (height r) + 1) left r)
  | otherwise =
    let right = avlInsert v r
     in fixNode (Node val (max (height right) (height l) + 1) l right)

getReplacingNode :: AVL a -> AVL a
getReplacingNode node@(Node _ _ Empty _) = node
getReplacingNode node@(Node _ _ l _) = getReplacingNode l

--avlDelete :: Ord a => a -> AVL a -> AVL a
--avlDelete v (Node val h l r)
  -- | v == val =
    --let node = getReplacingNode r
     --in case node of
          --Node da he Empty right ->
            --fixNode (Node da (max (height l) (height r)) l r)
          --_ -> error ""
  -- | otherwise = error ""
