module Tautology where


data Prop = Const Bool
          | Var Char
          | Not Prop
          | And Prop Prop
          | Imply Prop Prop


type Assoc k v = [(k, v)]

type Subst = Assoc Char Bool


find :: Eq k => k -> Assoc k v -> v
find k t = head [v | (k', v) <- t, k == k']

-- Function that remove duplicates from a list
removeDups :: Eq a => [a] -> [a]
removeDups [] = []
removeDups (x:xs) = x : filter (/= x) (removeDups xs)


-- A function to evaluate a proposition given a substitution for its variables
-- defined by pattern matching on the five possible forms that the proposition can
-- take.
eval :: Subst -> Prop -> Bool
-- The value of a constant is simply the constant itself.
eval _ (Const x)   = x
-- The value of a variable is obtained by looking up its value in the substitution set.
eval s (Var x)     = find x s
-- Negation of a propostion
eval s (Not p)     = not (eval s p)
-- The value of a conjunction is given by taking the conjunction of the values of
-- the two argument proposition.
eval s (And p q)   = (eval s p) && (eval s q)
-- Implication can be obtained by applying `<=` ordering on logical values.
eval s (Imply p q) = eval s p <= eval s q


-- A function that returns a list of all the variables in a proposition.
vars :: Prop -> [Char]
vars (Const _)   = []
vars (Var x)     = [x]
vars (Not p)     = vars p
vars (And p q)   = vars p ++ vars q
vars (Imply p q) = vars p ++ vars q


bools :: Int -> [[Bool]]
bools 0 = [[]]
bools n = l s ++ r s
  where
    l = map (False:)
    r = map (True:)
    s = bools (n - 1)

substs :: Prop -> [Subst]
substs p = map (zip vs) (bools (length vs))
  where
    vs = removeDups (vars p)


-- A function that decides if a proposition is a tautology.
isTaut :: Prop -> Bool
isTaut p = and [eval s p | s <- substs p]


-- Proposition variables
p1 :: Prop
p1 = And (Var 'A') (Not (Var 'A'))

p2 :: Prop
p2 = Imply (And (Var 'A') (Var 'B')) (Var 'A')

p3 :: Prop
p3 = Imply (Var 'A') (And (Var 'A') (Var 'B'))

p4 :: Prop
p4 = Imply (And (Var 'A') (Imply
        (Var 'A') (Var 'B'))) (Var 'B')
