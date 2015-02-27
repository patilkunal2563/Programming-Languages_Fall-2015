--Question 1
-- Program
sublist [] = [[]]
sublist (x:xs) = sublist xs ++ map (x:) (sublist xs)

--Question 2
replic = replicFunction.map return where
replicFunction [] = []
replicFunction (x:xs) = x : replicFunction (map (\(x:xs) -> x:x:xs) xs)

--Question 3
-- Program
laste = head.reverse

--Question 6
-- Program
data Formula
        = Atom Bool		
        | And Formula Formula	
        | Or Formula Formula	
        | Not Formula	
	
-- Show Formula to display results

instance Show Formula where
 show (Atom atomA) = "Atom " ++ show atomA
 show (And atomA atomB) = "And (" ++ show atomA ++ ") (" ++ show atomB ++ ")"
 show (Or atomA atomB) = "Or (" ++ show atomA ++ ") (" ++ show atomB ++ ")"
 show (Not atomA) = "Not (" ++ show atomA ++ ")"

--Question 6.1
-- Program
collect_atoms (Atom atomA) = [Atom atomA]
collect_atoms (And atomA atomB) = collect_atoms atomA ++ collect_atoms atomB
collect_atoms (Or atomA atomB) = collect_atoms atomA ++ collect_atoms atomB
collect_atoms (Not atomA) = collect_atoms atomA

--Question 6.2
-- Program
eval (Atom atomA) = atomA
eval (And atomA atomB) = eval atomA && eval atomB
eval (Or atomA atomB) = eval atomA || eval atomB
eval (Not atomA) = not (eval atomA)