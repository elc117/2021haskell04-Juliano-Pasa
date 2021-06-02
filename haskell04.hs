-- Prática 04 de Haskell
-- Nome: Juliano de Mello Pasa

-- Exercicio 1 
-- Classificacao de idoso

faixaIdoso :: Int -> String
faixaIdoso x 
  | x >= 80 = "IDO80"
  | x <= 79 && x >= 75 = "IDO79"
  | x <= 74 && x >= 70 = "IDO74"
  | x <= 69 && x >= 65 = "IDO69"
  | x <= 64 && x >= 60 = "IDO64"
  | otherwise = "ND"

-- Exercicio 2
-- Classificao de muitos idosos

classifIdosos :: [(String,Int)] -> [(String,Int,String)]
classifIdosos x = [(nome, idade, faixaIdoso idade) | (nome, idade) <- x] 

-- Exercicio 3
-- Classificao de muitos idosos funcao de alta ordem

classifIdosos' :: [(String,Int)] -> [(String,Int,String)]
classifIdosos' x = map (\(n, i) -> (n, i, faixaIdoso i)) x 

-- Exercicio 4
-- String de RGB

strColor :: (Int,Int,Int) -> String
strColor x = "rgb" ++ show x 

-- Exercicio 5
-- Gerador de circulos

genCircs :: Int -> (Int,Int) -> Int -> [(Int,Int,Int)]
genCircs n (x, y) r = [(x, y, r) | x <- take 5 (iterate (10+) x)]

-- Exercicio 6
-- Gerador de tons de vermelho

genReds :: Int -> [(Int,Int,Int)]
genReds n = [(x, 0, 0) | x <- take n [div 255 n,2*(div 255 n)..]]