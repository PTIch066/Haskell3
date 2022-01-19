--Лабораторна робота №5
--студентки групи КН-31 
--підгрупи 2
--Ясинського Дениса
--Варіант №11  

-- 1.11 Замiнити кожен n-й елемент списку вказаним значенням, напр. при
-- n=2 та значеннi ’z’: "1234590"⇒ "1z3z5z0".

f1 :: String  -> Char -> String
f1 [] ch = []
f1 (x:xs) ch
    | length xs > 1 = x : ch : f1 (tail xs) ch
    | otherwise = f1 [] ch

out :: IO()
out = do

-- 1) введення з клавіатури
    putStrLn "Input str:"
    str<-getLine
    putStrLn "Input char: "
    char<-getLine
    putStrLn(f1 str (head char))

--2) введення даних з файлу
    contents <- readFile "file.txt"

--3) виведення результатів на екран
    putStrLn "Input str:"
    str<-getLine
    putStrLn "Input char: "
    char<-getLine
    putStrLn(f1 str (head char))

--4) виведення результатів у файл
    writeFile "output.txt" (f1 str (head char))

--Висновок: Я ознайомився з модульною органiзацiєю програм та 
--засобами введення-виведення.