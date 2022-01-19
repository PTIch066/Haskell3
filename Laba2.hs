--Лабораторна робота №2
--студента групи КН-31
--підгрупа 2
--Ясинського Дениса
--Варіант №11

-- Мета: Набути досвiду визначення рекурсивних функцiй, використання механiзму
-- зiставлення зi зразком i роботи з кортежами та списками.

-- Завдання: Визначте вказанi функцiї в кожному з завдань: а) без застосування, б) з
-- застосуванням вбудованих функцiй.
-- Завдання 1.11: Повторити n-кратно кожен елемент списку, напр. при n=2: "asd"⇒[’a’, ’a’, ’s’, ’s’, ’d’, ’d’].

-- Завдання 2.11: Обчислити функцiю Ейлера φ(m).


-- Завдання 1.11 a:
f1 :: (Foldable t1, Eq t2, Num t2) => t2 -> t1 a -> [[a]]
f1 n lst = map (:[]) (concatMap (\x -> pov n x) lst)
pov n x = mypov n x []
mypov 0 x l = l
mypov n x l = l++x : mypov (n-1) x l

-- Завдання 1.11 б:
f2 n lst = concatMap (\x -> replicate n x) lst

-- Завдання 2.11 a:
prime :: Int -> Int -> Bool
prime 0 y = if y>1 then False 
                 else True 
prime x y = if x>y then prime (x-y) y
                 else prime (y-x) x

eiler1 n = len_arr (filter (prime n) [1..n]) 0
len_arr [] n = n
len_arr (x:xs) n = len_arr xs (n+1)

-- Завдання 2. б:
eiler2 n = length (filter (prime n) [1..n])      

--Тести:
--ghci> f1 2 "asd"
--["a","a","s","s","d","d"]
--ghci> f2 2 "asd"
--"aassdd"
--ghci> eiler1 5
--4
--ghci> eiler2 5
--4

-- Висновок: В результаті виконання лабораторної роботи, я навчився визначати рекурсивниї функцій,
-- та використовувати механізм зіставлення зі зразком та отримав досвід у праці з кортежами та списками.