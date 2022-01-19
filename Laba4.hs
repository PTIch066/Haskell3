--Лабораторна робота №4
--студента групи КН-31
--підгрупа 2
--Ясинський Денис
--варіант №11

-- Мета: Ознайомитись з системою типiв та класiв типiв. Набути досвiду визначення
-- нових типiв та класiв типiв i їх використання.

--11. пошуку iнформацiї за iменем

--Тести:
--ghci> getByName testData "Onuprienko A.A."
-- Just "098-000-23-55"

type People = String
type Phone = String 
type Info = String 
type Place = String 
type DayMonthYear = (Int, Int, Int)

data Notebook = Remind People DayMonthYear 
            |Contacts People Phone
            |Meeting DayMonthYear Place Info


getByName:: [Notebook]->People->Maybe Phone
getByName l p = getB l
    where   getB [] = Nothing
            getB (Contacts p' x:xs)
                |p'== p = Just x
                |otherwise = getB xs
            getB(_ : xs) = getB xs

testData :: [Notebook]
testData = [
    Remind "TOtomen R.D" (13,5,2021), 
    Remind "Makarov I.O" (7,10,2021), 
    Remind "Polishuk V.V." (4, 8,2021),
    Contacts "Onuprienko A.A." "098-000-23-55",
    Contacts "Cat V.I." "095-752-86-23",
    Contacts "Pint A.D." "095-322-95-03",
    Contacts "Dante A.D." "067-408-10-10",
    Meeting  (4, 8,2021) "Kyiv" "Zystrich Vidbylasya"
        ]

-- Висновок: Під час виконання лабораторної роботи, я ознайомився та 
-- імплементував класи типів мови Haskell. Також ознайомився з системою
-- типів та класів типів, визначив власні функції для нового типу. 