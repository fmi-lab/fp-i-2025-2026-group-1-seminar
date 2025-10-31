# Седмица 05 - Алгебрични типове данни

## Задача 01 - Типа Maybe
Напишете своя версия на типа `Maybe` и за нея напишете следните функции:

- `isJust/isNothing` - проверява дали типа е `Just/Nothing`
- `fromJust` - приема стойност от тип `Maybe` и ако тя е `Just` връща стойността в нея
- `fromMaybe` - приема стойност по подразбиране и стойност от тип `Maybe`. Ако вторият аргумент е `Just`, то функцията връща стойността му, в противен случай връща стойността по подразбиране
- `maybe` - приема стойност по подразбиране, едноместна функция `f` и стойност от тип `Maybe`. Ако третият аргумент е `Just`, то функцията да прилага `f` върху стойността, в противен случай да върне стойността по подразбиране
- `catMaybes` - приема списък от стойности от тип `Maybe` и връща списък от всички `Just` стойности
- `mapMaybe` - приема функция `f` на един аргумент, която връща стойност от тип `Maybe` и списък. Функцията да прилага `f` над всички елементи и като резултат да върне само получените `Just` стойности
- `lookup` - търси стойност в асоциативен списък по подаден ключ. Ако ключът не бъде намерен, връща `Nothing`

За референция, вижте функциите от модула [Data.Maybe](https://hackage.haskell.org/package/base-4.21.0.0/docs/Data-Maybe.html)

### Примери:
```haskell
ghic> isJust $ Just 5 -- True
ghic> isJust Nothing -- False

ghci> fromMaybe 3 $ Just 5 -- 5
ghci> fromMaybe 3 Nothing -- 3

ghci> maybe 3 (^ 2) $ Just 5 -- 25
ghci> maybe 3 (^ 2) Nothing

ghci> catMaybes [Just 3, Nothing, Just 5, Just 6 ,Nothing] -- [3,5,6]
ghci> mapMaybe (\x -> if odd x then (Just x) else Nothing) [3,5,6,4] -- [3,5]

ghci> lookup 2 [(1, "first"), (2, "second"), (3, "third")] -- Just "second"
ghci> lookup 4 [(1, "first"), (2, "second"), (3, "third")] -- Nothing
```

## Задача 02 - Типа Either
> Either is what's right or whatever's left

Напишете своя версия на типа `Either` и за нея напишете следните функции:

- `isLeft/isRight` - проверява дали типа е `Left/Right`
- `fromLeft/fromRight` - приема стойност по подразбиране и стойност от тип `Either`. Ако вторият аргумент е `Left/Right`, то функцията връща стойността му, в противен случай връща стойността по подразбиране
- `either` - приема две едноместни функции `f` и `g` и стойност от тип `Either`. Ако третият аргумент е `Left`, то функцията да прилага `f` върху стойността иначе да прилага `g`
- `lefts/rights` - приема списък от стойностти от тип `Either` и връща списък от всички `Left/Right` стойности
- `partitionEithers` - приема списък от стойностти от тип `Either` и връща наредена двойка от списъци, съдържащи съответно всички `Left` и всички `Right` стойности

За референция, вижте функциите от модула [Data.Either](https://hackage.haskell.org/package/base-4.21.0.0/docs/Data-Either.html)

### Примери:
```haskell
ghci> isRight $ Right 5 -- True
ghci> isRight $ Left "hello" -- False

ghci> fromRight 3 $ Right 5 -- 5
ghci> fromRight 3 $ Left "hello"

ghci> either (++ " world") (("number: " ++) . show) $ Right 5 -- "number: 5"
ghci> either (++ " world") (("number: " ++) . show) $ Left "hello" -- "hello world"

ghci> rights [Left "foo", Right 3, Left "bar", Right 7, Left "baz"] -- [3,7]
ghci> partitionEithers [Left "foo", Right 3, Left "bar", Right 7, Left "baz"] -- (["foo","bar","baz"],[3,7])
```

## Задача 03 - Непразен списък
Напишете своя версия на типа `NonEmpty`, представляващ непразен списък. Използвайте (:|) за конструктор на типа. За него напишете следните функции:

- `fromList` - конвертира списък към непразен списък, ако е възможно. Използвайте типа `Maybe`
- `asList` - конвертира непразен списък към обикновен списък
- `mapNE`, `foldrNE`, `foldr1NE`, `maximumNE` - аналог на съответните функции за списъци, но за непразен списък
- `mean` и `median` - по подаден непразен списък връщат съответно средното и медианата на списъка
- `repeatNE` - приема елемент `x` и генерира безкраен непразен списък съставен от `x`
- `iterateNE` - приема функция на 1 аргумент `f` и елемент `x`. Функцията да генерира безкраен непразен списък от приложенията на `f` върху `x`
- `cycleNE` - приема непразен списък и генерира безкраен непразен списък съставен от повтаряне на елементите на подадения списък

### Примери:
```haskell
ghci> fromList [] -- Nothing
ghci> fromList [4,8,5,1,6,9] -- Just (4 :| [8,5,1,6,9])

ghci> asList $ fromJust $ fromList [4,8,5,1,6,9] -- [4,8,5,1,6,9]

ghci> mapNE (^ 2) $ fromJust $ fromList [4,8,5,1,6,9] -- 16 :| [64,25,1,36,81]
ghci> foldrNE (+) 0 $ fromJust $ fromList [4,8,5,1,6,9] -- 33
ghci> foldr1NE (+) $ fromJust $ fromList [4,8,5,1,6,9] -- 33
ghci> maximumNE $ fromJust $ fromList [4,8,5,1,6,9] -- 9

ghci> mean $ fromJust $ fromList [4,8,5,1,6,9] -- 5.5
ghci> median $ fromJust $ fromList [4,8,5,1,6,9,2] -- 5
ghci> median $ fromJust $ fromList [4,8,5,1,6,9,2,3] -- 4.5

ghci> take 10 $ asList $ repeatNE 5 -- [5,5,5,5,5,5,5,5,5,5]
ghci> take 10 $ asList $ iterateNE (+ 2) 1 -- [1,3,5,7,9,11,13,15,17,19]
ghci> take 10 $ asList $ cycleNE $ fromJust $ fromList [1,2,3] -- [1,2,3,1,2,3,1,2,3,1]
```

За референция, вижте функциите от модула [Data.List.NonEmpty](https://hackage.haskell.org/package/base-4.21.0.0/docs/Data-List-NonEmpty.html)

## Задача 04 - Естествено число
Напишете потребителски тип `Nat`, представляващ естествено число. Чрез него напишете следните функции:

- `plus` - събира 2 естествени числа
- `multiply` - умножава 2 естествени числа
- `fromInt` - конвертира цяло число към естествено, ако е възможно. Използвайте типа `Maybe`
- `toInt` - конвертира естествено число към цяло

### Примери:
```haskell
ghci> let five = Succ $ Succ $ Succ $ Succ $ Succ Zero
ghci> let three = Succ $ Succ $ Succ Zero

ghci> toInt $ plus five three -- 8
ghci> toInt $ muliply five three -- 15
ghci> toInt $ muliply Zero three -- 0

ghci> fromInt 3 -- Just (Succ (Succ (Succ Zero)))
ghci> fromInt (-4) -- Nothing
```

## Задача 05 - Верига
Напишете потребителски тип верига (`Chain`), която наподобява списък, но позволява константно време на операцията конкатенация на две вериги. За целта напишете 3 конструктора - `Empty` представляващ празна верига, `Singleton` представляващ верига, съдържаща само 1 елемент и `Append` представляващ верига, изградена от 2 други вериги. Напишете следните функции:

- `(<:)` и `(>:)` - приемат елемент и верига и добавят елемента съответно в началото и края на веригата
- `headChain` - връща първия елемент на верига, ако тя не е празна
- `tailChain` - приема верига и връща нова верига, която съдържа елементите от подадената в същия ред, но без първия. Не е задължително новата верига да има същата структура като подадената  
- `append` - конкатенира две вериги
- `mapChain` - аналог на `map` за списъци, но за верига
- `foldlChain` - аналог на `foldl` за списъци, но за верига
- `toList` - приема верига и връща списък от елементите на веригата в същия ред
- `listify` - приема верига и връща нова верига, съдържаща същите елементи като подадената, но с променена структура - тя трябва да наподобява тази на списъка и изискването е всеки ляв елемент на `Append` верига да бъде `Singleton` (ако веригата не е празна разбира се) 

### Пример:
```haskell
ghci> let chain = Append (Append (Singleton 1) (Singleton 2)) (Append (Append (Singleton 3) (Singleton 4)) (Singleton 5))

ghci> 2 <: chain -- Append (Singleton 2) (Append (Append (Singleton 1) (Singleton 2)) (Append (Append (Singleton 3) (Singleton 4)) (Singleton 5)))
ghci> 2 <: Empty -- Singleton 2
ghci> 2 <: (Singleton 3) -- Append (Singleton 2) (Singleton 3)

ghci> headChain Empty -- Nothing
ghci> headChain $ Singleton 4 -- Just 4
ghci> headChain chain -- Just 1

ghci> mapChain (* 2) chain -- Append (Append (Singleton 2) (Singleton 4)) (Append (Append (Singleton 6) (Singleton 8)) (Singleton 10))
ghci> foldlChain (-) 0 chain -- -15
ghci> toList chain -- [1,2,3,4,5]
ghci> listify chain -- Append (Singleton 1) (Append (Singleton 2) (Append (Singleton 3) (Append (Singleton 4) (Singleton 5))))
```

## Задача 06 - Валидация и създаване на студенти
Напишете запис (record) `Student`, който представлява студент в университет. Един студент съдържа факултетен номер, име, имейл и телефонен номер, който обаче не е задължителен.

Напишете функция `createStudent`, която приема необходимите данни на един студент, валидира ги и в случай на успех връща новосъздадения студент. Валидациите са следните:
- факултетният номер трябва да има дължина от точно 10 символа
- името трябва да се състои от 3 думи - първо, бащино и фамилно име
- имейлът трябва да съдържа един символ @ и поне един символ . след него
- телефонният номер трябва да започва с 0 и да има дължина от точно 10 символа

Ако някои от тези валидации не са успешни, функцията да връща непразен списък, съдържащ подходящи съобщения спрямо вида на грешките (свободни сте да изберете формата на съобщенията).

### Примери:
```haskell
ghci> createStudent "3MI0800145" "Ivan Petrov Ivanov" "ivan@example.com" (Just "0898123456") -- Right (Student {fn = "3MI0800145", name = "Ivan Petrov Ivanov", email = "ivan@example.com", phone = Just "0898123456"})

ghci> createStudent "3MI080014" "Ivan Petrov Ivanov" "ivan@examplecom" Nothing -- Left [InvalidFnError,InvalidEmailError]
```