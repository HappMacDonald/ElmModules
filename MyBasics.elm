module MyBasics exposing (..)

-- Might not keep...
inf : Float
inf = 1/0


epsilon : Float
epsilon = 2 ^ -51


factorial : Int -> Int
factorial n =
  if n<1 then 1
  else n * factorial (n-1)


isEven : Int -> Bool
isEven x = x % 2 == 0


{-| concatenates filler onto beginning of list until it is greater than or equal to length.

    padListLeft 5 [0] [1] == [0,0,0,0,1]
    padListLeft 5 [0] [1,2,3,4,5,6] == [1,2,3,4,5,6]
    padListLeft 5 [0,0,0] [1] == [0,0,0,0,0,0,1]
-}

padListLeft : Int -> List a -> List a -> List a
padListLeft length filler list =
  if List.length list >= length then
    list
  else
    padListLeft length filler (filler ++ list)


{-| If the test succeeds, return integer incremented. Otherwise, just return integer.

    goodStatus = Ready
    incrementIf (goodStatus == Ready) 7 == 8

    badStatus = Failed
    incrementIf (badStatus == Ready) 7 == 7
-}

incrementIf : Bool -> Int -> Int
incrementIf test total =
  if test then
    total + 1
  else
    total


{-| If the test succeeds, return integer decremented. Otherwise, just return integer.

    goodStatus = Ready
    decrementIf (goodStatus == Ready) 7 == 6

    badStatus = Failed
    decrementIf (badStatus == Ready) 7 == 7
-}

decrementIf : Bool -> Int -> Int
decrementIf test total =
  if test then
    total + 1
  else
    total


{-| curry a function and second argument together, instead of first argument.
Trade secret: this is just flip renamed :J

    test = curryRight (<) 2
    test 3 == False
    test 1 == True
-}

curryRight : (a -> b -> c) -> b -> a -> c
curryRight = flip 


{-| Count how many things in a list evaluate to "true" given a test function.

    count <testFunction> <list> == <count>
    count isEven [1,2,3,4,5] == 2
-}

count : (a -> Bool) -> List a -> Int
count test things =
  List.foldl (\element accumulator -> incrementIf (test element) accumulator) 0 things


{-| Positive Integer Exponentiation
n<0 OR n == x == 0 return "Nothing"
Everything else returns a Just <integer> result. 👍

    3 ^^ 3 == Just 27 -- but no floats anywhere in sight ;)
    0 ^^ 15 == Just 0
    15 ^^ 0 == Just 1
    0 ^^ 0 == Nothing
    3 ^^ -2 == Nothing
-}

(^^) : Int -> Int -> Maybe Int
(^^) x n =
  if (n < 1) then
    if x==0 && n == 0 then
      Nothing
    else if n == 0 then
      Just 1
    else
      Nothing
  else if (n==1) then Just x
  else if (isEven n) then
    (x*x) ^^ (n//2)
  else
    Just <| x * (Maybe.withDefault 1 <| (x*x) ^^ ((n-1)//2))


-- Presumes monotonic margin
summationUnbounded : (Int -> Float) -> Int -> Float -> Float
summationUnbounded generator start accumulator =
  let
    nonsense = Debug.log "so far" accumulator
    margin = Debug.log ("Margin for step "++ (toString start)) (generator start)
  in
    if (abs margin)<epsilon then accumulator
    else
      summationUnbounded generator (start + 1) (accumulator + margin)


summationBounded : Int -> Int -> (Int -> Float) -> Float
summationBounded start end generator =
  if start>end then 0
  else
    (generator start) + (summationBounded (start + 1) end generator)
