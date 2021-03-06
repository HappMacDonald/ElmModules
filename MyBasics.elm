module MyBasics exposing
  ( inf
  , epsilon
  , factorial
  , isEven
  , padListLeft
  , incrementIf
  , decrementIf
  , curryRight
  , count
  , (^^)
  , intExponent
  , summationUnbounded
  , summationBounded
  , maybeAndThen2
  )

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
    total - 1
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


{-| Positive Integer Exponentiation
n<0 OR n == x == 0 return "Nothing"
Everything else returns a Just <integer> result. 👍
(This is just the not-infix variant of (^^) )

    intExponent 3 3 == Just 27 -- but no floats anywhere in sight ;)
    intExponent 0 15 == Just 0
    intExponent 15 0 == Just 1
    intExponent 0 0 == Nothing
    intExponent 3 -2 == Nothing
-}

intExponent : Int -> Int -> Maybe Int
intExponent = (^^)


{-| Calculates approximate 
Presumes that | generator(n) - generator(n-1) | shrinks reliably as n grows
instead of wildly growing larger and smaller.
Namely, it will bottom out the calculation once that delta shrinks
sufficiently small.

* start: where n starts growing from, usually you'll feed 0 or 1 here,
but you can choose any number including negatives.
Bear in mind that this function WILL count upwards towards +∞
regardless what sign `start` is.

* accumulator: the running sum so far; usually you'll feed 0 in here.

* generator: takes an int, emits a float. This is the function
that you're summing over.

    summationUnbounded start accumulator generator == answer
    summationUnbounded 0 0 (\n -> 2 ^ -n) ≈ 2
-}
summationUnbounded : Int -> Float -> (Int -> Float) -> Float
summationUnbounded start accumulator generator =
  let
--    _ = Debug.log "so far" accumulator
    margin = {-Debug.log ("Margin for step "++ (toString start)) -}(generator start)
  in
    if (abs margin)<epsilon then accumulator
    else
      summationUnbounded (start + 1) (accumulator + margin) generator


{-| Calculates exact Σ[n = start -> end]generator(n)Σ[n = start -> ∞]generator(n)
given that start <= end.
In case start > end, always returns zero.

    summationBounded start end generator == answer
    summationBounded 10 20 (\n -> n) == 446
-}

summationBounded : Int -> Int -> (Int -> Float) -> Float
summationBounded start end generator =
  if start>end then 0
  else
    (generator start) + (summationBounded (start + 1) end generator)


{-|Accept two Maybe-laden arguments into a callback function
that expects 2 naked arguments, but will in fact return a maybe result.

If either maybeAndThen2 argument is Nothing then result will be Nothing.
If both are Just then those Justs get unwrapped, and the bare values
inside get fed into the callback function to arrive at the final result

.. which might still be a Nothing if that's what callback gives
us after all. ;)

    --simple example callback:
    safeIntDivide : Int -> Int -> Maybe Int
    safeIntDivide a b =
      if b == 0 then
        Nothing
      else
        Just ( a//b )

    maybeAndThen2 safeIntDivide (Just 10) (Just 2) == Just 5
    maybeAndThen2 safeIntDivide (Just 10) Nothing == Nothing
    maybeAndThen2 safeIntDivide Nothing (Just 2) == Nothing
    maybeAndThen2 safeIntDivide Nothing Nothing == Nothing
    maybeAndThen2 safeIntDivide (Just 10) (Just 0) == Nothing
-}

maybeAndThen2 : (a -> b -> Maybe c) -> Maybe a -> Maybe b -> Maybe c
maybeAndThen2 callback maybe1 maybe2 =
  case maybe1 of
    Just arg1 ->
      Maybe.andThen (callback arg1) maybe2

    Nothing ->
      Nothing

-- andThen2 f a b =
  -- Maybe.map2 f a b |> Maybe.andThen identity