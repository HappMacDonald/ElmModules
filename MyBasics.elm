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


-- Output is only properly defined when n>0, or when n==0 if x!=0. For inputs n<0 or x == n == 0 the output is horseshit and the solution is "don't feed those in". :)
-- If it's good enough for the larger language (0/0, etc) then why should I dodge ignoring invalid input and outputting potentially misleading gibberish as output for it?
(^^) : Int -> Int -> Int
(^^) x n =
  if (n < 1) then 1
  else if (n==1) then x
  else if (isEven n) then
    x*x ^^ (n//2)
  else
    x * (x*x ^^ ((n-1)//2))


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
