module MyBasicsTest exposing (..)

import Expect exposing (Expectation)
import Test exposing (..)
import Fuzz

import MyBasics

myBasicsTests : Test
myBasicsTests =
  describe "MyBasics"
  [ describe "factorial"
    [ test "0! = 1"
      <|\_ ->
          MyBasics.factorial 5
          |>Expect.equal 120
    , test "1! = 1"
      <|\_ ->
          MyBasics.factorial 5
          |>Expect.equal 120
    , test "5! = 120"
      <|\_ ->
          MyBasics.factorial 5
          |>Expect.equal 120
    , test "-1! = ?"
      <|\_ ->
          MyBasics.factorial 5
          |>Expect.equal 120
    ]
  , describe "isEven"
    [ test "2 isEven"
      <|\_ ->
          MyBasics.isEven 2
          |>Expect.true "Pointless String"
    , test "not 1 isEven"
      <|\_ ->
          MyBasics.isEven 1
          |>Expect.false "Pointless String"
    , test "0 isEven"
      <|\_ ->
          MyBasics.isEven 0
          |>Expect.true "Pointless String"
    , test "not -1 isEven"
      <|\_ ->
          MyBasics.isEven -1
          |>Expect.false "Pointless String"
    , test "-1000 isEven"
      <|\_ ->
          MyBasics.isEven -1000
          |>Expect.true "Pointless String"
    ]
  , describe "padListLeft"
    [ test "3 [0] [1] -> [0,0,1]"
      <|\_ ->
          MyBasics.padListLeft 3 [0] [1]
          |>Expect.equal [0,0,1]
    , test "4 [0,0] [2] -> [0,0,0,2]"
      <|\_ ->
          MyBasics.padListLeft 4 [0,0] [2]
          |>Expect.equal [0,0,0,0,2]
    , test "1 [\"q\"] [\"Hello\", \"there\", \"world!\"] -> [\"Hello\", \"there\", \"world!\"]"
      <|\_ ->
          MyBasics.padListLeft 1 ["q"] ["Hello", "there", "world!"]
          |>Expect.equal ["Hello", "there", "world!"]
    , test "0 [] [] -> []"
      <|\_ ->
          MyBasics.padListLeft 0 [] []
          |>Expect.equal []
    , test "-100 [0] [4] -> [4]"
      <|\_ ->
          MyBasics.padListLeft -100 [0] [4]
          |>Expect.equal [4]
    ]
  , describe "incrementIf"
    [ test "incrementIf False 0 == 0"
        <|\_ ->
            MyBasics.incrementIf False 0
            |>Expect.equal 0
    , test "incrementIf True 1 == 2"
        <|\_ ->
            MyBasics.incrementIf True 1
            |>Expect.equal 2
    , test "incrementIf True -1 == 0"
        <|\_ ->
            MyBasics.incrementIf True -1
            |>Expect.equal 0
    ]
  , describe "decrementIf"
    [ test "decrementIf False 0 == 0"
        <|\_ ->
            MyBasics.decrementIf False 0
            |>Expect.equal 0
    , test "decrementIf True 2 == 1"
        <|\_ ->
            MyBasics.decrementIf True 2
            |>Expect.equal 1
    , test "decrementIf True -1 == -2"
        <|\_ ->
            MyBasics.decrementIf True -1
            |>Expect.equal -2
    ]
  , describe "curryRight -- lessThanThree = curryRight (<) 3" <|
    let
      lessThanThree =
        MyBasics.curryRight (<) 3

    in
    [ test "2 |> lessThanThree is true"
        <|\_ ->
            2 |> lessThanThree
            |>Expect.true "Pointless String"
    , test "4 |> lessThanThree is false"
        <|\_ ->
            4 |> lessThanThree
            |>Expect.false "Pointless String"
    ]
  , describe "count"
    [ fuzz (Fuzz.list Fuzz.int) "it's results should always match inputList |> List.filter test |> List.length"
      <|\input ->
          Expect.equal
            ( input
              |>List.filter MyBasics.isEven
              |>List.length
            )
            ( input
              |>MyBasics.count MyBasics.isEven
            )
    ]
  , describe "(^^)/intExponent"
    [ fuzz2
      ( Fuzz.intRange 0 100 )
      ( Fuzz.intRange 1 4 )
        "(a = 0-100)^^(b = 1-4) == Just <| round <| a^b"
      <|\a b ->
          Expect.equal
          ( MyBasics.intExponent a b )
          <|Just <| a^b
    , fuzz2
      ( Fuzz.intRange 0 10 )
      ( Fuzz.intRange 1 9 )
        "(a = 0-10)^^(b = 1-9) == Just <| round <| a^b"
      <|\a b ->
          Expect.equal
          ( MyBasics.intExponent a b )
          <|Just <| a^b
    , fuzz2
      ( Fuzz.intRange 1 1000000000 )
      ( Fuzz.constant 0 )
        "(a>0)^^0 == Just 1"
      <|\a b ->
          Expect.equal
          ( MyBasics.intExponent a b )
          <|Just 1
    , fuzz2
      ( Fuzz.intRange -1000000000 -1 )
      ( Fuzz.constant 0 )
        "(a<0)^^0 == Just 1"
      <|\a b ->
          Expect.equal
          ( MyBasics.intExponent a b )
          <|Just 1
    , test "0^0 = Nothing"
      <|\_ ->
          Expect.equal
            Nothing
          <|MyBasics.intExponent 0 0
    ]
  , describe "summationUnbounded"
    [ test "summationUnbounded 0 0 (\n -> 2 ^ -n) ≈ 2"
      <|\_ ->
          let
            result = MyBasics.summationUnbounded 0 0 (\n -> 2 ^ toFloat -n)
          in
            Expect.within (Expect.Absolute 1e-14) result 2
    ]
  , describe "summationBounded"
    [ test "summationBounded 10 20 (\n -> n) == 165"
      <|\_ ->
          let
            result = MyBasics.summationBounded 10 20 (\n -> toFloat n)
          in
            Expect.within (Expect.Absolute 1e-14) result 165
    ]
  ]
