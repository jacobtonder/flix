import Char

/////////////////////////////////////////////////////////////////////////////
// Select expressions (channel select)                                     //
/////////////////////////////////////////////////////////////////////////////


// Units
def f01(ch: Channel[Unit]): Channel[Unit] =
  ch <- ()

def selectChannel01(): Unit =
  let ch = channel Unit;
  let _ = spawn f01(ch);
  select {
    case x <- ch => x
  }

@test
def testSelectChannel01(): Bool = selectChannel01() `assertEq!` ()

def f02(ch: Channel[Unit]): Channel[Unit] =
  ch <- ()

def selectChannel02(): Unit =
  let ch = channel Unit 1;
  let _ = spawn f02(ch);
  select {
    case x <- ch => x
  }

@test
def testSelectChannel02(): Bool = selectChannel02() `assertEq!` ()

def f03(ch: Channel[Unit]): Channel[Unit] =
  ch <- ()

def selectChannel03(): Unit =
  let ch = channel Unit 10;
  let _ = spawn f03(ch);
  select {
    case x <- ch => x
  }

@test
def testSelectChannel03(): Bool = selectChannel03() `assertEq!` ()

def f04(ch: Channel[Unit]): Channel[Unit] =
  ch <- ()

def selectChannel04(): Unit =
  let ch = channel Unit 2147483647;
  let _ = spawn f04(ch);
  select {
    case x <- ch => x
  }

@test
def testSelectChannel04(): Bool = selectChannel04() `assertEq!` ()

def selectChannel05(): Unit =
  let ch = channel Unit 1;
  let _ = ch <- ();
  select {
    case x <- ch => x
  }

@test
def testSelectChannel05(): Bool = selectChannel05() `assertEq!` ()

def selectChannel06(): Unit =
  let ch = channel Unit 1;
  let _ = ch <- ();
  select {
    case x <- ch => ()
  }

@test
def testSelectChannel06(): Bool = selectChannel06() `assertEq!` ()

def f07(ch: Channel[Unit]): Channel[Unit] =
  ch <- ()

def selectChannel07(): Unit =
  let ch1 = channel Unit 1;
  let ch2 = channel Unit 1;
  let _ = spawn f07(ch1);
  select {
    case x <- ch1 => x
  }

@test
def testSelectChannel07(): Bool = selectChannel07() `assertEq!` ()

def f08(ch: Channel[Unit]): Channel[Unit] =
  ch <- ()

def selectChannel08(): Unit =
  let ch1 = channel Unit 1;
  let ch2 = channel Unit 1;
  let _ = spawn f08(ch2);
  select {
    case x <- ch2 => x
  }

@test
def testSelectChannel08(): Bool = selectChannel08() `assertEq!` ()


// Bool
def f09(ch: Channel[Bool]): Channel[Bool] =
  ch <- true

def selectChannel09(): Bool =
  let ch = channel Bool;
  let _ = spawn f09(ch);
  select {
    case x <- ch => x
  }

@test
def testSelectChannel09(): Bool = selectChannel09() `assertEq!` true

def f10(ch: Channel[Bool]): Channel[Bool] =
  ch <- false

def selectChannel10(): Bool =
  let ch = channel Bool;
  let _ = spawn f10(ch);
  select {
    case x <- ch => x
  }

@test
def testSelectChannel10(): Bool = selectChannel10() `assertEq!` false

def f11(ch: Channel[Bool]): Channel[Bool] =
  ch <- true

def selectChannel11(): Bool =
  let ch = channel Bool 1;
  let _ = spawn f11(ch);
  select {
    case x <- ch => x
  }

@test
def testSelectChannel11(): Bool = selectChannel11() `assertEq!` true

def f12(ch: Channel[Bool]): Channel[Bool] =
  ch <- false

def selectChannel12(): Bool =
  let ch = channel Bool 1;
  let _ = spawn f12(ch);
  select {
    case x <- ch => x
  }

@test
def testSelectChannel12(): Bool = selectChannel12() `assertEq!` false

def f13(ch: Channel[Bool]): Channel[Bool] =
  ch <- true

def selectChannel13(): Bool =
  let ch = channel Bool 10;
  let _ = spawn f13(ch);
  select {
    case x <- ch => x
  }

@test
def testSelectChannel13(): Bool = selectChannel13() `assertEq!` true

def f14(ch: Channel[Bool]): Channel[Bool] =
  ch <- false

def selectChannel14(): Bool =
  let ch = channel Bool 10;
  let _ = spawn f14(ch);
  select {
    case x <- ch => x
  }

@test
def testSelectChannel14(): Bool = selectChannel14() `assertEq!` false

def f15(ch: Channel[Bool]): Channel[Bool] =
  ch <- true

def selectChannel15(): Bool =
  let ch = channel Bool 2147483647;
  let _ = spawn f15(ch);
  select {
    case x <- ch => x
  }

@test
def testSelectChannel15(): Bool = selectChannel15() `assertEq!` true

def f16(ch: Channel[Bool]): Channel[Bool] =
  ch <- false

def selectChannel16(): Bool =
  let ch = channel Bool 2147483647;
  let _ = spawn f16(ch);
  select {
    case x <- ch => x
  }

@test
def testSelectChannel16(): Bool = selectChannel16() `assertEq!` false

def selectChannel17(): Bool =
  let ch = channel Bool 1;
  let _ = ch <- true;
  select {
    case x <- ch => x
  }

@test
def testSelectChannel17(): Bool = selectChannel17() `assertEq!` true

def selectChannel18(): Bool =
  let ch = channel Bool 1;
  let _ = ch <- false;
  select {
    case x <- ch => x
  }

@test
def testSelectChannel18(): Bool = selectChannel18() `assertEq!` false

def selectChannel19(): Bool =
  let ch = channel Bool 1;
  let _ = ch <- true;
  select {
    case x <- ch => true
  }

@test
def testSelectChannel19(): Bool = selectChannel19() `assertEq!` true

def selectChannel20(): Bool =
  let ch = channel Bool 1;
  let _ = ch <- false;
  select {
    case x <- ch => false
  }

@test
def testSelectChannel20(): Bool = selectChannel20() `assertEq!` false

def f21(ch: Channel[Bool]): Channel[Bool] =
  ch <- true

def selectChannel21(): Bool =
  let ch1 = channel Bool 1;
  let ch2 = channel Bool 1;
  let _ = spawn f21(ch1);
  select {
    case x <- ch1 => x
  }

@test
def testSelectChannel21(): Bool = selectChannel21() `assertEq!` true

def f22(ch: Channel[Bool]): Channel[Bool] =
  ch <- false

def selectChannel22(): Bool =
  let ch1 = channel Bool 1;
  let ch2 = channel Bool 1;
  let _ = spawn f22(ch1);
  select {
    case x <- ch1 => x
  }

@test
def testSelectChannel22(): Bool = selectChannel22() `assertEq!` false

def f23(ch: Channel[Bool]): Channel[Bool] =
  ch <- true

def selectChannel23(): Bool =
  let ch1 = channel Bool 1;
  let ch2 = channel Bool 1;
  let _ = spawn f23(ch2);
  select {
    case x <- ch2 => x
  }

@test
def testSelectChannel23(): Bool = selectChannel23() `assertEq!` true

def f24(ch: Channel[Bool]): Channel[Bool] =
  ch <- false

def selectChannel24(): Bool =
  let ch1 = channel Bool 1;
  let ch2 = channel Bool 1;
  let _ = spawn f24(ch2);
  select {
    case x <- ch2 => x
  }

@test
def testSelectChannel24(): Bool = selectChannel24() `assertEq!` false

/*
// Int
def f25(ch: Channel[Int]): Channel[Int] =
  ch <- true

def selectChannel25(): Int =
  let ch = channel Int;
  let _ = spawn f25(ch);
  select {
    case x <- ch => x
  }

@test
def testSelectChannel25(): Int = selectChannel25() `assertEq!` true

def f26(ch: Channel[Int]): Channel[Int] =
  ch <- false

def selectChannel26(): Int =
  let ch = channel Int;
  let _ = spawn f26(ch);
  select {
    case x <- ch => x
  }

@test
def testSelectChannel26(): Int = selectChannel26() `assertEq!` false

def f27(ch: Channel[Int]): Channel[Int] =
  ch <- true

def selectChannel27(): Int =
  let ch = channel Int 1;
  let _ = spawn f27(ch);
  select {
    case x <- ch => x
  }

@test
def testSelectChannel27(): Int = selectChannel27() `assertEq!` true

def f28(ch: Channel[Int]): Channel[Int] =
  ch <- false

def selectChannel28(): Int =
  let ch = channel Int 1;
  let _ = spawn f28(ch);
  select {
    case x <- ch => x
  }

@test
def testSelectChannel28(): Int = selectChannel28() `assertEq!` false

def f29(ch: Channel[Int]): Channel[Int] =
  ch <- true

def selectChannel29(): Int =
  let ch = channel Int 10;
  let _ = spawn f29(ch);
  select {
    case x <- ch => x
  }

@test
def testSelectChannel29(): Int = selectChannel29() `assertEq!` true

def f30(ch: Channel[Int]): Channel[Int] =
  ch <- false

def selectChannel30(): Int =
  let ch = channel Int 10;
  let _ = spawn f30(ch);
  select {
    case x <- ch => x
  }

@test
def testSelectChannel30(): Int = selectChannel30() `assertEq!` false

def f31(ch: Channel[Int]): Channel[Int] =
  ch <- true

def selectChannel31(): Int =
  let ch = channel Int 2147483647;
  let _ = spawn f31(ch);
  select {
    case x <- ch => x
  }

@test
def testSelectChannel31(): Int = selectChannel31() `assertEq!` true

def f32(ch: Channel[Int]): Channel[Int] =
  ch <- false

def selectChannel32(): Int =
  let ch = channel Int 2147483647;
  let _ = spawn f32(ch);
  select {
    case x <- ch => x
  }

@test
def testSelectChannel32(): Int = selectChannel32() `assertEq!` false

def selectChannel33(): Int =
  let ch = channel Int 1;
  let _ = ch <- true;
  select {
    case x <- ch => x
  }

@test
def testSelectChannel33(): Int = selectChannel33() `assertEq!` true

def selectChannel34(): Int =
  let ch = channel Int 1;
  let _ = ch <- false;
  select {
    case x <- ch => x
  }

@test
def testSelectChannel34(): Int = selectChannel34() `assertEq!` false

def selectChannel19(): Int =
  let ch = channel Int 1;
  let _ = ch <- true;
  select {
    case x <- ch => true
  }

@test
def testSelectChannel19(): Int = selectChannel19() `assertEq!` true

def selectChannel20(): Int =
  let ch = channel Int 1;
  let _ = ch <- false;
  select {
    case x <- ch => false
  }

@test
def testSelectChannel20(): Int = selectChannel20() `assertEq!` false

def f21(ch: Channel[Int]): Int =
  ch <- true

def selectChannel21(): Int =
  let ch1 = channel Int 1;
  let ch2 = channel Int 1;
  let _ = spawn f21(ch1);
  select {
    case x <- ch1 => x
  }

@test
def testSelectChannel21(): Int = selectChannel21() `assertEq!` true

def f22(ch: Channel[Int]): Int =
  ch <- false

def selectChannel22(): Int =
  let ch1 = channel Int 1;
  let ch2 = channel Int 1;
  let _ = spawn f22(ch1);
  select {
    case x <- ch1 => x
  }

@test
def testSelectChannel22(): Int = selectChannel22() `assertEq!` false

def f23(ch: Channel[Int]): Int =
  ch <- true

def selectChannel23(): Int =
  let ch1 = channel Int 1;
  let ch2 = channel Int 1;
  let _ = spawn f23(ch2);
  select {
    case x <- ch1 => x
  }

@test
def testSelectChannel23(): Int = selectChannel23() `assertEq!` true

def f24(ch: Channel[Int]): Int =
  ch <- false

def selectChannel24(): Int =
  let ch1 = channel Int 1;
  let ch2 = channel Int 1;
  let _ = spawn f24(ch2);
  select {
    case x <- ch1 => x
  }

@test
def testSelectChannel24(): Int = selectChannel24() `assertEq!` false
*/











/*
// Test units
def testSelectChannelUnit(): Unit =
  let ch: Channel[Unit] = channel Unit 1;
  let _ = ch <- ();
  select {
      case x <- ch => x
  }

@test
def testSelectChannel01(): Bool = testSelectChannelUnit() `assertEq!` ()

// Test bools
def testSelectChannelBool(var: Bool): Bool =
  let ch: Channel[Bool] = channel Bool 1;
  let _ = ch <- var;
  select {
      case x <- ch => x
  }

@test
def testSelectChannel02(): Bool = testSelectChannelBool(true) `assertEq!` true

@test
def testSelectChannel03(): Bool = testSelectChannelBool(false) `assertEq!` false

// Test integers
def testSelectChannelInt(var: Int): Int =
  let ch: Channel[Int] = channel Int 1;
  let _ = ch <- var;
  select {
      case x <- ch => x
  }

@test
def testSelectChannel04(): Bool = testSelectChannelInt(0) `assertEq!` 0

@test
def testSelectChannel6(): Bool = testSelectChannelInt(-1) `assertEq!` -1

@test
def testSelectChannel06(): Bool = testSelectChannelInt(1) `assertEq!` 1

@test
def testSelectChannel07(): Bool = testSelectChannelInt(-100) `assertEq!` -100

@test
def testSelectChannel08(): Bool = testSelectChannelInt(100) `assertEq!` 100

// Test floats
def testSelectChannelFloat(var: Float64): Float64 =
  let ch: Channel[Float64] = channel Float64 1;
  let _ = ch <- var;
  select {
     case x <- ch => x
  }

// @test
// def testSelectChannel09(): Bool = testSelectChannelFloat32(0f32) `assertEq!` 0f32
//
// @test
// def testSelectChannel10(): Bool = testSelectChannelFloat32(-1.1f32) `assertEq!` -1.1f32
//
// @test
// def testSelectChannel11(): Bool = testSelectChannelFloat32(1.1f32) `assertEq!` 1.1f32

// Test strings
def testSelectChannelStr(var: Str): Str =
  let ch: Channel[Str] = channel Str 1;
  let _ = ch <- var;
  select {
      case x <- ch => x
  }

@test
def testSelectChannel12(): Bool = testSelectChannelStr("") `assertEq!` ""

@test
def testSelectChannel13(): Bool = testSelectChannelStr("str") `assertEq!` "str"

// Test chars
def testSelectChannelChar(var: Char): Char =
  let ch: Channel[Char] = channel Char 1;
  let _ = ch <- var;
  select {
      case x <- ch => x
  }

@test
def testSelectChannel14(): Bool = testSelectChannelChar('a') `assertEq!` 'a'

@test
def testSelectChannel15(): Bool = testSelectChannelChar('b') `assertEq!` 'b'

// Test tuples
def testSelectChannelIntStrTuple(var: (Int, Str)): (Int, Str) =
  let ch: Channel[(Int, Str)] = channel (Int, Str);
  let _ = ch <- var;
  select {
      case x <- ch => x
  }

def testSelectChannelIntIntTuple(var: (Int, Int)): (Int, Int) =
  let ch: Channel[(Int, Int)] = channel (Int, Int);
  let _ = ch <- var;
  select {
      case x <- ch => x
  }

def testSelectChannelIntIntIntIntTuple(var: (Int, Int, Int, Int)): (Int, Int, Int, Int) =
  let ch: Channel[(Int, Int, Int, Int)] = channel (Int, Int, Int, Int);
  let _ = ch <- var;
  select {
      case x <- ch => x
  }

@test
def testSelectChannel16(): Bool = testSelectChannelIntStrTuple((2, "Str")) `assertEq!` (2, "Str")

@test
def testSelectChannel17(): Bool = testSelectChannelIntIntTuple((2, 10)) `assertEq!` (2, 10)

@test
def testSelectChannel18(): Bool = testSelectChannelIntIntIntIntTuple((2, 10, 12, 0)) `assertEq!` (2, 10, 12, 0)

// Test lists
def testSelectChannelIntList(var: List[Int]): List[Int] =
  let ch: Channel[List[Int]] = channel List[Int] 1;
  let _ = ch <- var;
  select {
      case x <- ch => x
  }

def testSelectChannelStrList(var: List[Str]): List[Str] =
  let ch: Channel[List[Str]] = channel List[Str] 1;
  let _ = ch <- var;
  select {
      case x <- ch => x
  }

@test
def testSelectChannel19(): Bool = testSelectChannelIntList(Nil) `assertEq!` Nil

@test
def testSelectChannel20(): Bool = testSelectChannelIntList(1 :: Nil) `assertEq!` 1 :: Nil

@test
def testSelectChannel21(): Bool = testSelectChannelStrList("str" :: Nil) `assertEq!` "str" :: Nil

@test
def testSelectChannel22(): Bool = testSelectChannelStrList("str" :: "trs" :: Nil) `assertEq!` "str" :: "trs" :: Nil

// Test functions
def testSelectChannelIntIntFunction(var: Int -> Int): Int -> Int =
  let ch: Channel[Int -> Int] = channel Int -> Int 1;
  let _ = ch <- var;
  select {
      case x <- ch => x
  }

def testSelectChannelIntStrFunction(var: Int -> Str): Int -> Str =
  let ch: Channel[Int -> Str] = channel Int -> Str 1;
  let _ = ch <- var;
  select {
      case x <- ch => x
  }

def testSelectChannelIntIntIntFunction(var: Int -> Int -> Int): Int -> Int -> Int =
  let ch: Channel[Int -> Int -> Int] = channel Int -> Int -> Int 1;
  let _ = ch <- var;
  select {
      case x <- ch => x
  }

def testSelectChannelTupleIntIntIntFunction(var: (Int, Int) -> Int): (Int, Int) -> Int =
  let ch: Channel[(Int, Int) -> Int] = channel (Int, Int) -> Int 1;
  let _ = ch <- var;
  select {
      case x <- ch => x
  }

@test
def testSelectChannel23(): Bool = testSelectChannelIntIntFunction(x -> x)(2) `assertEq!` 2

@test
def testSelectChannel24(): Bool = testSelectChannelIntStrFunction(x -> "2")(2) `assertEq!` "2"

//@test
//def testSelectChannel25(): Bool = testSelectChannelIntIntIntFunction((x -> y) -> x + y)(2, 3) `assertEq!` 5

//@test
//def testSelectChannel26(): Bool = testSelectChannelTupleIntIntIntFunction((x -> y) -> x + y)((2, 3)) `assertEq!` 5

// Default
//def testSelectChannelDefault(): Int =
//  let ch: Channel[Unit] = channel Unit;
//  ch <- ();;
//  select {
//    case _ <- ch => 2
//  }

//@test
//def testSelectChannel29(): Bool = testSelectChannelDefault() `assertEq!` 2

// Multiple channel types

def testSelectChannelMultipleTypes(): Str =
  let ch1: Channel[Char] = channel Char;
  let ch2: Channel[Str] = channel Str;
  let _ = spawn putStr(ch2, "a");
  select {
    case x <- ch1 => Char.toString(x)
    case x <- ch2 => x
  }

@test
def testSelectChannel30(): Bool = testSelectChannelMultipleTypes() `assertEq!` "a"

@test
def select_int_channel_first(): Bool =
  let ch1 = channel Int;
  let ch2 = channel Int;
  let _ = spawn putInt(ch1, 2);
  let r = select {
    case x <- ch1 => x
    case x <- ch2 => x
  };
  r `assertEq!` 2

@test
def select_int_channel_last(): Bool =
  let ch1 = channel Int;
  let ch2 = channel Int;
  let _ = spawn putInt(ch1, 2);
  let r = select {
    case x <- ch1 => x + 1
    case x <- ch2 => x + 2
  };
  r `assertEq!` 3


@test
def select_string_channel_first(): Bool =
  let ch1 = channel Str;
  let ch2 = channel Str;
  let _ = spawn put;
  let r = select {
    case x <- ch1 => x + "1"
    case x <- ch2 => x + "2"
  };
  let _ = ch1 <- "str";
  r `assertEq!` "str1"


@test
def select_string_channel_last(): Bool =
  let ch1 = channel Str;
  let ch2 = channel Str;
  let r = select {
    case x <- ch1 => x + "1"
    case x <- ch2 => x + "2"
  };
  let _ = ch2 <- "str";
  r `assertEq!` "str2"
*/
