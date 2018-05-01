/////////////////////////////////////////////////////////////////////////////
// Tests the new Channel function that creates a new Channel               //
//                                                                         //
/////////////////////////////////////////////////////////////////////////////

/************ Channels with default buffersize ***********/
enum Color {
    case Red,
    case Green,
    case Blue
}

@test
def testNewChannel01(): Unit =
  let x = channel Int;
  ()

@test
def testNewChannel02(): Channel[Int] = channel Int

@test
def testNewChannel03(): Channel[Bool] = channel Bool

@test
def testNewChannel04(): Channel[Char] = channel Char

@test
def testNewChannel05(): Channel[Int8] = channel Int8

@test
def testNewChannel06(): Channel[Int16] = channel Int16

@test
def testNewChannel07(): Channel[Int32] = channel Int32

@test
def testNewChannel08(): Channel[Int64] = channel Int64

@test
def testNewChannel09(): Channel[BigInt] = channel BigInt

@test
def testNewChannel10(): Channel[Float32] = channel Float32

@test
def testNewChannel11(): Channel[Float64] = channel Float64

@test
def testNewChannel12(): Channel[Str] = channel Str

@test
def testNewChannel13(): Channel[(Int, Int)] = channel (Int, Int)

//@test
//def testNewChannel14(): Channel[Opt[Int]] = channel Opt[Int]

@test
def testNewChannel15(): Channel[List[Int]] = channel List[Int]

//@test
//def testNewChannel16(): Channel[Vec[Int]] = channel Vec[Int]

@test
def testNewChannel17(): Channel[Set[Int]] = channel Set[Int]

@test
def testNewChannel18(): Channel[Color] = channel Color

@test
def testNewChannel19(): Channel[Int -> Int] = channel Int -> Int

@test
def testNewChannel20(): Channel[(Int, Str) -> Int] = channel (Int, Str) -> Int

@test
def testNewChannel21(): Channel[Int -> (Str -> Int)] = channel Int -> Str -> Int

@test
def testNewChannel22(): Channel[Channel[Int]] = channel Channel[Int]

@test
def testNewChannel23(): Channel[Channel[Int8]] = channel Channel[Int8]

@test
def testNewChannel24(): Channel[Channel[Int16]] = channel Channel[Int16]

@test
def testNewChannel25(): Channel[Channel[Int32]] = channel Channel[Int32]

@test
def testNewChannel26(): Channel[Channel[Int64]] = channel Channel[Int64]

@test
def testNewChannel27(): Channel[Channel[BigInt]] = channel Channel[BigInt]

@test
def testNewChannel28(): Channel[Channel[Bool]] = channel Channel[Bool]

@test
def testNewChannel29(): Channel[Channel[Str]] = channel Channel[Str]

@test
def testNewChannel30(): Channel[Channel[Float32]] = channel Channel[Float32]

@test
def testNewChannel31(): Channel[Channel[Float64]] = channel Channel[Float64]

@test
def testNewChannel32(): Channel[Channel[Char]] = channel Channel[Char]

@test
def testNewChannel33(): Channel[Channel[Color]] = channel Channel[Color]

@test
def testNewChannel34(): Channel[Channel[(Int, Int)]] = channel Channel[(Int, Int)]

@test
def testNewChannel35(): Channel[Channel[(Int8, Int8)]] = channel Channel[(Int8, Int8)]

@test
def testNewChannel36(): Channel[Channel[(Int16, Int16)]] = channel Channel[(Int16, Int16)]

@test
def testNewChannel37(): Channel[Channel[(Int32, Int32)]] = channel Channel[(Int32, Int32)]

@test
def testNewChannel38(): Channel[Channel[(Int64, Int64)]] = channel Channel[(Int64, Int64)]

@test
def testNewChannel39(): Channel[Channel[(BigInt, BigInt)]] = channel Channel[(BigInt, BigInt)]

@test
def testNewChannel40(): Channel[Channel[(Float32, Float32)]] = channel Channel[(Float32, Float32)]

@test
def testNewChannel41(): Channel[Channel[(Float64, Float64)]] = channel Channel[(Float64, Float64)]

@test
def testNewChannel42(): Channel[Channel[(Bool, Bool)]] = channel Channel[(Bool, Bool)]

@test
def testNewChannel43(): Channel[Channel[(Str, Str)]] = channel Channel[(Str, Str)]

@test
def testNewChannel44(): Channel[Channel[(Char, Char)]] = channel Channel[(Char, Char)]

@test
def testNewChannel45(): Channel[Channel[(Color, Color)]] = channel Channel[(Color, Color)]

@test
def testNewChannel46(): Channel[Channel[Channel[Int]]] = channel Channel[Channel[Int]]

@test
def testNewChannel47(): Channel[Channel[Channel[Int8]]] = channel Channel[Channel[Int8]]

@test
def testNewChannel48(): Channel[Channel[Channel[Int16]]] = channel Channel[Channel[Int16]]

@test
def testNewChannel49(): Channel[Channel[Channel[Int32]]] = channel Channel[Channel[Int32]]

@test
def testNewChannel50(): Channel[Channel[Channel[Int64]]] = channel Channel[Channel[Int64]]

@test
def testNewChannel51(): Channel[Channel[Channel[BigInt]]] = channel Channel[Channel[BigInt]]

@test
def testNewChannel52(): Channel[Channel[Channel[Float32]]] = channel Channel[Channel[Float32]]

@test
def testNewChannel53(): Channel[Channel[Channel[Float64]]] = channel Channel[Channel[Float64]]

@test
def testNewChannel54(): Channel[Channel[Channel[Bool]]] = channel Channel[Channel[Bool]]

@test
def testNewChannel55(): Channel[Channel[Channel[Str]]] = channel Channel[Channel[Str]]

@test
def testNewChannel56(): Channel[Channel[Channel[Char]]] = channel Channel[Channel[Char]]

@test
def testNewChannel57(): Channel[Channel[Channel[Color]]] = channel Channel[Channel[Color]]

@test
def testNewChannel58(): Channel[Channel[Channel[(Int, Int)]]] = channel Channel[Channel[(Int, Int)]]

@test
def testNewChannel59(): Channel[Channel[Channel[(Int8, Int8)]]] = channel Channel[Channel[(Int8, Int8)]]

@test
def testNewChannel60(): Channel[Channel[Channel[(Int16, Int16)]]] = channel Channel[Channel[(Int16, Int16)]]

@test
def testNewChannel61(): Channel[Channel[Channel[(Int32, Int32)]]] = channel Channel[Channel[(Int32, Int32)]]

@test
def testNewChannel62(): Channel[Channel[Channel[(Int64, Int64)]]] = channel Channel[Channel[(Int64, Int64)]]

@test
def testNewChannel63(): Channel[Channel[Channel[(BigInt, BigInt)]]] = channel Channel[Channel[(BigInt, BigInt)]]

@test
def testNewChannel64(): Channel[Channel[Channel[(Float32, Float32)]]] = channel Channel[Channel[(Float32, Float32)]]

@test
def testNewChannel65(): Channel[Channel[Channel[(Float64, Float64)]]] = channel Channel[Channel[(Float64, Float64)]]

@test
def testNewChannel66(): Channel[Channel[Channel[(Bool, Bool)]]] = channel Channel[Channel[(Bool, Bool)]]

@test
def testNewChannel67(): Channel[Channel[Channel[(Str, Str)]]] = channel Channel[Channel[(Str, Str)]]

@test
def testNewChannel68(): Channel[Channel[Channel[(Char, Char)]]] = channel Channel[Channel[(Char, Char)]]

@test
def testNewChannel69(): Channel[Channel[Channel[(Color, Color)]]] = channel Channel[Channel[(Color, Color)]]

@test
def testNewChannel70(): Channel[Channel[Channel[Channel[Int]]]] = channel Channel[Channel[Channel[Int]]]

@test
def testNewChannel71(): Channel[Channel[Channel[Channel[Int8]]]] = channel Channel[Channel[Channel[Int8]]]

@test
def testNewChannel72(): Channel[Channel[Channel[Channel[Int16]]]] = channel Channel[Channel[Channel[Int16]]]

@test
def testNewChannel73(): Channel[Channel[Channel[Channel[Int32]]]] = channel Channel[Channel[Channel[Int32]]]

@test
def testNewChannel74(): Channel[Channel[Channel[Channel[Int64]]]] = channel Channel[Channel[Channel[Int64]]]

@test
def testNewChannel75(): Channel[Channel[Channel[Channel[BigInt]]]] = channel Channel[Channel[Channel[BigInt]]]

@test
def testNewChannel76(): Channel[Channel[Channel[Channel[Float32]]]] = channel Channel[Channel[Channel[Float32]]]

@test
def testNewChannel77(): Channel[Channel[Channel[Channel[Float64]]]] = channel Channel[Channel[Channel[Float64]]]

@test
def testNewChannel78(): Channel[Channel[Channel[Channel[Bool]]]] = channel Channel[Channel[Channel[Bool]]]

@test
def testNewChannel79(): Channel[Channel[Channel[Channel[Str]]]] = channel Channel[Channel[Channel[Str]]]

@test
def testNewChannel80(): Channel[Channel[Channel[Channel[Char]]]] = channel Channel[Channel[Channel[Char]]]

@test
def testNewChannel81(): Channel[Channel[Channel[Channel[Color]]]] = channel Channel[Channel[Channel[Color]]]

@test
def testNewChannel82(): Channel[Channel[Channel[Channel[(Int, Int)]]]] = channel Channel[Channel[Channel[(Int, Int)]]]

@test
def testNewChannel83(): Channel[Channel[Channel[Channel[(Int8, Int8)]]]] = channel Channel[Channel[Channel[(Int8, Int8)]]]

@test
def testNewChannel84(): Channel[Channel[Channel[Channel[(Int16, Int16)]]]] = channel Channel[Channel[Channel[(Int16, Int16)]]]

@test
def testNewChannel85(): Channel[Channel[Channel[Channel[(Int32, Int32)]]]] = channel Channel[Channel[Channel[(Int32, Int32)]]]

@test
def testNewChannel86(): Channel[Channel[Channel[Channel[(Int64, Int64)]]]] = channel Channel[Channel[Channel[(Int64, Int64)]]]

@test
def testNewChannel87(): Channel[Channel[Channel[Channel[(BigInt, BigInt)]]]] = channel Channel[Channel[Channel[(BigInt, BigInt)]]]

@test
def testNewChannel88(): Channel[Channel[Channel[Channel[(Float32, Float32)]]]] = channel Channel[Channel[Channel[(Float32, Float32)]]]

@test
def testNewChannel89(): Channel[Channel[Channel[Channel[(Float64, Float64)]]]] = channel Channel[Channel[Channel[(Float64, Float64)]]]

@test
def testNewChannel90(): Channel[Channel[Channel[Channel[(Bool, Bool)]]]] = channel Channel[Channel[Channel[(Bool, Bool)]]]

@test
def testNewChannel91(): Channel[Channel[Channel[Channel[(Str, Str)]]]] = channel Channel[Channel[Channel[(Str, Str)]]]

@test
def testNewChannel92(): Channel[Channel[Channel[Channel[(Char, Char)]]]] = channel Channel[Channel[Channel[(Char, Char)]]]

@test
def testNewChannel93(): Channel[Channel[Channel[Channel[(Color, Color)]]]] = channel Channel[Channel[Channel[(Color, Color)]]]

@test
def testNewChannel94(): Channel[Channel[Channel[Channel[(Char, Char)]]]] = channel Channel[Channel[Channel[(Char, Char)]]]

@test
def testNewChannel95(): Channel[Channel[Unit]] = channel Channel[Unit]

@test
def testNewChannel96(): Channel[Channel[(Unit, Unit)]] = channel Channel[(Unit, Unit)]

@test
def testNewChannel97(): Channel[Channel[Channel[Unit]]] = channel Channel[Channel[Unit]]

@test
def testNewChannel98(): Channel[Channel[Channel[(Unit, Unit)]]] = channel Channel[Channel[(Unit, Unit)]]

@test
def testNewChannel99(): Channel[Channel[Channel[Channel[Unit]]]] = channel Channel[Channel[Channel[Unit]]]

@test
def testNewChannel100(): Channel[Channel[Channel[Channel[(Unit, Unit)]]]] = channel Channel[Channel[Channel[(Unit, Unit)]]]


/************ Channels with specified buffersize ***********/

@test
def testNewChannel101(): Unit =
  let x = channel Int 2;
  ()

@test
def testNewChannel102(): Channel[Unit] = channel Unit 2

@test
def testNewChannel103(): Channel[Int] = channel Int 2

@test
def testNewChannel104(): Channel[Bool] = channel Bool 2

@test
def testNewChannel105(): Channel[Char] = channel Char 2

@test
def testNewChannel106(): Channel[Int8] = channel Int8 2

@test
def testNewChannel107(): Channel[Int16] = channel Int16 2

@test
def testNewChannel108(): Channel[Int32] = channel Int32 2

@test
def testNewChannel109(): Channel[Int64] = channel Int64 2

@test
def testNewChannel110(): Channel[BigInt] = channel BigInt 2

@test
def testNewChannel111(): Channel[Float32] = channel Float32 2

@test
def testNewChannel112(): Channel[Float64] = channel Float64 2

@test
def testNewChannel113(): Channel[Str] = channel Str 2

//@test
//def testNewChannel114(): Channel[(Int, Int)] = channel (Int, Int) 2
//
//@test
//def testNewChannel115(): Channel[Opt[Int]] = channel Opt[Int] 2

@test
def testNewChannel116(): Channel[List[Int]] = channel List[Int] 2

//@test
//def testNewChannel117(): Channel[Vec[Int]] = channel Vec[Int] 2

@test
def testNewChannel118(): Channel[Set[Int]] = channel Set[Int] 2

@test
def testNewChannel119(): Channel[Color] = channel Color 2

@test
def testNewChannel120(): Channel[Int -> Int] = channel Int -> Int 2

@test
def testNewChannel121(): Channel[(Int, Str) -> Int] = channel (Int, Str) -> Int 2

@test
def testNewChannel122(): Channel[Int -> Str -> Int] = channel Int -> Str -> Int 2

@test
def testNewChannel123(): Channel[Channel[Int]] = channel Channel[Int] 2

def bufferSize(): Int = 2
@test
def testNewChannel124(): Channel[Int] = channel Int bufferSize()


