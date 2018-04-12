/////////////////////////////////////////////////////////////////////////////
// Tests the new Channel function that creates a new Channel               //
//                                                                         //
/////////////////////////////////////////////////////////////////////////////

/************ Channels with default buffersize ***********/
@test
def newChannel01(): Unit =
  let x = channel Int;
  ()

@test
def newChannel02(): Channel[Int] = channel Int

@test
def newChannel03(): Channel[Bool] = channel Bool

@test
def newChannel04(): Channel[Char] = channel Char

@test
def newChannel05(): Channel[Int8] = channel Int8

@test
def newChannel06(): Channel[Int16] = channel Int16

@test
def newChannel07(): Channel[Int32] = channel Int32

@test
def newChannel08(): Channel[Int64] = channel Int64

@test
def newChannel09(): Channel[BigInt] = channel BigInt

@test
def newChannel10(): Channel[Float32] = channel Float32

@test
def newChannel11(): Channel[Float64] = channel Float64

@test
def newChannel12(): Channel[Str] = channel Str

@test
def newChannel13(): Channel[Int -> Int] = channel Int -> Int

@test
def newChannel14(): Channel[(Int, Str) -> Int] = channel (Int, Str) -> Int

@test
def newChannel15(): Channel[(Int, Str) -> Int] = channel Int -> Str -> Int

@test
def newChannel16(): Channel[Channel[Int]] = channel Channel[Int]

@test
def newChannel17(): Channel[Channel[Channel[Int]]] = channel Channel[Channel[Int]]

@test
def newChannel18(): Channel[Channel[Channel[Channel[Int]]]] = channel Channel[Channel[Channel[Int]]]

/************ Channels with specified buffersize ***********/

@test
def newChannel19(): Unit =
  let x = channel Int:2;
  ()

@test
def newChannel20(): Channel[Int] = channel Int:2

@test
def newChannel21(): Channel[Bool] = channel Bool:2

@test
def newChannel22(): Channel[Char] = channel Char:2

@test
def newChannel23(): Channel[Int8] = channel Int8:2

@test
def newChannel24(): Channel[Int16] = channel Int16:2

@test
def newChannel25(): Channel[Int32] = channel Int32:2

@test
def newChannel26(): Channel[Int64] = channel Int64:2

@test
def newChannel27(): Channel[BigInt] = channel BigInt:2

@test
def newChannel28(): Channel[Float32] = channel Float32:2

@test
def newChannel29(): Channel[Float64] = channel Float64:2

@test
def newChannel30(): Channel[Str] = channel Str:2

@test
def newChannel31(): Channel[Int -> Int] = channel Int -> Int:2

@test
def newChannel32(): Channel[(Int, Str) -> Int] = channel (Int, Str) -> Int:2

@test
def newChannel33(): Channel[(Int, Str) -> Int] = channel Int -> Str -> Int:2

@test
def newChannel34(): Channel[Channel[Int]] = channel Channel[Int]:2

def bufferSize():Int = 2

@test
def newChannel(): Channel[Int] = channel Int:bufferSize()
