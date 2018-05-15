/////////////////////////////////////////////////////////////////////////////
// Put channel expressions                                                 //
/////////////////////////////////////////////////////////////////////////////


// Unit
// 01
def f011(ch: Channel[Unit]): Channel[Unit] =
    ch <- ()

def putChannelUnit01(): Unit =
    let ch = channel Unit 1;
    let _ = spawn f011(ch);
    <- ch

@test
def testPutChannelUnit01(): Bool = putChannelUnit01() `assertEq!` ()


// 02
def f021(ch: Channel[Unit]): Channel[Unit] =
    ch <- ()

def f022(to: Channel[Unit], from: Channel[Unit]): Channel[Unit] =
    to <- <- from

def putChannelUnit02(): Unit =
    let ch1 = channel Unit 1;
    let ch2 = channel Unit 1;
    let _ = spawn f021(ch1);
    let _ = spawn f022(ch2, ch1);
    <- ch2

@test
def testPutChannelUnit02(): Bool = putChannelUnit02() `assertEq!` ()


// 03
def f031(ch: Channel[Unit]): Channel[Unit] =
    ch <- ()

def f032(to: Channel[Unit], from: Channel[Unit]): Channel[Unit] =
    to <- <- from

def putChannelUnit03(): Unit =
    let ch1 = channel Unit 1;
    let ch2 = channel Unit 1;
    let ch3 = channel Unit 1;
    let _ = spawn f031(ch1);
    let _ = spawn f031(ch1);
    let _ = spawn f032(ch2, ch1);
    let _ = spawn f032(ch2, ch1);
    let _ = spawn f032(ch3, ch2);
    let _ = <- ch2;
    <- ch3

@test
def testPutChannelUnit03(): Bool = putChannelUnit03() `assertEq!` ()


// 04
def f041(ch: Channel[Unit]): Channel[Unit] =
    ch <- ()

def f042(to: Channel[Unit], through: Channel[Unit], from: Channel[Unit]): Channel[Unit] =
    to <- <- through <- <- from

def putChannelUnit04(): Unit =
    let ch1 = channel Unit 1;
    let ch2 = channel Unit 1;
    let ch3 = channel Unit 1;
    let _ = spawn f041(ch1);
    let _ = spawn f041(ch1);
    let _ = spawn f042(ch3, ch2, ch1);
    let _ = spawn f042(ch3, ch2, ch1);
    let _ = <- ch3;
    <- ch3

@test
def testPutChannelUnit04(): Bool = putChannelUnit04() `assertEq!` ()


// 05
def f051(ch: Channel[Unit], v1: Unit, v2: Unit): Channel[Unit] =
    ch <- v1 <- v2

def putChannelUnit05(): Unit =
    let ch = channel Unit 1;
    let _ = spawn f051(ch, (), ());
    let _ = <- ch;
    <- ch

@test
def testPutChannelUnit05(): Bool = putChannelUnit05() `assertEq!` ()



// Bool
// 01
def f111(ch: Channel[Bool]): Channel[Bool] =
    ch <- true

def putChannelBool01(): Bool =
    let ch = channel Bool 1;
    let _ = spawn f111(ch);
    <- ch

@test
def testPutChannelBool01(): Bool = putChannelBool01() `assertEq!` true


// 02
def f121(ch: Channel[Bool]): Channel[Bool] =
    ch <- true

def f122(to: Channel[Bool], from: Channel[Bool]): Channel[Bool] =
    to <- <- from

def putChannelBool02(): Bool =
    let ch1 = channel Bool 1;
    let ch2 = channel Bool 1;
    let _ = spawn f121(ch1);
    let _ = spawn f122(ch2, ch1);
    <- ch2

@test
def testPutChannelBool02(): Bool = putChannelBool02() `assertEq!` true


// 03
def f131(ch: Channel[Bool]): Channel[Bool] =
    ch <- true

def f132(to: Channel[Bool], from: Channel[Bool]): Channel[Bool] =
    to <- <- from

def putChannelBool03(): Bool =
    let ch1 = channel Bool 1;
    let ch2 = channel Bool 1;
    let ch3 = channel Bool 1;
    let _ = spawn f131(ch1);
    let _ = spawn f131(ch1);
    let _ = spawn f132(ch2, ch1);
    let _ = spawn f132(ch2, ch1);
    let _ = spawn f132(ch3, ch2);
    let _ = <- ch2;
    <- ch3

@test
def testPutChannelBool03(): Bool = putChannelBool03() `assertEq!` true


// 04
def f141(ch: Channel[Bool]): Channel[Bool] =
    ch <- true

def f142(to: Channel[Bool], through: Channel[Bool], from: Channel[Bool]): Channel[Bool] =
    to <- <- through <- <- from

def putChannelBool04(): Bool =
    let ch1 = channel Bool 1;
    let ch2 = channel Bool 1;
    let ch3 = channel Bool 1;
    let _ = spawn f141(ch1);
    let _ = spawn f141(ch1);
    let _ = spawn f142(ch3, ch2, ch1);
    let _ = spawn f142(ch3, ch2, ch1);
    let _ = <- ch3;
    <- ch3

@test
def testPutChannelBool04(): Bool = putChannelBool04() `assertEq!` true


// 05
def f151(ch: Channel[Bool], v1: Bool, v2: Bool): Channel[Bool] =
    ch <- v1 <- v2

def putChannelBool05(): Bool =
    let ch = channel Bool 1;
    let _ = spawn f151(ch, true, true);
    let _ = <- ch;
    <- ch

@test
def testPutChannelBool05(): Bool = putChannelBool05() `assertEq!` true



// Int8
// 01
def f211(ch: Channel[Int8]): Channel[Int8] =
    ch <- 1i8

def putChannelInt801(): Int8 =
    let ch = channel Int8 1;
    let _ = spawn f211(ch);
    <- ch

@test
def testPutChannelInt801(): Bool = putChannelInt801() `assertEq!` 1i8


// 02
def f221(ch: Channel[Int8]): Channel[Int8] =
    ch <- 1i8

def f222(to: Channel[Int8], from: Channel[Int8]): Channel[Int8] =
    to <- <- from

def putChannelInt802(): Int8 =
    let ch1 = channel Int8 1;
    let ch2 = channel Int8 1;
    let _ = spawn f221(ch1);
    let _ = spawn f222(ch2, ch1);
    <- ch2

@test
def testPutChannelInt802(): Bool = putChannelInt802() `assertEq!` 1i8


// 03
def f231(ch: Channel[Int8]): Channel[Int8] =
    ch <- 1i8

def f232(to: Channel[Int8], from: Channel[Int8]): Channel[Int8] =
    to <- <- from

def putChannelInt803(): Int8 =
    let ch1 = channel Int8 1;
    let ch2 = channel Int8 1;
    let ch3 = channel Int8 1;
    let _ = spawn f231(ch1);
    let _ = spawn f231(ch1);
    let _ = spawn f232(ch2, ch1);
    let _ = spawn f232(ch2, ch1);
    let _ = spawn f232(ch3, ch2);
    let _ = <- ch2;
    <- ch3

@test
def testPutChannelInt803(): Bool = putChannelInt803() `assertEq!` 1i8


// 04
def f241(ch: Channel[Int8]): Channel[Int8] =
    ch <- 1i8

def f242(to: Channel[Int8], through: Channel[Int8], from: Channel[Int8]): Channel[Int8] =
    to <- <- through <- <- from

def putChannelInt804(): Int8 =
    let ch1 = channel Int8 1;
    let ch2 = channel Int8 1;
    let ch3 = channel Int8 1;
    let _ = spawn f241(ch1);
    let _ = spawn f241(ch1);
    let _ = spawn f242(ch3, ch2, ch1);
    let _ = spawn f242(ch3, ch2, ch1);
    let _ = <- ch3;
    <- ch3

@test
def testPutChannelInt804(): Bool = putChannelInt804() `assertEq!` 1i8


// 05
def f251(ch: Channel[Int8], v1: Int8, v2: Int8): Channel[Int8] =
    ch <- v1 <- v2

def putChannelInt805(): Int8 =
    let ch = channel Int8 1;
    let _ = spawn f251(ch, 1i8, 1i8);
    let _ = <- ch;
    <- ch

@test
def testPutChannelInt805(): Bool = putChannelInt805() `assertEq!` 1i8



// Int16
// 01
def f311(ch: Channel[Int16]): Channel[Int16] =
    ch <- 1i16

def putChannelInt1601(): Int16 =
    let ch = channel Int16 1;
    let _ = spawn f311(ch);
    <- ch

@test
def testPutChannelInt1601(): Bool = putChannelInt1601() `assertEq!` 1i16


// 02
def f321(ch: Channel[Int16]): Channel[Int16] =
    ch <- 1i16

def f322(to: Channel[Int16], from: Channel[Int16]): Channel[Int16] =
    to <- <- from

def putChannelInt1602(): Int16 =
    let ch1 = channel Int16 1;
    let ch2 = channel Int16 1;
    let _ = spawn f321(ch1);
    let _ = spawn f322(ch2, ch1);
    <- ch2

@test
def testPutChannelInt1602(): Bool = putChannelInt1602() `assertEq!` 1i16


// 03
def f331(ch: Channel[Int16]): Channel[Int16] =
    ch <- 1i16

def f332(to: Channel[Int16], from: Channel[Int16]): Channel[Int16] =
    to <- <- from

def putChannelInt1603(): Int16 =
    let ch1 = channel Int16 1;
    let ch2 = channel Int16 1;
    let ch3 = channel Int16 1;
    let _ = spawn f331(ch1);
    let _ = spawn f331(ch1);
    let _ = spawn f332(ch2, ch1);
    let _ = spawn f332(ch2, ch1);
    let _ = spawn f332(ch3, ch2);
    let _ = <- ch2;
    <- ch3

@test
def testPutChannelInt1603(): Bool = putChannelInt1603() `assertEq!` 1i16


// 04
def f341(ch: Channel[Int16]): Channel[Int16] =
    ch <- 1i16

def f342(to: Channel[Int16], through: Channel[Int16], from: Channel[Int16]): Channel[Int16] =
    to <- <- through <- <- from

def putChannelInt1604(): Int16 =
    let ch1 = channel Int16 1;
    let ch2 = channel Int16 1;
    let ch3 = channel Int16 1;
    let _ = spawn f341(ch1);
    let _ = spawn f341(ch1);
    let _ = spawn f342(ch3, ch2, ch1);
    let _ = spawn f342(ch3, ch2, ch1);
    let _ = <- ch3;
    <- ch3

@test
def testPutChannelInt1604(): Bool = putChannelInt1604() `assertEq!` 1i16


// 05
def f351(ch: Channel[Int16], v1: Int16, v2: Int16): Channel[Int16] =
    ch <- v1 <- v2

def putChannelInt1605(): Int16 =
    let ch = channel Int16 1;
    let _ = spawn f351(ch, 1i16, 1i16);
    let _ = <- ch;
    <- ch

@test
def testPutChannelInt1605(): Bool = putChannelInt1605() `assertEq!` 1i16



// Int32
// 01
def f411(ch: Channel[Int32]): Channel[Int32] =
    ch <- 1i32

def putChannelInt3201(): Int32 =
    let ch = channel Int32 1;
    let _ = spawn f411(ch);
    <- ch

@test
def testPutChannelInt3201(): Bool = putChannelInt3201() `assertEq!` 1i32


// 02
def f421(ch: Channel[Int32]): Channel[Int32] =
    ch <- 1i32

def f422(to: Channel[Int32], from: Channel[Int32]): Channel[Int32] =
    to <- <- from

def putChannelInt3202(): Int32 =
    let ch1 = channel Int32 1;
    let ch2 = channel Int32 1;
    let _ = spawn f421(ch1);
    let _ = spawn f422(ch2, ch1);
    <- ch2

@test
def testPutChannelInt3202(): Bool = putChannelInt3202() `assertEq!` 1i32


// 03
def f431(ch: Channel[Int32]): Channel[Int32] =
    ch <- 1i32

def f432(to: Channel[Int32], from: Channel[Int32]): Channel[Int32] =
    to <- <- from

def putChannelInt3203(): Int32 =
    let ch1 = channel Int32 1;
    let ch2 = channel Int32 1;
    let ch3 = channel Int32 1;
    let _ = spawn f431(ch1);
    let _ = spawn f431(ch1);
    let _ = spawn f432(ch2, ch1);
    let _ = spawn f432(ch2, ch1);
    let _ = spawn f432(ch3, ch2);
    let _ = <- ch2;
    <- ch3

@test
def testPutChannelInt3203(): Bool = putChannelInt3203() `assertEq!` 1i32


// 04
def f441(ch: Channel[Int32]): Channel[Int32] =
    ch <- 1i32

def f442(to: Channel[Int32], through: Channel[Int32], from: Channel[Int32]): Channel[Int32] =
    to <- <- through <- <- from

def putChannelInt3204(): Int32 =
    let ch1 = channel Int32 1;
    let ch2 = channel Int32 1;
    let ch3 = channel Int32 1;
    let _ = spawn f441(ch1);
    let _ = spawn f441(ch1);
    let _ = spawn f442(ch3, ch2, ch1);
    let _ = spawn f442(ch3, ch2, ch1);
    let _ = <- ch3;
    <- ch3

@test
def testPutChannelInt3204(): Bool = putChannelInt3204() `assertEq!` 1i32


// 05
def f451(ch: Channel[Int32], v1: Int32, v2: Int32): Channel[Int32] =
    ch <- v1 <- v2

def putChannelInt3205(): Int32 =
    let ch = channel Int32 1;
    let _ = spawn f451(ch, 1i32, 1i32);
    let _ = <- ch;
    <- ch

@test
def testPutChannelInt3205(): Bool = putChannelInt3205() `assertEq!` 1i32



// Int64
// 01
def f611(ch: Channel[Int64]): Channel[Int64] =
    ch <- 1i64

def putChannelInt6401(): Int64 =
    let ch = channel Int64 1;
    let _ = spawn f611(ch);
    <- ch

@test
def testPutChannelInt6401(): Bool = putChannelInt6401() `assertEq!` 1i64


// 02
def f621(ch: Channel[Int64]): Channel[Int64] =
    ch <- 1i64

def f622(to: Channel[Int64], from: Channel[Int64]): Channel[Int64] =
    to <- <- from

def putChannelInt6402(): Int64 =
    let ch1 = channel Int64 1;
    let ch2 = channel Int64 1;
    let _ = spawn f621(ch1);
    let _ = spawn f622(ch2, ch1);
    <- ch2

@test
def testPutChannelInt6402(): Bool = putChannelInt6402() `assertEq!` 1i64


// 03
def f631(ch: Channel[Int64]): Channel[Int64] =
    ch <- 1i64

def f632(to: Channel[Int64], from: Channel[Int64]): Channel[Int64] =
    to <- <- from

def putChannelInt6403(): Int64 =
    let ch1 = channel Int64 1;
    let ch2 = channel Int64 1;
    let ch3 = channel Int64 1;
    let _ = spawn f631(ch1);
    let _ = spawn f631(ch1);
    let _ = spawn f632(ch2, ch1);
    let _ = spawn f632(ch2, ch1);
    let _ = spawn f632(ch3, ch2);
    let _ = <- ch2;
    <- ch3

@test
def testPutChannelInt6403(): Bool = putChannelInt6403() `assertEq!` 1i64


// 04
def f641(ch: Channel[Int64]): Channel[Int64] =
    ch <- 1i64

def f642(to: Channel[Int64], through: Channel[Int64], from: Channel[Int64]): Channel[Int64] =
    to <- <- through <- <- from

def putChannelInt6404(): Int64 =
    let ch1 = channel Int64 1;
    let ch2 = channel Int64 1;
    let ch3 = channel Int64 1;
    let _ = spawn f641(ch1);
    let _ = spawn f641(ch1);
    let _ = spawn f642(ch3, ch2, ch1);
    let _ = spawn f642(ch3, ch2, ch1);
    let _ = <- ch3;
    <- ch3

@test
def testPutChannelInt6404(): Bool = putChannelInt6404() `assertEq!` 1i64


// 05
def f651(ch: Channel[Int64], v1: Int64, v2: Int64): Channel[Int64] =
    ch <- v1 <- v2

def putChannelInt6405(): Int64 =
    let ch = channel Int64 1;
    let _ = spawn f651(ch, 1i64, 1i64);
    let _ = <- ch;
    <- ch

@test
def testPutChannelInt6405(): Bool = putChannelInt6405() `assertEq!` 1i64



// BigInt
// 01
def f711(ch: Channel[BigInt]): Channel[BigInt] =
    ch <- 1ii

def putChannelBigInt01(): BigInt =
    let ch = channel BigInt 1;
    let _ = spawn f711(ch);
    <- ch

@test
def testPutChannelBigInt01(): Bool = putChannelBigInt01() `assertEq!` 1ii


// 02
def f721(ch: Channel[BigInt]): Channel[BigInt] =
    ch <- 1ii

def f722(to: Channel[BigInt], from: Channel[BigInt]): Channel[BigInt] =
    to <- <- from

def putChannelBigInt02(): BigInt =
    let ch1 = channel BigInt 1;
    let ch2 = channel BigInt 1;
    let _ = spawn f721(ch1);
    let _ = spawn f722(ch2, ch1);
    <- ch2

@test
def testPutChannelBigInt02(): Bool = putChannelBigInt02() `assertEq!` 1ii


// 03
def f731(ch: Channel[BigInt]): Channel[BigInt] =
    ch <- 1ii

def f732(to: Channel[BigInt], from: Channel[BigInt]): Channel[BigInt] =
    to <- <- from

def putChannelBigInt03(): BigInt =
    let ch1 = channel BigInt 1;
    let ch2 = channel BigInt 1;
    let ch3 = channel BigInt 1;
    let _ = spawn f731(ch1);
    let _ = spawn f731(ch1);
    let _ = spawn f732(ch2, ch1);
    let _ = spawn f732(ch2, ch1);
    let _ = spawn f732(ch3, ch2);
    let _ = <- ch2;
    <- ch3

@test
def testPutChannelBigInt03(): Bool = putChannelBigInt03() `assertEq!` 1ii


// 04
def f741(ch: Channel[BigInt]): Channel[BigInt] =
    ch <- 1ii

def f742(to: Channel[BigInt], through: Channel[BigInt], from: Channel[BigInt]): Channel[BigInt] =
    to <- <- through <- <- from

def putChannelBigInt04(): BigInt =
    let ch1 = channel BigInt 1;
    let ch2 = channel BigInt 1;
    let ch3 = channel BigInt 1;
    let _ = spawn f741(ch1);
    let _ = spawn f741(ch1);
    let _ = spawn f742(ch3, ch2, ch1);
    let _ = spawn f742(ch3, ch2, ch1);
    let _ = <- ch3;
    <- ch3

@test
def testPutChannelBigInt04(): Bool = putChannelBigInt04() `assertEq!` 1ii


// 05
def f751(ch: Channel[BigInt], v1: BigInt, v2: BigInt): Channel[BigInt] =
    ch <- v1 <- v2

def putChannelBigInt05(): BigInt =
    let ch = channel BigInt 1;
    let _ = spawn f751(ch, 1ii, 1ii);
    let _ = <- ch;
    <- ch

@test
def testPutChannelBigInt05(): Bool = putChannelBigInt05() `assertEq!` 1ii



// Char
// 01
def f811(ch: Channel[Char]): Channel[Char] =
    ch <- 'a'

def putChannelChar01(): Char =
    let ch = channel Char 1;
    let _ = spawn f811(ch);
    <- ch

@test
def testPutChannelChar01(): Bool = putChannelChar01() `assertEq!` 'a'


// 02
def f821(ch: Channel[Char]): Channel[Char] =
    ch <- 'a'

def f822(to: Channel[Char], from: Channel[Char]): Channel[Char] =
    to <- <- from

def putChannelChar02(): Char =
    let ch1 = channel Char 1;
    let ch2 = channel Char 1;
    let _ = spawn f821(ch1);
    let _ = spawn f822(ch2, ch1);
    <- ch2

@test
def testPutChannelChar02(): Bool = putChannelChar02() `assertEq!` 'a'


// 03
def f831(ch: Channel[Char]): Channel[Char] =
    ch <- 'a'

def f832(to: Channel[Char], from: Channel[Char]): Channel[Char] =
    to <- <- from

def putChannelChar03(): Char =
    let ch1 = channel Char 1;
    let ch2 = channel Char 1;
    let ch3 = channel Char 1;
    let _ = spawn f831(ch1);
    let _ = spawn f831(ch1);
    let _ = spawn f832(ch2, ch1);
    let _ = spawn f832(ch2, ch1);
    let _ = spawn f832(ch3, ch2);
    let _ = <- ch2;
    <- ch3

@test
def testPutChannelChar03(): Bool = putChannelChar03() `assertEq!` 'a'


// 04
def f841(ch: Channel[Char]): Channel[Char] =
    ch <- 'a'

def f842(to: Channel[Char], through: Channel[Char], from: Channel[Char]): Channel[Char] =
    to <- <- through <- <- from

def putChannelChar04(): Char =
    let ch1 = channel Char 1;
    let ch2 = channel Char 1;
    let ch3 = channel Char 1;
    let _ = spawn f841(ch1);
    let _ = spawn f841(ch1);
    let _ = spawn f842(ch3, ch2, ch1);
    let _ = spawn f842(ch3, ch2, ch1);
    let _ = <- ch3;
    <- ch3

@test
def testPutChannelChar04(): Bool = putChannelChar04() `assertEq!` 'a'


// 05
def f851(ch: Channel[Char], v1: Char, v2: Char): Channel[Char] =
    ch <- v1 <- v2

def putChannelChar05(): Char =
    let ch = channel Char 1;
    let _ = spawn f851(ch, 'a', 'a');
    let _ = <- ch;
    <- ch

@test
def testPutChannelChar05(): Bool = putChannelChar05() `assertEq!` 'a'



// Float32
// 01
def f911(ch: Channel[Float32]): Channel[Float32] =
    ch <- 1.0f32

def putChannelFloat3201(): Float32 =
    let ch = channel Float32 1;
    let _ = spawn f911(ch);
    <- ch

@test
def testPutChannelFloat3201(): Bool = putChannelFloat3201() `assertEq!` 1.0f32


// 02
def f921(ch: Channel[Float32]): Channel[Float32] =
    ch <- 1.0f32

def f922(to: Channel[Float32], from: Channel[Float32]): Channel[Float32] =
    to <- <- from

def putChannelFloat3202(): Float32 =
    let ch1 = channel Float32 1;
    let ch2 = channel Float32 1;
    let _ = spawn f921(ch1);
    let _ = spawn f922(ch2, ch1);
    <- ch2

@test
def testPutChannelFloat3202(): Bool = putChannelFloat3202() `assertEq!` 1.0f32


// 03
def f931(ch: Channel[Float32]): Channel[Float32] =
    ch <- 1.0f32

def f932(to: Channel[Float32], from: Channel[Float32]): Channel[Float32] =
    to <- <- from

def putChannelFloat3203(): Float32 =
    let ch1 = channel Float32 1;
    let ch2 = channel Float32 1;
    let ch3 = channel Float32 1;
    let _ = spawn f931(ch1);
    let _ = spawn f931(ch1);
    let _ = spawn f932(ch2, ch1);
    let _ = spawn f932(ch2, ch1);
    let _ = spawn f932(ch3, ch2);
    let _ = <- ch2;
    <- ch3

@test
def testPutChannelFloat3203(): Bool = putChannelFloat3203() `assertEq!` 1.0f32


// 04
def f941(ch: Channel[Float32]): Channel[Float32] =
    ch <- 1.0f32

def f942(to: Channel[Float32], through: Channel[Float32], from: Channel[Float32]): Channel[Float32] =
    to <- <- through <- <- from

def putChannelFloat3204(): Float32 =
    let ch1 = channel Float32 1;
    let ch2 = channel Float32 1;
    let ch3 = channel Float32 1;
    let _ = spawn f941(ch1);
    let _ = spawn f941(ch1);
    let _ = spawn f942(ch3, ch2, ch1);
    let _ = spawn f942(ch3, ch2, ch1);
    let _ = <- ch3;
    <- ch3

@test
def testPutChannelFloat3204(): Bool = putChannelFloat3204() `assertEq!` 1.0f32


// 05
def f951(ch: Channel[Float32], v1: Float32, v2: Float32): Channel[Float32] =
    ch <- v1 <- v2

def putChannelFloat3205(): Float32 =
    let ch = channel Float32 1;
    let _ = spawn f951(ch, 1.0f32, 1.0f32);
    let _ = <- ch;
    <- ch

@test
def testPutChannelFloat3205(): Bool = putChannelFloat3205() `assertEq!` 1.0f32



// Float64
// 01
def f1011(ch: Channel[Float64]): Channel[Float64] =
    ch <- 1.0f64

def putChannelFloat6401(): Float64 =
    let ch = channel Float64 1;
    let _ = spawn f1011(ch);
    <- ch

@test
def testPutChannelFloat6401(): Bool = putChannelFloat6401() `assertEq!` 1.0f64


// 02
def f1021(ch: Channel[Float64]): Channel[Float64] =
    ch <- 1.0f64

def f1022(to: Channel[Float64], from: Channel[Float64]): Channel[Float64] =
    to <- <- from

def putChannelFloat6402(): Float64 =
    let ch1 = channel Float64 1;
    let ch2 = channel Float64 1;
    let _ = spawn f1021(ch1);
    let _ = spawn f1022(ch2, ch1);
    <- ch2

@test
def testPutChannelFloat6402(): Bool = putChannelFloat6402() `assertEq!` 1.0f64


// 03
def f1031(ch: Channel[Float64]): Channel[Float64] =
    ch <- 1.0f64

def f1032(to: Channel[Float64], from: Channel[Float64]): Channel[Float64] =
    to <- <- from

def putChannelFloat6403(): Float64 =
    let ch1 = channel Float64 1;
    let ch2 = channel Float64 1;
    let ch3 = channel Float64 1;
    let _ = spawn f1031(ch1);
    let _ = spawn f1031(ch1);
    let _ = spawn f1032(ch2, ch1);
    let _ = spawn f1032(ch2, ch1);
    let _ = spawn f1032(ch3, ch2);
    let _ = <- ch2;
    <- ch3

@test
def testPutChannelFloat6403(): Bool = putChannelFloat6403() `assertEq!` 1.0f64


// 04
def f1041(ch: Channel[Float64]): Channel[Float64] =
    ch <- 1.0f64

def f1042(to: Channel[Float64], through: Channel[Float64], from: Channel[Float64]): Channel[Float64] =
    to <- <- through <- <- from

def putChannelFloat6404(): Float64 =
    let ch1 = channel Float64 1;
    let ch2 = channel Float64 1;
    let ch3 = channel Float64 1;
    let _ = spawn f1041(ch1);
    let _ = spawn f1041(ch1);
    let _ = spawn f1042(ch3, ch2, ch1);
    let _ = spawn f1042(ch3, ch2, ch1);
    let _ = <- ch3;
    <- ch3

@test
def testPutChannelFloat6404(): Bool = putChannelFloat6404() `assertEq!` 1.0f64


// 05
def f1051(ch: Channel[Float64], v1: Float64, v2: Float64): Channel[Float64] =
    ch <- v1 <- v2

def putChannelFloat6405(): Float64 =
    let ch = channel Float64 1;
    let _ = spawn f1051(ch, 1.0f64, 1.0f64);
    let _ = <- ch;
    <- ch

@test
def testPutChannelFloat6405(): Bool = putChannelFloat6405() `assertEq!` 1.0f64



// Str
// 01
def f1111(ch: Channel[Str]): Channel[Str] =
    ch <- "a"

def putChannelStr01(): Str =
    let ch = channel Str 1;
    let _ = spawn f1111(ch);
    <- ch

@test
def testPutChannelStr01(): Bool = putChannelStr01() `assertEq!` "a"


// 02
def f1121(ch: Channel[Str]): Channel[Str] =
    ch <- "a"

def f1122(to: Channel[Str], from: Channel[Str]): Channel[Str] =
    to <- <- from

def putChannelStr02(): Str =
    let ch1 = channel Str 1;
    let ch2 = channel Str 1;
    let _ = spawn f1121(ch1);
    let _ = spawn f1122(ch2, ch1);
    <- ch2

@test
def testPutChannelStr02(): Bool = putChannelStr02() `assertEq!` "a"


// 03
def f1131(ch: Channel[Str]): Channel[Str] =
    ch <- "a"

def f1132(to: Channel[Str], from: Channel[Str]): Channel[Str] =
    to <- <- from

def putChannelStr03(): Str =
    let ch1 = channel Str 1;
    let ch2 = channel Str 1;
    let ch3 = channel Str 1;
    let _ = spawn f1131(ch1);
    let _ = spawn f1131(ch1);
    let _ = spawn f1132(ch2, ch1);
    let _ = spawn f1132(ch2, ch1);
    let _ = spawn f1132(ch3, ch2);
    let _ = <- ch2;
    <- ch3

@test
def testPutChannelStr03(): Bool = putChannelStr03() `assertEq!` "a"


// 04
def f1141(ch: Channel[Str]): Channel[Str] =
    ch <- "a"

def f1142(to: Channel[Str], through: Channel[Str], from: Channel[Str]): Channel[Str] =
    to <- <- through <- <- from

def putChannelStr04(): Str =
    let ch1 = channel Str 1;
    let ch2 = channel Str 1;
    let ch3 = channel Str 1;
    let _ = spawn f1141(ch1);
    let _ = spawn f1141(ch1);
    let _ = spawn f1142(ch3, ch2, ch1);
    let _ = spawn f1142(ch3, ch2, ch1);
    let _ = <- ch3;
    <- ch3

@test
def testPutChannelStr04(): Bool = putChannelStr04() `assertEq!` "a"


// 05
def f1151(ch: Channel[Str], v1: Str, v2: Str): Channel[Str] =
    ch <- v1 <- v2

def putChannelStr05(): Str =
    let ch = channel Str 1;
    let _ = spawn f1151(ch, "a", "a");
    let _ = <- ch;
    <- ch

@test
def testPutChannelStr05(): Bool = putChannelStr05() `assertEq!` "a"



// Channel Int
// 01

def f1211(ch: Channel[Int]): Channel[Int] =
    ch <- 1

def f1212(ch1: Channel[Channel[Int]], ch2: Channel[Int]): Channel[Channel[Int]] =
    ch1 <- ch2

def putChannelChannelInt01(): Int =
    let ch1 = channel Channel[Int] 1;
    let ch2 = channel Int 1;
    let _ = spawn f1211(ch2);
    let _ = spawn f1212(ch1, ch2);
    <- <- ch1

@test
def testputChannelChannelInt01(): Bool = putChannelChannelInt01() `assertEq!` 1


// 02
def f1221(ch: Channel[Int]): Channel[Int] =
    ch <- 1

def f1222(ch1: Channel[Channel[Int]], ch2: Channel[Int]): Channel[Channel[Int]] =
    ch1 <- ch2

def f1223(to: Channel[Channel[Int]], from: Channel[Channel[Int]]): Channel[Channel[Int]] =
    to <- <- from

def putChannelChannelInt02(): Int =
    let ch1 = channel Channel[Int] 1;
    let ch2 = channel Channel[Int] 1;
    let ch3 = channel Int 1;
    let _ = spawn f1221(ch3);
    let _ = spawn f1222(ch2, ch3);
    let _ = spawn f1223(ch1, ch2);
    <- <- ch1

@test
def testputChannelChannelInt02(): Bool = putChannelChannelInt02() `assertEq!` 1


// 03
def f1231(ch: Channel[Int]): Channel[Int] =
    ch <- 1

def f1232(ch1: Channel[Channel[Int]], ch2: Channel[Int]): Channel[Channel[Int]] =
    ch1 <- ch2

def f1233(to: Channel[Channel[Int]], from: Channel[Channel[Int]]): Channel[Channel[Int]] =
    to <- <- from

def putChannelChannelInt03(): Int =
    let ch1 = channel Channel[Int] 1;
    let ch2 = channel Channel[Int] 1;
    let ch3 = channel Channel[Int] 1;
    let ch4 = channel Int 1;
    let ch5 = channel Int 1;
    let _ = spawn f1231(ch4);
    let _ = spawn f1231(ch5);
    let _ = spawn f1232(ch3, ch4);
    let _ = spawn f1232(ch3, ch5);
    let _ = spawn f1233(ch2, ch3);
    let _ = spawn f1233(ch2, ch3);
    let _ = spawn f1233(ch1, ch2);
    let _ = <- ch2;
    <- <- ch1

@test
def testputChannelChannelInt03(): Bool = putChannelChannelInt03() `assertEq!` 1


// 04
def f1241(ch: Channel[Int]): Channel[Int] =
    ch <- 1

def f1242(ch1: Channel[Channel[Int]], ch2: Channel[Int]): Channel[Channel[Int]] =
    ch1 <- ch2

def f1243(to: Channel[Channel[Int]], through: Channel[Channel[Int]], from: Channel[Channel[Int]]): Channel[Channel[Int]] =
    to <- <- through <- <- from

def putChannelChannelInt04(): Int =
    let ch1 = channel Channel[Int] 1;
    let ch2 = channel Channel[Int] 1;
    let ch3 = channel Channel[Int] 1;
    let ch4 = channel Int 1;
    let ch5 = channel Int 1;
    let _ = spawn f1241(ch4);
    let _ = spawn f1241(ch5);
    let _ = spawn f1242(ch3, ch4);
    let _ = spawn f1242(ch3, ch5);
    let _ = spawn f1243(ch1, ch2, ch3);
    let _ = spawn f1243(ch1, ch2, ch3);
    let _ = <- ch1;
    <- <- ch1

@test
def testputChannelChannelInt04(): Bool = putChannelChannelInt04() `assertEq!` 1


// 05
def f1251(ch: Channel[Int]): Channel[Int] =
    ch <- 1

def f1252(ch: Channel[Channel[Int]], v1: Channel[Int], v2: Channel[Int]): Channel[Channel[Int]] =
    ch <- v1 <- v2

def putChannelChannelInt05(): Int =
    let ch1 = channel Channel[Int] 1;
    let ch2 = channel Int 1;
    let ch3 = channel Int 1;
    let _ = spawn f1251(ch2);
    let _ = spawn f1251(ch3);
    let _ = spawn f1252(ch1, ch2, ch3);
    <- <- ch1

@test
def testputChannelChannelInt05(): Bool = putChannelChannelInt05() `assertEq!` 1



// List
// 01
def f1311(ch: Channel[List[Int]]): Channel[List[Int]] =
    ch <- 1 :: Nil

def putChannelList01(): List[Int] =
    let ch = channel List[Int] 1;
    let _ = spawn f1311(ch);
    <- ch

@test
def testPutChannelList01(): Bool = putChannelList01() `assertEq!` 1 :: Nil


// 02
def f1321(ch: Channel[List[Int]]): Channel[List[Int]] =
    ch <- 1 :: Nil

def f1322(to: Channel[List[Int]], from: Channel[List[Int]]): Channel[List[Int]] =
    to <- <- from

def putChannelList02(): List[Int] =
    let ch1 = channel List[Int] 1;
    let ch2 = channel List[Int] 1;
    let _ = spawn f1321(ch1);
    let _ = spawn f1322(ch2, ch1);
    <- ch2

@test
def testPutChannelList02(): Bool = putChannelList02() `assertEq!` 1 :: Nil


// 03
def f1331(ch: Channel[List[Int]]): Channel[List[Int]] =
    ch <- 1 :: Nil

def f1332(to: Channel[List[Int]], from: Channel[List[Int]]): Channel[List[Int]] =
    to <- <- from

def putChannelList03(): List[Int] =
    let ch1 = channel List[Int] 1;
    let ch2 = channel List[Int] 1;
    let ch3 = channel List[Int] 1;
    let _ = spawn f1331(ch1);
    let _ = spawn f1331(ch1);
    let _ = spawn f1332(ch2, ch1);
    let _ = spawn f1332(ch2, ch1);
    let _ = spawn f1332(ch3, ch2);
    let _ = <- ch2;
    <- ch3

@test
def testPutChannelList03(): Bool = putChannelList03() `assertEq!` 1 :: Nil


// 04
def f1341(ch: Channel[List[Int]]): Channel[List[Int]] =
    ch <- 1 :: Nil

def f1342(to: Channel[List[Int]], through: Channel[List[Int]], from: Channel[List[Int]]): Channel[List[Int]] =
    to <- <- through <- <- from

def putChannelList04(): List[Int] =
    let ch1 = channel List[Int] 1;
    let ch2 = channel List[Int] 1;
    let ch3 = channel List[Int] 1;
    let _ = spawn f1341(ch1);
    let _ = spawn f1341(ch1);
    let _ = spawn f1342(ch3, ch2, ch1);
    let _ = spawn f1342(ch3, ch2, ch1);
    let _ = <- ch3;
    <- ch3

@test
def testPutChannelList04(): Bool = putChannelList04() `assertEq!` 1 :: Nil


// 05
def f1351(ch: Channel[List[Int]], v1: List[Int], v2: List[Int]): Channel[List[Int]] =
    ch <- v1 <- v2

def putChannelList05(): List[Int] =
    let ch = channel List[Int] 1;
    let _ = spawn f1351(ch, 1 :: Nil, 1 :: Nil);
    let _ = <- ch;
    <- ch

@test
def testPutChannelList05(): Bool = putChannelList05() `assertEq!` 1 :: Nil