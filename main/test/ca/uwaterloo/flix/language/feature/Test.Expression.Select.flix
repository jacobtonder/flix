/////////////////////////////////////////////////////////////////////////////
// Select expressions (channel select)                                     //
/////////////////////////////////////////////////////////////////////////////

def main(): Int = 42

// Test integers
def fi01(): Int = select {
    case _ = <- ch => 0
}

test@
def testSelectWildcardi01(): Bool = assertEq!(fi01(), 0)

def fi02(): Int = select {
    case _ = <- ch => -1
}

test@
def testSelectWildcardi02(): Bool = assertEq!(fi02(), -1)

def fi03(): Int = select {
    case _ = <- ch => 1
}

test@
def testSelectWildcardi03(): Bool = assertEq!(fi03(), 1)

def fi04(): Int = select {
    case _ = <- ch => -100
}

test@
def testSelectWildcardi04(): Bool = assertEq!(fi04(), -100)

def fi05(): Int = select {
    case _ = <- ch => 100
}

test@
def testSelectWildcardi05(): Bool = assertEq!(fi05(), 100)

// Test floats
def f0s1(): Float32 = select {
    case _ = <- ch => 0f32
}

test@
def testSelectWildcards01(): Bool = assertEq!(f0s1(), 0f32)

def f0s2(): Float32 = select {
    case _ = <- ch => -1.1f32
}

test@
def testSelectWildcards02(): Bool = assertEq!(f0s2(), -1.1f32)

def f0s3(): Float32 = select {
    case _ = <- ch => 1.1f32
}

test@
def testSelectWildcards03(): Bool = assertEq!(f0s3(), 1.1f32)

// Test strings
def f0s1(): Str = select {
    case _ = <- ch => ""
}

test@
def testSelectWildcards01(): Bool = assertEq!(f0s1(), "")

def f0s2(): Str = select {
    case _ = <- ch => "str"
}

test@
def testSelectWildcards02(): Bool = assertEq!(f0s2(), "str")

def f0s3(): Str = select {
    case _ = <- ch => "strstrstrstrstrstrstrstrstr"
}

test@
def testSelectWildcards03(): Bool = assertEq!(f0s3(), "strstrstrstrstrstrstrstrstr")
