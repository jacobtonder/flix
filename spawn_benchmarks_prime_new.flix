def isPrime(n: Int, i: Int, w: Int): Bool =
    if (n == 2)
        true
    else if (n == 3)
        true
    else if (n % 2 == 0)
        false
    else if (n % 3 == 0)
        false
    else if (i * i <= n)
        if(n % i == 0)
            false
        else
            let i = i + w;
            let w = 6 - w;
            isPrime(n, i, w)
    else
        true

def prime(n: Int, x: Int): Bool =
    let b = isPrime(n, 5, 2);
    if(x > 0) prime(n, x - 1) else b

def applyPrimeToList(list: List[Int]): List[Int] =
    List.filter((x) -> prime(x, 1), list)

def spawnPrimeLists(list: List[List[Int]], outCh: Channel[List[List[Int]]]): Int =
    let primedList = List.map((x) -> applyPrimeToList(x), list);
    let _ = outCh <- primedList;
    1

def seqPrimeLists(list: List[List[Int]]): List[List[Int]] =
    List.map((x) -> applyPrimeToList(x), list)




// Sequential
/*
@benchmark
def primeP1Seq(): Unit =
    let lNum = 2**1;
    let lSize = 2**12;
    let n = lSize * lNum;
    let ch = channel List[List[Int]] 1;
    let list1 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let _ = spawnPrimeLists(list1, ch);
    let _ = <- ch;
    ()

@benchmark
def primeP2T1(): Unit =
    let lNum = 2**2;
    let lSize = 2**12;
    let n = lSize * lNum;
    let ch = channel List[List[Int]] 1;
    let list1 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let _ = spawn spawnPrimeLists(list1, ch);
    let _ = <- ch;
    ()

@benchmark
def primeP3Seq(): Unit =
    let lNum = 2**3;
    let lSize = 2**12;
    let n = lSize * lNum;
    let ch = channel List[List[Int]] 1;
    let list1 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let _ = spawnPrimeLists(list1, ch);
    let _ = <- ch;
    ()

@benchmark
def primeP4Seq(): Unit =
    let lNum = 2**4;
    let lSize = 2**12;
    let n = lSize * lNum;
    let ch = channel List[List[Int]] 1;
    let list1 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let _ = spawnPrimeLists(list1, ch);
    let _ = <- ch;
    ()

@benchmark
def primeP5Seq(): Unit =
    let lNum = 2**5;
    let lSize = 2**12;
    let n = lSize * lNum;
    let ch = channel List[List[Int]] 1;
    let list1 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let _ = spawnPrimeLists(list1, ch);
    let _ = <- ch;
    ()

@benchmark
def primeP6Seq(): Unit =
    let lNum = 2**6;
    let lSize = 2**12;
    let n = lSize * lNum;
    let ch = channel List[List[Int]] 1;
    let list1 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let _ = spawnPrimeLists(list1, ch);
    let _ = <- ch;
    ()

@benchmark
def primeP7Seq(): Unit =
    let lNum = 2**7;
    let lSize = 2**12;
    let n = lSize * lNum;
    let ch = channel List[List[Int]] 1;
    let list1 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let _ = spawnPrimeLists(list1, ch);
    let _ = <- ch;
    ()

@benchmark
def primeP8Seq(): Unit =
    let lNum = 2**8;
    let lSize = 2**12;
    let n = lSize * lNum;
    let ch = channel List[List[Int]] 1;
    let list1 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let _ = spawnPrimeLists(list1, ch);
    let _ = <- ch;
    ()

@benchmark
def prime91Seq(): Unit =
    let lNum = 2**9;
    let lSize = 2**12;
    let n = lSize * lNum;
    let ch = channel List[List[Int]] 1;
    let list1 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let _ = spawnPrimeLists(list1, ch);
    let _ = <- ch;
    ()

@benchmark
def primeP10Seq(): Unit =
    let lNum = 2**10;
    let lSize = 2**12;
    let n = lSize * lNum;
    let ch = channel List[List[Int]] 1;
    let list1 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let _ = spawnPrimeLists(list1, ch);
    let _ = <- ch;
    ()

@benchmark
def primeP11Seq(): Unit =
    let lNum = 2**11;
    let lSize = 2**12;
    let n = lSize * lNum;
    let ch = channel List[List[Int]] 1;
    let list1 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let _ = spawnPrimeLists(list1, ch);
    let _ = <- ch;
    ()

@benchmark
def primeP12Seq(): Unit =
    let lNum = 2**12;
    let lSize = 2**12;
    let n = lSize * lNum;
    let ch = channel List[List[Int]] 1;
    let list1 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let _ = spawnPrimeLists(list1, ch);
    let _ = <- ch;
    ()
*/


// 1 Thread
/*
@benchmark
def primeP1T1(): Unit =
    let lNum = 2**1;
    let lSize = 2**12;
    let n = lSize * lNum;
    let ch = channel List[List[Int]] 1;
    let list1 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let _ = spawn spawnPrimeLists(list1, ch);
    let _ = <- ch;
    ()

@benchmark
def primeP2T1(): Unit =
    let lNum = 2**2;
    let lSize = 2**12;
    let n = lSize * lNum;
    let ch = channel List[List[Int]] 1;
    let list1 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let _ = spawn spawnPrimeLists(list1, ch);
    let _ = <- ch;
    ()

@benchmark
def primeP3T1(): Unit =
    let lNum = 2**3;
    let lSize = 2**12;
    let n = lSize * lNum;
    let ch = channel List[List[Int]] 1;
    let list1 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let _ = spawn spawnPrimeLists(list1, ch);
    let _ = <- ch;
    ()

@benchmark
def primeP4T1(): Unit =
    let lNum = 2**4;
    let lSize = 2**12;
    let n = lSize * lNum;
    let ch = channel List[List[Int]] 1;
    let list1 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let _ = spawn spawnPrimeLists(list1, ch);
    let _ = <- ch;
    ()

@benchmark
def primeP5T1(): Unit =
    let lNum = 2**5;
    let lSize = 2**12;
    let n = lSize * lNum;
    let ch = channel List[List[Int]] 1;
    let list1 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let _ = spawn spawnPrimeLists(list1, ch);
    let _ = <- ch;
    ()

@benchmark
def primeP6T1(): Unit =
    let lNum = 2**6;
    let lSize = 2**12;
    let n = lSize * lNum;
    let ch = channel List[List[Int]] 1;
    let list1 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let _ = spawn spawnPrimeLists(list1, ch);
    let _ = <- ch;
    ()

@benchmark
def primeP7T1(): Unit =
    let lNum = 2**7;
    let lSize = 2**12;
    let n = lSize * lNum;
    let ch = channel List[List[Int]] 1;
    let list1 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let _ = spawn spawnPrimeLists(list1, ch);
    let _ = <- ch;
    ()

@benchmark
def primeP8T1(): Unit =
    let lNum = 2**8;
    let lSize = 2**12;
    let n = lSize * lNum;
    let ch = channel List[List[Int]] 1;
    let list1 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let _ = spawn spawnPrimeLists(list1, ch);
    let _ = <- ch;
    ()

@benchmark
def prime91T1(): Unit =
    let lNum = 2**9;
    let lSize = 2**12;
    let n = lSize * lNum;
    let ch = channel List[List[Int]] 1;
    let list1 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let _ = spawn spawnPrimeLists(list1, ch);
    let _ = <- ch;
    ()

@benchmark
def primeP10T1(): Unit =
    let lNum = 2**10;
    let lSize = 2**12;
    let n = lSize * lNum;
    let ch = channel List[List[Int]] 1;
    let list1 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let _ = spawn spawnPrimeLists(list1, ch);
    let _ = <- ch;
    ()

@benchmark
def primeP11T1(): Unit =
    let lNum = 2**11;
    let lSize = 2**12;
    let n = lSize * lNum;
    let ch = channel List[List[Int]] 1;
    let list1 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let _ = spawn spawnPrimeLists(list1, ch);
    let _ = <- ch;
    ()

@benchmark
def primeP12T1(): Unit =
    let lNum = 2**12;
    let lSize = 2**12;
    let n = lSize * lNum;
    let ch = channel List[List[Int]] 1;
    let list1 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let _ = spawn spawnPrimeLists(list1, ch);
    let _ = <- ch;
    ()


*/
// 2 Threads
/*
@benchmark
def primeP1T2(): Unit =
    let lNum = 2**1;
    let lSize = 2**12;
    let n = lSize * lNum;
    let ch = channel List[List[Int]] 2;
    let list1 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list2 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let _ = spawn spawnPrimeLists(list1, ch);
    let _ = spawn spawnPrimeLists(list2, ch);
    let _ = <- ch;
    let _ = <- ch;
    ()

@benchmark
def primeP2T2(): Unit =
    let lNum = 2**2;
    let lSize = 2**12;
    let n = lSize * lNum;
    let ch = channel List[List[Int]] 2;
    let list1 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list2 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let _ = spawn spawnPrimeLists(list1, ch);
    let _ = spawn spawnPrimeLists(list2, ch);
    let _ = <- ch;
    let _ = <- ch;
    ()

@benchmark
def primeP3T2(): Unit =
    let lNum = 2**3;
    let lSize = 2**12;
    let n = lSize * lNum;
    let ch = channel List[List[Int]] 2;
    let list1 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list2 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let _ = spawn spawnPrimeLists(list1, ch);
    let _ = spawn spawnPrimeLists(list2, ch);
    let _ = <- ch;
    let _ = <- ch;
    ()

@benchmark
def primeP4T2(): Unit =
    let lNum = 2**4;
    let lSize = 2**12;
    let n = lSize * lNum;
    let ch = channel List[List[Int]] 2;
    let list1 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list2 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let _ = spawn spawnPrimeLists(list1, ch);
    let _ = spawn spawnPrimeLists(list2, ch);
    let _ = <- ch;
    let _ = <- ch;
    ()

@benchmark
def primeP5T2(): Unit =
    let lNum = 2**5;
    let lSize = 2**12;
    let n = lSize * lNum;
    let ch = channel List[List[Int]] 2;
    let list1 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list2 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let _ = spawn spawnPrimeLists(list1, ch);
    let _ = spawn spawnPrimeLists(list2, ch);
    let _ = <- ch;
    let _ = <- ch;
    ()

@benchmark
def primeP6T2(): Unit =
    let lNum = 2**6;
    let lSize = 2**12;
    let n = lSize * lNum;
    let ch = channel List[List[Int]] 2;
    let list1 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list2 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let _ = spawn spawnPrimeLists(list1, ch);
    let _ = spawn spawnPrimeLists(list2, ch);
    let _ = <- ch;
    let _ = <- ch;
    ()

@benchmark
def primeP7T2(): Unit =
    let lNum = 2**7;
    let lSize = 2**12;
    let n = lSize * lNum;
    let ch = channel List[List[Int]] 2;
    let list1 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list2 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let _ = spawn spawnPrimeLists(list1, ch);
    let _ = spawn spawnPrimeLists(list2, ch);
    let _ = <- ch;
    let _ = <- ch;
    ()

@benchmark
def primeP8T2(): Unit =
    let lNum = 2**8;
    let lSize = 2**12;
    let n = lSize * lNum;
    let ch = channel List[List[Int]] 2;
    let list1 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list2 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let _ = spawn spawnPrimeLists(list1, ch);
    let _ = spawn spawnPrimeLists(list2, ch);
    let _ = <- ch;
    let _ = <- ch;
    ()

@benchmark
def primeP9T2(): Unit =
    let lNum = 2**9;
    let lSize = 2**12;
    let n = lSize * lNum;
    let ch = channel List[List[Int]] 2;
    let list1 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list2 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let _ = spawn spawnPrimeLists(list1, ch);
    let _ = spawn spawnPrimeLists(list2, ch);
    let _ = <- ch;
    let _ = <- ch;
    ()

@benchmark
def primeP10T2(): Unit =
    let lNum = 2**10;
    let lSize = 2**12;
    let n = lSize * lNum;
    let ch = channel List[List[Int]] 2;
    let list1 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list2 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let _ = spawn spawnPrimeLists(list1, ch);
    let _ = spawn spawnPrimeLists(list2, ch);
    let _ = <- ch;
    let _ = <- ch;
    ()

@benchmark
def primeP11T2(): Unit =
    let lNum = 2**11;
    let lSize = 2**12;
    let n = lSize * lNum;
    let ch = channel List[List[Int]] 2;
    let list1 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list2 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let _ = spawn spawnPrimeLists(list1, ch);
    let _ = spawn spawnPrimeLists(list2, ch);
    let _ = <- ch;
    let _ = <- ch;
    ()

@benchmark
def primeP12T2(): Unit =
    let lNum = 2**12;
    let lSize = 2**12;
    let n = lSize * lNum;
    let ch = channel List[List[Int]] 2;
    let list1 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list2 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let _ = spawn spawnPrimeLists(list1, ch);
    let _ = spawn spawnPrimeLists(list2, ch);
    let _ = <- ch;
    let _ = <- ch;
    ()
*/


// 3 Threads
/*
@benchmark
def primeP1T3(): Unit =
    let lNum = 2**1;
    let lSize = 2**12;
    let n = lSize * lNum;
    let ch = channel List[List[Int]] 3;
    let list1 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list2 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list3 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let _ = spawn spawnPrimeLists(list1, ch);
    let _ = spawn spawnPrimeLists(list2, ch);
    let _ = spawn spawnPrimeLists(list3, ch);
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    ()

@benchmark
def primeP2T3(): Unit =
    let lNum = 2**2;
    let lSize = 2**12;
    let n = lSize * lNum;
    let ch = channel List[List[Int]] 3;
    let list1 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list2 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list3 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let _ = spawn spawnPrimeLists(list1, ch);
    let _ = spawn spawnPrimeLists(list2, ch);
    let _ = spawn spawnPrimeLists(list3, ch);
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    ()

@benchmark
def primeP3T3(): Unit =
    let lNum = 2**3;
    let lSize = 2**12;
    let n = lSize * lNum;
    let ch = channel List[List[Int]] 3;
    let list1 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list2 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list3 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let _ = spawn spawnPrimeLists(list1, ch);
    let _ = spawn spawnPrimeLists(list2, ch);
    let _ = spawn spawnPrimeLists(list3, ch);
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    ()

@benchmark
def primeP4T3(): Unit =
    let lNum = 2**4;
    let lSize = 2**12;
    let n = lSize * lNum;
    let ch = channel List[List[Int]] 3;
    let list1 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list2 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list3 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let _ = spawn spawnPrimeLists(list1, ch);
    let _ = spawn spawnPrimeLists(list2, ch);
    let _ = spawn spawnPrimeLists(list3, ch);
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    ()

@benchmark
def primeP5T3(): Unit =
    let lNum = 2**5;
    let lSize = 2**12;
    let n = lSize * lNum;
    let ch = channel List[List[Int]] 3;
    let list1 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list2 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list3 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let _ = spawn spawnPrimeLists(list1, ch);
    let _ = spawn spawnPrimeLists(list2, ch);
    let _ = spawn spawnPrimeLists(list3, ch);
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    ()

@benchmark
def primeP6T3(): Unit =
    let lNum = 2**6;
    let lSize = 2**12;
    let n = lSize * lNum;
    let ch = channel List[List[Int]] 3;
    let list1 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list2 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list3 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let _ = spawn spawnPrimeLists(list1, ch);
    let _ = spawn spawnPrimeLists(list2, ch);
    let _ = spawn spawnPrimeLists(list3, ch);
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    ()

@benchmark
def primeP7T3(): Unit =
    let lNum = 2**7;
    let lSize = 2**12;
    let n = lSize * lNum;
    let ch = channel List[List[Int]] 3;
    let list1 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list2 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list3 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let _ = spawn spawnPrimeLists(list1, ch);
    let _ = spawn spawnPrimeLists(list2, ch);
    let _ = spawn spawnPrimeLists(list3, ch);
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    ()

@benchmark
def primeP8T3(): Unit =
    let lNum = 2**8;
    let lSize = 2**12;
    let n = lSize * lNum;
    let ch = channel List[List[Int]] 3;
    let list1 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list2 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list3 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let _ = spawn spawnPrimeLists(list1, ch);
    let _ = spawn spawnPrimeLists(list2, ch);
    let _ = spawn spawnPrimeLists(list3, ch);
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    ()

@benchmark
def primeP9T3(): Unit =
    let lNum = 2**9;
    let lSize = 2**12;
    let n = lSize * lNum;
    let ch = channel List[List[Int]] 3;
    let list1 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list2 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list3 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let _ = spawn spawnPrimeLists(list1, ch);
    let _ = spawn spawnPrimeLists(list2, ch);
    let _ = spawn spawnPrimeLists(list3, ch);
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    ()

@benchmark
def primeP10T3(): Unit =
    let lNum = 2**10;
    let lSize = 2**12;
    let n = lSize * lNum;
    let ch = channel List[List[Int]] 3;
    let list1 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list2 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list3 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let _ = spawn spawnPrimeLists(list1, ch);
    let _ = spawn spawnPrimeLists(list2, ch);
    let _ = spawn spawnPrimeLists(list3, ch);
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    ()

@benchmark
def primeP11T3(): Unit =
    let lNum = 2**11;
    let lSize = 2**12;
    let n = lSize * lNum;
    let ch = channel List[List[Int]] 3;
    let list1 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list2 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list3 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let _ = spawn spawnPrimeLists(list1, ch);
    let _ = spawn spawnPrimeLists(list2, ch);
    let _ = spawn spawnPrimeLists(list3, ch);
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    ()

@benchmark
def primeP12T3(): Unit =
    let lNum = 2**12;
    let lSize = 2**12;
    let n = lSize * lNum;
    let ch = channel List[List[Int]] 3;
    let list1 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list2 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list3 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let _ = spawn spawnPrimeLists(list1, ch);
    let _ = spawn spawnPrimeLists(list2, ch);
    let _ = spawn spawnPrimeLists(list3, ch);
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    ()
*/



// 4 Threads
/*
@benchmark
def primeP1T4(): Unit =
    let lNum = 2**1;
    let lSize = 2**12;
    let n = lSize * lNum;
    let ch = channel List[List[Int]] 2;
    let list1 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list2 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list3 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list4 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let _ = spawn spawnPrimeLists(list1, ch);
    let _ = spawn spawnPrimeLists(list2, ch);
    let _ = spawn spawnPrimeLists(list3, ch);
    let _ = spawn spawnPrimeLists(list4, ch);
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    ()

@benchmark
def primeP2T4(): Unit =
    let lNum = 2**2;
    let lSize = 2**12;
    let n = lSize * lNum;
    let ch = channel List[List[Int]] 2;
    let list1 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list2 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list3 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list4 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let _ = spawn spawnPrimeLists(list1, ch);
    let _ = spawn spawnPrimeLists(list2, ch);
    let _ = spawn spawnPrimeLists(list3, ch);
    let _ = spawn spawnPrimeLists(list4, ch);
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    ()

@benchmark
def primeP3T4(): Unit =
    let lNum = 2**3;
    let lSize = 2**12;
    let n = lSize * lNum;
    let ch = channel List[List[Int]] 2;
    let list1 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list2 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list3 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list4 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let _ = spawn spawnPrimeLists(list1, ch);
    let _ = spawn spawnPrimeLists(list2, ch);
    let _ = spawn spawnPrimeLists(list3, ch);
    let _ = spawn spawnPrimeLists(list4, ch);
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    ()

@benchmark
def primeP4T4(): Unit =
    let lNum = 2**4;
    let lSize = 2**12;
    let n = lSize * lNum;
    let ch = channel List[List[Int]] 2;
    let list1 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list2 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list3 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list4 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let _ = spawn spawnPrimeLists(list1, ch);
    let _ = spawn spawnPrimeLists(list2, ch);
    let _ = spawn spawnPrimeLists(list3, ch);
    let _ = spawn spawnPrimeLists(list4, ch);
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    ()

@benchmark
def primeP5T4(): Unit =
    let lNum = 2**5;
    let lSize = 2**12;
    let n = lSize * lNum;
    let ch = channel List[List[Int]] 2;
    let list1 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list2 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list3 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list4 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let _ = spawn spawnPrimeLists(list1, ch);
    let _ = spawn spawnPrimeLists(list2, ch);
    let _ = spawn spawnPrimeLists(list3, ch);
    let _ = spawn spawnPrimeLists(list4, ch);
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    ()

@benchmark
def primeP6T4(): Unit =
    let lNum = 2**6;
    let lSize = 2**12;
    let n = lSize * lNum;
    let ch = channel List[List[Int]] 2;
    let list1 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list2 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list3 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list4 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let _ = spawn spawnPrimeLists(list1, ch);
    let _ = spawn spawnPrimeLists(list2, ch);
    let _ = spawn spawnPrimeLists(list3, ch);
    let _ = spawn spawnPrimeLists(list4, ch);
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    ()

@benchmark
def primeP7T4(): Unit =
    let lNum = 2**7;
    let lSize = 2**12;
    let n = lSize * lNum;
    let ch = channel List[List[Int]] 2;
    let list1 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list2 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list3 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list4 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let _ = spawn spawnPrimeLists(list1, ch);
    let _ = spawn spawnPrimeLists(list2, ch);
    let _ = spawn spawnPrimeLists(list3, ch);
    let _ = spawn spawnPrimeLists(list4, ch);
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    ()

@benchmark
def primeP8T4(): Unit =
    let lNum = 2**8;
    let lSize = 2**12;
    let n = lSize * lNum;
    let ch = channel List[List[Int]] 2;
    let list1 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list2 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list3 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list4 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let _ = spawn spawnPrimeLists(list1, ch);
    let _ = spawn spawnPrimeLists(list2, ch);
    let _ = spawn spawnPrimeLists(list3, ch);
    let _ = spawn spawnPrimeLists(list4, ch);
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    ()

@benchmark
def primeP9T4(): Unit =
    let lNum = 2**9;
    let lSize = 2**12;
    let n = lSize * lNum;
    let ch = channel List[List[Int]] 2;
    let list1 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list2 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list3 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list4 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let _ = spawn spawnPrimeLists(list1, ch);
    let _ = spawn spawnPrimeLists(list2, ch);
    let _ = spawn spawnPrimeLists(list3, ch);
    let _ = spawn spawnPrimeLists(list4, ch);
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    ()

@benchmark
def primeP10T4(): Unit =
    let lNum = 2**10;
    let lSize = 2**12;
    let n = lSize * lNum;
    let ch = channel List[List[Int]] 2;
    let list1 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list2 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list3 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list4 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let _ = spawn spawnPrimeLists(list1, ch);
    let _ = spawn spawnPrimeLists(list2, ch);
    let _ = spawn spawnPrimeLists(list3, ch);
    let _ = spawn spawnPrimeLists(list4, ch);
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    ()
*/


// 5 Threads
/*
@benchmark
def primeP1T5(): Unit =
    let lNum = 2**1;
    let lSize = 2**12;
    let n = lSize * lNum;
    let ch = channel List[List[Int]] 2;
    let list1 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list2 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list3 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list4 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list5 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let _ = spawn spawnPrimeLists(list1, ch);
    let _ = spawn spawnPrimeLists(list2, ch);
    let _ = spawn spawnPrimeLists(list3, ch);
    let _ = spawn spawnPrimeLists(list4, ch);
    let _ = spawn spawnPrimeLists(list5, ch);
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    ()

@benchmark
def primeP2T5(): Unit =
    let lNum = 2**2;
    let lSize = 2**12;
    let n = lSize * lNum;
    let ch = channel List[List[Int]] 2;
    let list1 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list2 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list3 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list4 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list5 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let _ = spawn spawnPrimeLists(list1, ch);
    let _ = spawn spawnPrimeLists(list2, ch);
    let _ = spawn spawnPrimeLists(list3, ch);
    let _ = spawn spawnPrimeLists(list4, ch);
    let _ = spawn spawnPrimeLists(list5, ch);
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    ()

@benchmark
def primeP3T5(): Unit =
    let lNum = 2**3;
    let lSize = 2**12;
    let n = lSize * lNum;
    let ch = channel List[List[Int]] 2;
    let list1 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list2 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list3 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list4 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list5 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let _ = spawn spawnPrimeLists(list1, ch);
    let _ = spawn spawnPrimeLists(list2, ch);
    let _ = spawn spawnPrimeLists(list3, ch);
    let _ = spawn spawnPrimeLists(list4, ch);
    let _ = spawn spawnPrimeLists(list5, ch);
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    ()

@benchmark
def primeP4T5(): Unit =
    let lNum = 2**4;
    let lSize = 2**12;
    let n = lSize * lNum;
    let ch = channel List[List[Int]] 2;
    let list1 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list2 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list3 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list4 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list5 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let _ = spawn spawnPrimeLists(list1, ch);
    let _ = spawn spawnPrimeLists(list2, ch);
    let _ = spawn spawnPrimeLists(list3, ch);
    let _ = spawn spawnPrimeLists(list4, ch);
    let _ = spawn spawnPrimeLists(list5, ch);
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    ()

@benchmark
def primeP5T5(): Unit =
    let lNum = 2**5;
    let lSize = 2**12;
    let n = lSize * lNum;
    let ch = channel List[List[Int]] 2;
    let list1 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list2 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list3 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list4 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list5 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let _ = spawn spawnPrimeLists(list1, ch);
    let _ = spawn spawnPrimeLists(list2, ch);
    let _ = spawn spawnPrimeLists(list3, ch);
    let _ = spawn spawnPrimeLists(list4, ch);
    let _ = spawn spawnPrimeLists(list5, ch);
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    ()

@benchmark
def primeP6T5(): Unit =
    let lNum = 2**6;
    let lSize = 2**12;
    let n = lSize * lNum;
    let ch = channel List[List[Int]] 2;
    let list1 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list2 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list3 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list4 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list5 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let _ = spawn spawnPrimeLists(list1, ch);
    let _ = spawn spawnPrimeLists(list2, ch);
    let _ = spawn spawnPrimeLists(list3, ch);
    let _ = spawn spawnPrimeLists(list4, ch);
    let _ = spawn spawnPrimeLists(list5, ch);
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    ()

@benchmark
def primeP7T5(): Unit =
    let lNum = 2**7;
    let lSize = 2**12;
    let n = lSize * lNum;
    let ch = channel List[List[Int]] 2;
    let list1 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list2 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list3 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list4 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list5 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let _ = spawn spawnPrimeLists(list1, ch);
    let _ = spawn spawnPrimeLists(list2, ch);
    let _ = spawn spawnPrimeLists(list3, ch);
    let _ = spawn spawnPrimeLists(list4, ch);
    let _ = spawn spawnPrimeLists(list5, ch);
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    ()

@benchmark
def primeP8T5(): Unit =
    let lNum = 2**8;
    let lSize = 2**12;
    let n = lSize * lNum;
    let ch = channel List[List[Int]] 2;
    let list1 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list2 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list3 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list4 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list5 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let _ = spawn spawnPrimeLists(list1, ch);
    let _ = spawn spawnPrimeLists(list2, ch);
    let _ = spawn spawnPrimeLists(list3, ch);
    let _ = spawn spawnPrimeLists(list4, ch);
    let _ = spawn spawnPrimeLists(list5, ch);
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    ()

@benchmark
def primeP9T5(): Unit =
    let lNum = 2**9;
    let lSize = 2**12;
    let n = lSize * lNum;
    let ch = channel List[List[Int]] 2;
    let list1 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list2 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list3 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list4 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list5 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let _ = spawn spawnPrimeLists(list1, ch);
    let _ = spawn spawnPrimeLists(list2, ch);
    let _ = spawn spawnPrimeLists(list3, ch);
    let _ = spawn spawnPrimeLists(list4, ch);
    let _ = spawn spawnPrimeLists(list5, ch);
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    ()

@benchmark
def primeP10T5(): Unit =
    let lNum = 2**10;
    let lSize = 2**12;
    let n = lSize * lNum;
    let ch = channel List[List[Int]] 2;
    let list1 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list2 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list3 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list4 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list5 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let _ = spawn spawnPrimeLists(list1, ch);
    let _ = spawn spawnPrimeLists(list2, ch);
    let _ = spawn spawnPrimeLists(list3, ch);
    let _ = spawn spawnPrimeLists(list4, ch);
    let _ = spawn spawnPrimeLists(list5, ch);
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    ()
*/


// 6 Threads
/*
@benchmark
def primeP1T6(): Unit =
    let lNum = 2**1;
    let lSize = 2**12;
    let n = lSize * lNum;
    let ch = channel List[List[Int]] 2;
    let list1 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list2 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list3 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list4 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list5 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list6 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let _ = spawn spawnPrimeLists(list1, ch);
    let _ = spawn spawnPrimeLists(list2, ch);
    let _ = spawn spawnPrimeLists(list3, ch);
    let _ = spawn spawnPrimeLists(list4, ch);
    let _ = spawn spawnPrimeLists(list5, ch);
    let _ = spawn spawnPrimeLists(list6, ch);
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    ()

@benchmark
def primeP2T6(): Unit =
    let lNum = 2**2;
    let lSize = 2**12;
    let n = lSize * lNum;
    let ch = channel List[List[Int]] 2;
    let list1 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list2 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list3 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list4 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list5 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list6 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let _ = spawn spawnPrimeLists(list1, ch);
    let _ = spawn spawnPrimeLists(list2, ch);
    let _ = spawn spawnPrimeLists(list3, ch);
    let _ = spawn spawnPrimeLists(list4, ch);
    let _ = spawn spawnPrimeLists(list5, ch);
    let _ = spawn spawnPrimeLists(list6, ch);
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    ()

@benchmark
def primeP3T6(): Unit =
    let lNum = 2**3;
    let lSize = 2**12;
    let n = lSize * lNum;
    let ch = channel List[List[Int]] 2;
    let list1 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list2 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list3 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list4 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list5 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list6 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let _ = spawn spawnPrimeLists(list1, ch);
    let _ = spawn spawnPrimeLists(list2, ch);
    let _ = spawn spawnPrimeLists(list3, ch);
    let _ = spawn spawnPrimeLists(list4, ch);
    let _ = spawn spawnPrimeLists(list5, ch);
    let _ = spawn spawnPrimeLists(list6, ch);
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    ()

@benchmark
def primeP4T6(): Unit =
    let lNum = 2**4;
    let lSize = 2**12;
    let n = lSize * lNum;
    let ch = channel List[List[Int]] 2;
    let list1 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list2 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list3 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list4 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list5 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list6 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let _ = spawn spawnPrimeLists(list1, ch);
    let _ = spawn spawnPrimeLists(list2, ch);
    let _ = spawn spawnPrimeLists(list3, ch);
    let _ = spawn spawnPrimeLists(list4, ch);
    let _ = spawn spawnPrimeLists(list5, ch);
    let _ = spawn spawnPrimeLists(list6, ch);
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    ()

@benchmark
def primeP5T6(): Unit =
    let lNum = 2**5;
    let lSize = 2**12;
    let n = lSize * lNum;
    let ch = channel List[List[Int]] 2;
    let list1 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list2 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list3 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list4 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list5 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list6 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let _ = spawn spawnPrimeLists(list1, ch);
    let _ = spawn spawnPrimeLists(list2, ch);
    let _ = spawn spawnPrimeLists(list3, ch);
    let _ = spawn spawnPrimeLists(list4, ch);
    let _ = spawn spawnPrimeLists(list5, ch);
    let _ = spawn spawnPrimeLists(list6, ch);
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    ()

@benchmark
def primeP6T6(): Unit =
    let lNum = 2**6;
    let lSize = 2**12;
    let n = lSize * lNum;
    let ch = channel List[List[Int]] 2;
    let list1 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list2 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list3 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list4 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list5 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list6 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let _ = spawn spawnPrimeLists(list1, ch);
    let _ = spawn spawnPrimeLists(list2, ch);
    let _ = spawn spawnPrimeLists(list3, ch);
    let _ = spawn spawnPrimeLists(list4, ch);
    let _ = spawn spawnPrimeLists(list5, ch);
    let _ = spawn spawnPrimeLists(list6, ch);
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    ()

@benchmark
def primeP7T6(): Unit =
    let lNum = 2**7;
    let lSize = 2**12;
    let n = lSize * lNum;
    let ch = channel List[List[Int]] 2;
    let list1 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list2 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list3 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list4 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list5 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list6 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let _ = spawn spawnPrimeLists(list1, ch);
    let _ = spawn spawnPrimeLists(list2, ch);
    let _ = spawn spawnPrimeLists(list3, ch);
    let _ = spawn spawnPrimeLists(list4, ch);
    let _ = spawn spawnPrimeLists(list5, ch);
    let _ = spawn spawnPrimeLists(list6, ch);
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    ()

@benchmark
def primeP8T6(): Unit =
    let lNum = 2**8;
    let lSize = 2**12;
    let n = lSize * lNum;
    let ch = channel List[List[Int]] 2;
    let list1 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list2 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list3 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list4 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list5 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list6 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let _ = spawn spawnPrimeLists(list1, ch);
    let _ = spawn spawnPrimeLists(list2, ch);
    let _ = spawn spawnPrimeLists(list3, ch);
    let _ = spawn spawnPrimeLists(list4, ch);
    let _ = spawn spawnPrimeLists(list5, ch);
    let _ = spawn spawnPrimeLists(list6, ch);
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    ()

@benchmark
def primeP9T6(): Unit =
    let lNum = 2**9;
    let lSize = 2**12;
    let n = lSize * lNum;
    let ch = channel List[List[Int]] 2;
    let list1 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list2 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list3 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list4 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list5 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list6 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let _ = spawn spawnPrimeLists(list1, ch);
    let _ = spawn spawnPrimeLists(list2, ch);
    let _ = spawn spawnPrimeLists(list3, ch);
    let _ = spawn spawnPrimeLists(list4, ch);
    let _ = spawn spawnPrimeLists(list5, ch);
    let _ = spawn spawnPrimeLists(list6, ch);
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    ()

@benchmark
def primeP10T6(): Unit =
    let lNum = 2**10;
    let lSize = 2**12;
    let n = lSize * lNum;
    let ch = channel List[List[Int]] 2;
    let list1 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list2 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list3 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list4 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list5 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list6 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let _ = spawn spawnPrimeLists(list1, ch);
    let _ = spawn spawnPrimeLists(list2, ch);
    let _ = spawn spawnPrimeLists(list3, ch);
    let _ = spawn spawnPrimeLists(list4, ch);
    let _ = spawn spawnPrimeLists(list5, ch);
    let _ = spawn spawnPrimeLists(list6, ch);
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    ()
    
*/

// 7 Threads
/*
@benchmark
def primeP1T7(): Unit =
    let lNum = 2**1;
    let lSize = 2**12;
    let n = lSize * lNum;
    let ch = channel List[List[Int]] 2;
    let list1 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list2 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list3 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list4 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list5 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list6 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list7 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let _ = spawn spawnPrimeLists(list1, ch);
    let _ = spawn spawnPrimeLists(list2, ch);
    let _ = spawn spawnPrimeLists(list3, ch);
    let _ = spawn spawnPrimeLists(list4, ch);
    let _ = spawn spawnPrimeLists(list5, ch);
    let _ = spawn spawnPrimeLists(list6, ch);
    let _ = spawn spawnPrimeLists(list7, ch);
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    ()

@benchmark
def primeP2T7(): Unit =
    let lNum = 2**2;
    let lSize = 2**12;
    let n = lSize * lNum;
    let ch = channel List[List[Int]] 2;
    let list1 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list2 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list3 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list4 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list5 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list6 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list7 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let _ = spawn spawnPrimeLists(list1, ch);
    let _ = spawn spawnPrimeLists(list2, ch);
    let _ = spawn spawnPrimeLists(list3, ch);
    let _ = spawn spawnPrimeLists(list4, ch);
    let _ = spawn spawnPrimeLists(list5, ch);
    let _ = spawn spawnPrimeLists(list6, ch);
    let _ = spawn spawnPrimeLists(list7, ch);
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    ()

@benchmark
def primeP3T7(): Unit =
    let lNum = 2**3;
    let lSize = 2**12;
    let n = lSize * lNum;
    let ch = channel List[List[Int]] 2;
    let list1 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list2 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list3 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list4 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list5 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list6 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list7 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let _ = spawn spawnPrimeLists(list1, ch);
    let _ = spawn spawnPrimeLists(list2, ch);
    let _ = spawn spawnPrimeLists(list3, ch);
    let _ = spawn spawnPrimeLists(list4, ch);
    let _ = spawn spawnPrimeLists(list5, ch);
    let _ = spawn spawnPrimeLists(list6, ch);
    let _ = spawn spawnPrimeLists(list7, ch);
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    ()

@benchmark
def primeP4T7(): Unit =
    let lNum = 2**4;
    let lSize = 2**12;
    let n = lSize * lNum;
    let ch = channel List[List[Int]] 2;
    let list1 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list2 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list3 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list4 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list5 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list6 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list7 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let _ = spawn spawnPrimeLists(list1, ch);
    let _ = spawn spawnPrimeLists(list2, ch);
    let _ = spawn spawnPrimeLists(list3, ch);
    let _ = spawn spawnPrimeLists(list4, ch);
    let _ = spawn spawnPrimeLists(list5, ch);
    let _ = spawn spawnPrimeLists(list6, ch);
    let _ = spawn spawnPrimeLists(list7, ch);
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    ()

@benchmark
def primeP5T7(): Unit =
    let lNum = 2**5;
    let lSize = 2**12;
    let n = lSize * lNum;
    let ch = channel List[List[Int]] 2;
    let list1 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list2 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list3 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list4 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list5 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list6 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list7 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let _ = spawn spawnPrimeLists(list1, ch);
    let _ = spawn spawnPrimeLists(list2, ch);
    let _ = spawn spawnPrimeLists(list3, ch);
    let _ = spawn spawnPrimeLists(list4, ch);
    let _ = spawn spawnPrimeLists(list5, ch);
    let _ = spawn spawnPrimeLists(list6, ch);
    let _ = spawn spawnPrimeLists(list7, ch);
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    ()

@benchmark
def primeP6T7(): Unit =
    let lNum = 2**6;
    let lSize = 2**12;
    let n = lSize * lNum;
    let ch = channel List[List[Int]] 2;
    let list1 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list2 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list3 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list4 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list5 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list6 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list7 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let _ = spawn spawnPrimeLists(list1, ch);
    let _ = spawn spawnPrimeLists(list2, ch);
    let _ = spawn spawnPrimeLists(list3, ch);
    let _ = spawn spawnPrimeLists(list4, ch);
    let _ = spawn spawnPrimeLists(list5, ch);
    let _ = spawn spawnPrimeLists(list6, ch);
    let _ = spawn spawnPrimeLists(list7, ch);
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    ()

@benchmark
def primeP7T7(): Unit =
    let lNum = 2**7;
    let lSize = 2**12;
    let n = lSize * lNum;
    let ch = channel List[List[Int]] 2;
    let list1 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list2 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list3 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list4 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list5 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list6 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list7 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let _ = spawn spawnPrimeLists(list1, ch);
    let _ = spawn spawnPrimeLists(list2, ch);
    let _ = spawn spawnPrimeLists(list3, ch);
    let _ = spawn spawnPrimeLists(list4, ch);
    let _ = spawn spawnPrimeLists(list5, ch);
    let _ = spawn spawnPrimeLists(list6, ch);
    let _ = spawn spawnPrimeLists(list7, ch);
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    ()

@benchmark
def primeP8T7(): Unit =
    let lNum = 2**8;
    let lSize = 2**12;
    let n = lSize * lNum;
    let ch = channel List[List[Int]] 2;
    let list1 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list2 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list3 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list4 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list5 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list6 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list7 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let _ = spawn spawnPrimeLists(list1, ch);
    let _ = spawn spawnPrimeLists(list2, ch);
    let _ = spawn spawnPrimeLists(list3, ch);
    let _ = spawn spawnPrimeLists(list4, ch);
    let _ = spawn spawnPrimeLists(list5, ch);
    let _ = spawn spawnPrimeLists(list6, ch);
    let _ = spawn spawnPrimeLists(list7, ch);
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    ()

@benchmark
def primeP9T7(): Unit =
    let lNum = 2**9;
    let lSize = 2**12;
    let n = lSize * lNum;
    let ch = channel List[List[Int]] 2;
    let list1 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list2 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list3 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list4 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list5 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list6 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list7 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let _ = spawn spawnPrimeLists(list1, ch);
    let _ = spawn spawnPrimeLists(list2, ch);
    let _ = spawn spawnPrimeLists(list3, ch);
    let _ = spawn spawnPrimeLists(list4, ch);
    let _ = spawn spawnPrimeLists(list5, ch);
    let _ = spawn spawnPrimeLists(list6, ch);
    let _ = spawn spawnPrimeLists(list7, ch);
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    ()

@benchmark
def primeP10T7(): Unit =
    let lNum = 2**10;
    let lSize = 2**12;
    let n = lSize * lNum;
    let ch = channel List[List[Int]] 2;
    let list1 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list2 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list3 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list4 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list5 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list6 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list7 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let _ = spawn spawnPrimeLists(list1, ch);
    let _ = spawn spawnPrimeLists(list2, ch);
    let _ = spawn spawnPrimeLists(list3, ch);
    let _ = spawn spawnPrimeLists(list4, ch);
    let _ = spawn spawnPrimeLists(list5, ch);
    let _ = spawn spawnPrimeLists(list6, ch);
    let _ = spawn spawnPrimeLists(list7, ch);
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    ()
*/


// 8 Threads
/*
@benchmark
def primeP1T8(): Unit =
    let lNum = 2**1;
    let lSize = 2**12;
    let n = lSize * lNum;
    let ch = channel List[List[Int]] 2;
    let list1 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list2 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list3 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list4 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list5 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list6 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list7 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list8 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let _ = spawn spawnPrimeLists(list1, ch);
    let _ = spawn spawnPrimeLists(list2, ch);
    let _ = spawn spawnPrimeLists(list3, ch);
    let _ = spawn spawnPrimeLists(list4, ch);
    let _ = spawn spawnPrimeLists(list5, ch);
    let _ = spawn spawnPrimeLists(list6, ch);
    let _ = spawn spawnPrimeLists(list7, ch);
    let _ = spawn spawnPrimeLists(list8, ch);
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    ()

@benchmark
def primeP2T8(): Unit =
    let lNum = 2**2;
    let lSize = 2**12;
    let n = lSize * lNum;
    let ch = channel List[List[Int]] 2;
    let list1 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list2 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list3 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list4 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list5 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list6 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list7 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list8 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let _ = spawn spawnPrimeLists(list1, ch);
    let _ = spawn spawnPrimeLists(list2, ch);
    let _ = spawn spawnPrimeLists(list3, ch);
    let _ = spawn spawnPrimeLists(list4, ch);
    let _ = spawn spawnPrimeLists(list5, ch);
    let _ = spawn spawnPrimeLists(list6, ch);
    let _ = spawn spawnPrimeLists(list7, ch);
    let _ = spawn spawnPrimeLists(list8, ch);
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    ()

@benchmark
def primeP3T8(): Unit =
    let lNum = 2**3;
    let lSize = 2**12;
    let n = lSize * lNum;
    let ch = channel List[List[Int]] 2;
    let list1 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list2 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list3 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list4 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list5 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list6 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list7 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list8 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let _ = spawn spawnPrimeLists(list1, ch);
    let _ = spawn spawnPrimeLists(list2, ch);
    let _ = spawn spawnPrimeLists(list3, ch);
    let _ = spawn spawnPrimeLists(list4, ch);
    let _ = spawn spawnPrimeLists(list5, ch);
    let _ = spawn spawnPrimeLists(list6, ch);
    let _ = spawn spawnPrimeLists(list7, ch);
    let _ = spawn spawnPrimeLists(list8, ch);
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    ()

@benchmark
def primeP4T8(): Unit =
    let lNum = 2**4;
    let lSize = 2**12;
    let n = lSize * lNum;
    let ch = channel List[List[Int]] 2;
    let list1 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list2 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list3 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list4 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list5 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list6 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list7 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list8 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let _ = spawn spawnPrimeLists(list1, ch);
    let _ = spawn spawnPrimeLists(list2, ch);
    let _ = spawn spawnPrimeLists(list3, ch);
    let _ = spawn spawnPrimeLists(list4, ch);
    let _ = spawn spawnPrimeLists(list5, ch);
    let _ = spawn spawnPrimeLists(list6, ch);
    let _ = spawn spawnPrimeLists(list7, ch);
    let _ = spawn spawnPrimeLists(list8, ch);
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    ()

@benchmark
def primeP5T8(): Unit =
    let lNum = 2**5;
    let lSize = 2**12;
    let n = lSize * lNum;
    let ch = channel List[List[Int]] 2;
    let list1 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list2 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list3 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list4 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list5 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list6 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list7 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list8 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let _ = spawn spawnPrimeLists(list1, ch);
    let _ = spawn spawnPrimeLists(list2, ch);
    let _ = spawn spawnPrimeLists(list3, ch);
    let _ = spawn spawnPrimeLists(list4, ch);
    let _ = spawn spawnPrimeLists(list5, ch);
    let _ = spawn spawnPrimeLists(list6, ch);
    let _ = spawn spawnPrimeLists(list7, ch);
    let _ = spawn spawnPrimeLists(list8, ch);
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    ()

@benchmark
def primeP6T8(): Unit =
    let lNum = 2**6;
    let lSize = 2**12;
    let n = lSize * lNum;
    let ch = channel List[List[Int]] 2;
    let list1 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list2 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list3 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list4 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list5 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list6 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list7 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list8 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let _ = spawn spawnPrimeLists(list1, ch);
    let _ = spawn spawnPrimeLists(list2, ch);
    let _ = spawn spawnPrimeLists(list3, ch);
    let _ = spawn spawnPrimeLists(list4, ch);
    let _ = spawn spawnPrimeLists(list5, ch);
    let _ = spawn spawnPrimeLists(list6, ch);
    let _ = spawn spawnPrimeLists(list7, ch);
    let _ = spawn spawnPrimeLists(list8, ch);
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    ()

@benchmark
def primeP7T8(): Unit =
    let lNum = 2**7;
    let lSize = 2**12;
    let n = lSize * lNum;
    let ch = channel List[List[Int]] 2;
    let list1 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list2 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list3 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list4 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list5 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list6 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list7 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list8 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let _ = spawn spawnPrimeLists(list1, ch);
    let _ = spawn spawnPrimeLists(list2, ch);
    let _ = spawn spawnPrimeLists(list3, ch);
    let _ = spawn spawnPrimeLists(list4, ch);
    let _ = spawn spawnPrimeLists(list5, ch);
    let _ = spawn spawnPrimeLists(list6, ch);
    let _ = spawn spawnPrimeLists(list7, ch);
    let _ = spawn spawnPrimeLists(list8, ch);
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    ()

@benchmark
def primeP8T8(): Unit =
    let lNum = 2**8;
    let lSize = 2**12;
    let n = lSize * lNum;
    let ch = channel List[List[Int]] 2;
    let list1 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list2 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list3 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list4 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list5 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list6 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list7 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list8 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let _ = spawn spawnPrimeLists(list1, ch);
    let _ = spawn spawnPrimeLists(list2, ch);
    let _ = spawn spawnPrimeLists(list3, ch);
    let _ = spawn spawnPrimeLists(list4, ch);
    let _ = spawn spawnPrimeLists(list5, ch);
    let _ = spawn spawnPrimeLists(list6, ch);
    let _ = spawn spawnPrimeLists(list7, ch);
    let _ = spawn spawnPrimeLists(list8, ch);
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    ()

@benchmark
def primeP9T8(): Unit =
    let lNum = 2**9;
    let lSize = 2**12;
    let n = lSize * lNum;
    let ch = channel List[List[Int]] 2;
    let list1 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list2 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list3 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list4 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list5 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list6 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list7 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list8 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let _ = spawn spawnPrimeLists(list1, ch);
    let _ = spawn spawnPrimeLists(list2, ch);
    let _ = spawn spawnPrimeLists(list3, ch);
    let _ = spawn spawnPrimeLists(list4, ch);
    let _ = spawn spawnPrimeLists(list5, ch);
    let _ = spawn spawnPrimeLists(list6, ch);
    let _ = spawn spawnPrimeLists(list7, ch);
    let _ = spawn spawnPrimeLists(list8, ch);
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    ()
*/


// 16 Threads
/*
@benchmark
def primeP1T16(): Unit =
    let lNum = 2**1;
    let lSize = 2**12;
    let n = lSize * lNum;
    let ch = channel List[List[Int]] 2;
    let list1 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list2 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list3 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list4 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list5 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list6 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list7 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list8 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list9 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list10 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list11 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list12 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list13 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list14 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list15 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list16 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let _ = spawn spawnPrimeLists(list1, ch);
    let _ = spawn spawnPrimeLists(list2, ch);
    let _ = spawn spawnPrimeLists(list3, ch);
    let _ = spawn spawnPrimeLists(list4, ch);
    let _ = spawn spawnPrimeLists(list5, ch);
    let _ = spawn spawnPrimeLists(list6, ch);
    let _ = spawn spawnPrimeLists(list7, ch);
    let _ = spawn spawnPrimeLists(list8, ch);
    let _ = spawn spawnPrimeLists(list9, ch);
    let _ = spawn spawnPrimeLists(list10, ch);
    let _ = spawn spawnPrimeLists(list11, ch);
    let _ = spawn spawnPrimeLists(list12, ch);
    let _ = spawn spawnPrimeLists(list13, ch);
    let _ = spawn spawnPrimeLists(list14, ch);
    let _ = spawn spawnPrimeLists(list15, ch);
    let _ = spawn spawnPrimeLists(list16, ch);
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    ()

@benchmark
def primeP2T16(): Unit =
    let lNum = 2**2;
    let lSize = 2**12;
    let n = lSize * lNum;
    let ch = channel List[List[Int]] 2;
    let list1 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list2 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list3 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list4 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list5 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list6 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list7 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list8 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list7 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list8 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list9 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list10 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list11 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list12 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list13 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list14 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list15 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list16 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let _ = spawn spawnPrimeLists(list1, ch);
    let _ = spawn spawnPrimeLists(list2, ch);
    let _ = spawn spawnPrimeLists(list3, ch);
    let _ = spawn spawnPrimeLists(list4, ch);
    let _ = spawn spawnPrimeLists(list5, ch);
    let _ = spawn spawnPrimeLists(list6, ch);
    let _ = spawn spawnPrimeLists(list7, ch);
    let _ = spawn spawnPrimeLists(list8, ch);
    let _ = spawn spawnPrimeLists(list9, ch);
    let _ = spawn spawnPrimeLists(list10, ch);
    let _ = spawn spawnPrimeLists(list11, ch);
    let _ = spawn spawnPrimeLists(list12, ch);
    let _ = spawn spawnPrimeLists(list13, ch);
    let _ = spawn spawnPrimeLists(list14, ch);
    let _ = spawn spawnPrimeLists(list15, ch);
    let _ = spawn spawnPrimeLists(list16, ch);
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    ()

@benchmark
def primeP3T16(): Unit =
    let lNum = 2**3;
    let lSize = 2**12;
    let n = lSize * lNum;
    let ch = channel List[List[Int]] 2;
    let list1 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list2 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list3 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list4 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list5 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list6 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list7 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list8 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list7 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list8 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list9 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list10 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list11 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list12 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list13 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list14 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list15 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list16 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let _ = spawn spawnPrimeLists(list1, ch);
    let _ = spawn spawnPrimeLists(list2, ch);
    let _ = spawn spawnPrimeLists(list3, ch);
    let _ = spawn spawnPrimeLists(list4, ch);
    let _ = spawn spawnPrimeLists(list5, ch);
    let _ = spawn spawnPrimeLists(list6, ch);
    let _ = spawn spawnPrimeLists(list7, ch);
    let _ = spawn spawnPrimeLists(list8, ch);
    let _ = spawn spawnPrimeLists(list9, ch);
    let _ = spawn spawnPrimeLists(list10, ch);
    let _ = spawn spawnPrimeLists(list11, ch);
    let _ = spawn spawnPrimeLists(list12, ch);
    let _ = spawn spawnPrimeLists(list13, ch);
    let _ = spawn spawnPrimeLists(list14, ch);
    let _ = spawn spawnPrimeLists(list15, ch);
    let _ = spawn spawnPrimeLists(list16, ch);
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    ()

@benchmark
def primeP4T16(): Unit =
    let lNum = 2**4;
    let lSize = 2**12;
    let n = lSize * lNum;
    let ch = channel List[List[Int]] 2;
    let list1 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list2 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list3 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list4 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list5 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list6 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list7 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list8 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list7 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list8 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list9 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list10 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list11 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list12 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list13 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list14 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list15 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list16 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let _ = spawn spawnPrimeLists(list1, ch);
    let _ = spawn spawnPrimeLists(list2, ch);
    let _ = spawn spawnPrimeLists(list3, ch);
    let _ = spawn spawnPrimeLists(list4, ch);
    let _ = spawn spawnPrimeLists(list5, ch);
    let _ = spawn spawnPrimeLists(list6, ch);
    let _ = spawn spawnPrimeLists(list7, ch);
    let _ = spawn spawnPrimeLists(list8, ch);
    let _ = spawn spawnPrimeLists(list9, ch);
    let _ = spawn spawnPrimeLists(list10, ch);
    let _ = spawn spawnPrimeLists(list11, ch);
    let _ = spawn spawnPrimeLists(list12, ch);
    let _ = spawn spawnPrimeLists(list13, ch);
    let _ = spawn spawnPrimeLists(list14, ch);
    let _ = spawn spawnPrimeLists(list15, ch);
    let _ = spawn spawnPrimeLists(list16, ch);
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    ()

@benchmark
def primeP5T16(): Unit =
    let lNum = 2**5;
    let lSize = 2**12;
    let n = lSize * lNum;
    let ch = channel List[List[Int]] 2;
    let list1 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list2 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list3 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list4 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list5 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list6 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list7 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list8 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list7 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list8 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list9 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list10 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list11 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list12 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list13 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list14 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list15 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list16 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let _ = spawn spawnPrimeLists(list1, ch);
    let _ = spawn spawnPrimeLists(list2, ch);
    let _ = spawn spawnPrimeLists(list3, ch);
    let _ = spawn spawnPrimeLists(list4, ch);
    let _ = spawn spawnPrimeLists(list5, ch);
    let _ = spawn spawnPrimeLists(list6, ch);
    let _ = spawn spawnPrimeLists(list7, ch);
    let _ = spawn spawnPrimeLists(list8, ch);
    let _ = spawn spawnPrimeLists(list9, ch);
    let _ = spawn spawnPrimeLists(list10, ch);
    let _ = spawn spawnPrimeLists(list11, ch);
    let _ = spawn spawnPrimeLists(list12, ch);
    let _ = spawn spawnPrimeLists(list13, ch);
    let _ = spawn spawnPrimeLists(list14, ch);
    let _ = spawn spawnPrimeLists(list15, ch);
    let _ = spawn spawnPrimeLists(list16, ch);
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    ()

@benchmark
def primeP6T16(): Unit =
    let lNum = 2**6;
    let lSize = 2**12;
    let n = lSize * lNum;
    let ch = channel List[List[Int]] 2;
    let list1 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list2 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list3 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list4 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list5 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list6 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list7 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list8 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list7 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list8 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list9 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list10 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list11 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list12 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list13 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list14 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list15 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list16 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let _ = spawn spawnPrimeLists(list1, ch);
    let _ = spawn spawnPrimeLists(list2, ch);
    let _ = spawn spawnPrimeLists(list3, ch);
    let _ = spawn spawnPrimeLists(list4, ch);
    let _ = spawn spawnPrimeLists(list5, ch);
    let _ = spawn spawnPrimeLists(list6, ch);
    let _ = spawn spawnPrimeLists(list7, ch);
    let _ = spawn spawnPrimeLists(list8, ch);
    let _ = spawn spawnPrimeLists(list9, ch);
    let _ = spawn spawnPrimeLists(list10, ch);
    let _ = spawn spawnPrimeLists(list11, ch);
    let _ = spawn spawnPrimeLists(list12, ch);
    let _ = spawn spawnPrimeLists(list13, ch);
    let _ = spawn spawnPrimeLists(list14, ch);
    let _ = spawn spawnPrimeLists(list15, ch);
    let _ = spawn spawnPrimeLists(list16, ch);
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    ()

@benchmark
def primeP7T16(): Unit =
    let lNum = 2**7;
    let lSize = 2**12;
    let n = lSize * lNum;
    let ch = channel List[List[Int]] 2;
    let list1 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list2 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list3 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list4 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list5 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list6 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list7 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list8 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list7 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list8 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list9 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list10 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list11 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list12 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list13 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list14 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list15 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list16 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let _ = spawn spawnPrimeLists(list1, ch);
    let _ = spawn spawnPrimeLists(list2, ch);
    let _ = spawn spawnPrimeLists(list3, ch);
    let _ = spawn spawnPrimeLists(list4, ch);
    let _ = spawn spawnPrimeLists(list5, ch);
    let _ = spawn spawnPrimeLists(list6, ch);
    let _ = spawn spawnPrimeLists(list7, ch);
    let _ = spawn spawnPrimeLists(list8, ch);
    let _ = spawn spawnPrimeLists(list9, ch);
    let _ = spawn spawnPrimeLists(list10, ch);
    let _ = spawn spawnPrimeLists(list11, ch);
    let _ = spawn spawnPrimeLists(list12, ch);
    let _ = spawn spawnPrimeLists(list13, ch);
    let _ = spawn spawnPrimeLists(list14, ch);
    let _ = spawn spawnPrimeLists(list15, ch);
    let _ = spawn spawnPrimeLists(list16, ch);
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    ()

@benchmark
def primeP8T16(): Unit =
    let lNum = 2**8;
    let lSize = 2**12;
    let n = lSize * lNum;
    let ch = channel List[List[Int]] 2;
    let list1 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list2 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list3 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list4 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list5 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list6 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list7 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list8 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list7 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list8 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list9 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list10 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list11 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list12 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list13 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list14 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list15 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let list16 = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let _ = spawn spawnPrimeLists(list1, ch);
    let _ = spawn spawnPrimeLists(list2, ch);
    let _ = spawn spawnPrimeLists(list3, ch);
    let _ = spawn spawnPrimeLists(list4, ch);
    let _ = spawn spawnPrimeLists(list5, ch);
    let _ = spawn spawnPrimeLists(list6, ch);
    let _ = spawn spawnPrimeLists(list7, ch);
    let _ = spawn spawnPrimeLists(list8, ch);
    let _ = spawn spawnPrimeLists(list9, ch);
    let _ = spawn spawnPrimeLists(list10, ch);
    let _ = spawn spawnPrimeLists(list11, ch);
    let _ = spawn spawnPrimeLists(list12, ch);
    let _ = spawn spawnPrimeLists(list13, ch);
    let _ = spawn spawnPrimeLists(list14, ch);
    let _ = spawn spawnPrimeLists(list15, ch);
    let _ = spawn spawnPrimeLists(list16, ch);
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    ()
*/



// 32 Threads

@benchmark
def primeP1T32(): Unit =
    let t = 32;
    let lNum = 2**1;
    let lSize = 2**12;
    let n = lSize * lNum * t;
    let ch = channel List[List[Int]] t;
    let list = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let range = List.range(1, t);
    let _ = List.map((x) -> spawn spawnPrimeLists(list, ch), range);
    ()

@benchmark
def primeP2T32(): Unit =
    let t = 32;
    let lNum = 2**2;
    let lSize = 2**12;
    let n = lSize * lNum * t;
    let ch = channel List[List[Int]] t;
    let list = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let range = List.range(1, t);
    let _ = List.map((x) -> spawn spawnPrimeLists(list, ch), range);
    ()

@benchmark
def primeP3T32(): Unit =
    let t = 32;
    let lNum = 2**3;
    let lSize = 2**12;
    let n = lSize * lNum * t;
    let ch = channel List[List[Int]] t;
    let list = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let range = List.range(1, t);
    let _ = List.map((x) -> spawn spawnPrimeLists(list, ch), range);
    ()

@benchmark
def primeP4T32(): Unit =
    let t = 32;
    let lNum = 2**4;
    let lSize = 2**12;
    let n = lSize * lNum * t;
    let ch = channel List[List[Int]] t;
    let list = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let range = List.range(1, t);
    let _ = List.map((x) -> spawn spawnPrimeLists(list, ch), range);
    ()

@benchmark
def primeP5T32(): Unit =
    let t = 32;
    let lNum = 2**5;
    let lSize = 2**12;
    let n = lSize * lNum * t;
    let ch = channel List[List[Int]] t;
    let list = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let range = List.range(1, t);
    let _ = List.map((x) -> spawn spawnPrimeLists(list, ch), range);
    ()

@benchmark
def primeP6T32(): Unit =
    let t = 32;
    let lNum = 2**6;
    let lSize = 2**12;
    let n = lSize * lNum * t;
    let ch = channel List[List[Int]] t;
    let list = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let range = List.range(1, t);
    let _ = List.map((x) -> spawn spawnPrimeLists(list, ch), range);
    ()

@benchmark
def primeP7T32(): Unit =
    let t = 32;
    let lNum = 2**7;
    let lSize = 2**12;
    let n = lSize * lNum * t;
    let ch = channel List[List[Int]] t;
    let list = List.map((x) -> List.range(lSize * 10, lSize * 11), List.range(1, lNum));
    let range = List.range(1, t);
    let _ = List.map((x) -> spawn spawnPrimeLists(list, ch), range);
    ()
