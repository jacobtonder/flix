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
    List.filter((x) -> prime(x, 1000), list)

def spawnPrimeLists(list: List[List[Int]], outCh: Channel[List[List[Int]]]): Unit =
    let primedList = List.map((x) -> applyPrimeToList(x), list);
    let _ = outCh <- List.append(primedList, (<- outCh));
    ()


// 1 Thread

@benchmark
def primeP1T1(): Unit =
    let lNum = 2**1;
    let lSize = 2**12;
    let n = lSize * lNum;
    let ch = channel List[List[Int]] 1;
    let list1 = List.map((x) -> List.range(1, lSize), List.range(1, lNum));
    let _ = ch <- (Nil) :: Nil;
    let _ = spawn spawnPrimeLists(list1, ch);
    let _ = <- ch;
    ()