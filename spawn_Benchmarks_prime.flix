/*
def primeList(list: List[Int], s: Int): List[(Int, Bool)] =
    let lenght = List.length(list);
    match s with {
        case 0 => Nil
        case s =>
            let ls = lenght / s;
            match list with {
                case Nil => Nil
                case list =>
                    let firstList = List.slice(0, ls, list);
                    let restList = List.slice(ls + 1, lenght, list);
                    let primedList = applyPrimeToList(firstList);
                    List.append(primedList, primeList(restList, s - 1))
            }
    }
*/

/*
def spawnPrimeList(list: List[Int], s: Int, ch: Channel[List[(Int, Bool)]]): Unit =
    let firstCh = channel List[(Int, Bool)] 1;
    let restCh = channel List[(Int, Bool)] 1;
    let lenght = List.length(list);
    match s with {
        case 0 =>
            let _ = ch <- Nil;
            ()
        case s =>
            let ls = lenght / s;
            match list with {
                //case Nil => ()
                case list =>
                    let firstList = List.slice(0, ls, list);
                    let restList = List.slice(ls + 1, lenght, list);
                    let _ = spawn spawnApplyPrimeToList(firstList, firstCh);
                    let _ = spawn spawnPrimeList(restList, s - 1, restCh);
                    let firstListPrimed = <- firstCh;
                    let restListPrimed = <- restCh;
                    let _ = ch <- List.append(firstListPrimed, restListPrimed);
                    ()
            }
    }
*/

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

/*

def prime(n: Int, x: Int): (Int, Bool) =
    if(x > 0)
        let _ = prime(n, x - 1);
        (x, isPrime(n, 5, 2))
    else
        (x, isPrime(n, 5, 2))


def applyPrimeToList(list: List[Int]): List[(Int, Bool)] =
    List.map((x) -> prime(x, 100), list)

def spawnApplyPrimeToList(list: List[Int], ch: Channel[List[(Int, Bool)]]): Unit =
    let _ = ch <- List.map((x) -> prime(x, 100), list);
    ()

def primeList(n: Int, s: Int): List[(Int, Bool)] =
    let max = 7**2;
    if (n - s > max)
        let e = (1 + (s / max)) * max;
        let list = List.range(s, e);
        let _ = primeList(n, e);
        applyPrimeToList(list)
    else
        let list = List.range(s, n);
        applyPrimeToList(list)

def spawnPrimeList(n: Int, s: Int, firstCh: Channel[List[(Int, Bool)]], t: Int): Unit =
    let max = 7**2;
    let lastCh = channel List[(Int, Bool)] 1;
    if (n - s > max)
        let e = (1 + (s / max)) * max;
        let list = List.range(s, e);
        if (t > 1)
            let _ = spawn spawnPrimeList(n, e, lastCh, t - 1);
            let _ = spawnApplyPrimeToList(list, firstCh);
            let _ = <- lastCh;
            ()
        else
            let _ = spawnPrimeList(n, e, lastCh, t);
            let _ = spawnApplyPrimeToList(list, firstCh);
            let _ = <- lastCh;
            ()
    else
        let list = List.range(s, n);
        spawnApplyPrimeToList(list, firstCh);
        ()

def spawnPrimeListUnLimited(n: Int, s: Int, firstCh: Channel[List[(Int, Bool)]]): Unit =
    let max = 7**2;
    let lastCh = channel List[(Int, Bool)] 1;
    if (n - s > max)
        let e = (1 + (s / max)) * max;
        let list = List.range(s, e);
        let _ = spawn spawnPrimeListUnLimited(n, e, lastCh);
        let _ = spawnApplyPrimeToList(list, firstCh);
        let _ = <- lastCh;
        ()
    else
        let list = List.range(s, n);
        spawnApplyPrimeToList(list, firstCh);
        ()

// Sequential

@benchmark
    def primesToP1(): List[(Int, Bool)] =
        let n = 2**1;
        primeList(n, 2)

@benchmark
    def primesToP2(): List[(Int, Bool)] =
        let n = 2**2;
        primeList(n, 2)

@benchmark
    def primesToP3(): List[(Int, Bool)] =
        let n = 2**3;
        primeList(n, 2)

@benchmark
    def primesToP4(): List[(Int, Bool)] =
        let n = 2**4;
        primeList(n, 2)

@benchmark
    def primesToP5(): List[(Int, Bool)] =
        let n = 2**5;
        primeList(n, 2)

@benchmark
    def primesToP6(): List[(Int, Bool)] =
        let n = 2**6;
        primeList(n, 2)

@benchmark
    def primesToP7(): List[(Int, Bool)] =
        let n = 2**7;
        primeList(n, 2)

@benchmark
    def primesToP8(): List[(Int, Bool)] =
        let n = 2**8;
        primeList(n, 2)

@benchmark
    def primesToP9(): List[(Int, Bool)] =
        let n = 2**9;
        primeList(n, 2)

@benchmark
    def primesToP10(): List[(Int, Bool)] =
        let n = 2**10;
        primeList(n, 2)

@benchmark
    def primesToP11(): List[(Int, Bool)] =
        let n = 2**11;
        primeList(n, 2)

@benchmark
    def primesToP12(): List[(Int, Bool)] =
        let n = 2**12;
        primeList(n, 2)

@benchmark
    def primesToP13(): List[(Int, Bool)] =
        let n = 2**13;
        primeList(n, 2)

@benchmark
    def primesToP14(): List[(Int, Bool)] =
        let n = 2**14;
        primeList(n, 2)
*/

// 1 Thread
/*
@benchmark
    def primesToP1T1Spawn(): List[(Int, Bool)] =
        let n = 2**1;
        let ch = channel List[(Int, Bool)] 1;
        let t = 1;
        let _ = spawnPrimeList(n, 2, ch, t);
        <- ch

@benchmark
    def primesToP2T1Spawn(): List[(Int, Bool)] =
        let n = 2**2;
        let ch = channel List[(Int, Bool)] 1;
        let t = 1;
        let _ = spawnPrimeList(n, 2, ch, t);
        <- ch

@benchmark
    def primesToP3T1Spawn(): List[(Int, Bool)] =
        let n = 2**3;
        let ch = channel List[(Int, Bool)] 1;
        let t = 1;
        let _ = spawnPrimeList(n, 2, ch, t);
        <- ch

@benchmark
    def primesToP4T1Spawn(): List[(Int, Bool)] =
        let n = 2**4;
        let ch = channel List[(Int, Bool)] 1;
        let t = 1;
        let _ = spawnPrimeList(n, 2, ch, t);
        <- ch

@benchmark
    def primesToP5T1Spawn(): List[(Int, Bool)] =
        let n = 2**5;
        let ch = channel List[(Int, Bool)] 1;
        let t = 1;
        let _ = spawnPrimeList(n, 2, ch, t);
        <- ch

@benchmark
    def primesToP6T1Spawn(): List[(Int, Bool)] =
        let n = 2**6;
        let ch = channel List[(Int, Bool)] 1;
        let t = 1;
        let _ = spawnPrimeList(n, 2, ch, t);
        <- ch

@benchmark
    def primesToP7T1Spawn(): List[(Int, Bool)] =
        let n = 2**7;
        let ch = channel List[(Int, Bool)] 1;
        let t = 1;
        let _ = spawnPrimeList(n, 2, ch, t);
        <- ch

@benchmark
    def primesToP8T1Spawn(): List[(Int, Bool)] =
        let n = 2**1;
        let ch = channel List[(Int, Bool)] 1;
        let t = 1;
        let _ = spawnPrimeList(n, 2, ch, t);
        <- ch

@benchmark
    def primesToP9T1Spawn(): List[(Int, Bool)] =
        let n = 2**9;
        let ch = channel List[(Int, Bool)] 1;
        let t = 1;
        let _ = spawnPrimeList(n, 2, ch, t);
        <- ch

@benchmark
    def primesToP10T1Spawn(): List[(Int, Bool)] =
        let n = 2**10;
        let ch = channel List[(Int, Bool)] 1;
        let t = 1;
        let _ = spawnPrimeList(n, 2, ch, t);
        <- ch

@benchmark
    def primesToP11T1Spawn(): List[(Int, Bool)] =
        let n = 2**11;
        let ch = channel List[(Int, Bool)] 1;
        let t = 1;
        let _ = spawnPrimeList(n, 2, ch, t);
        <- ch

@benchmark
    def primesToP12T1Spawn(): List[(Int, Bool)] =
        let n = 2**12;
        let ch = channel List[(Int, Bool)] 1;
        let t = 1;
        let _ = spawnPrimeList(n, 2, ch, t);
        <- ch

@benchmark
    def primesToP13T1Spawn(): List[(Int, Bool)] =
        let n = 2**13;
        let ch = channel List[(Int, Bool)] 1;
        let t = 1;
        let _ = spawnPrimeList(n, 2, ch, t);
        <- ch

@benchmark
    def primesToP14T1Spawn(): List[(Int, Bool)] =
        let n = 2**14;
        let ch = channel List[(Int, Bool)] 1;
        let t = 1;
        let _ = spawnPrimeList(n, 2, ch, t);
        <- ch

*/
// 2 Threads
/*

@benchmark
    def primesToP1T2Spawn(): List[(Int, Bool)] =
        let n = 2**1;
        let ch = channel List[(Int, Bool)] 1;
        let t = 2;
        let _ = spawnPrimeList(n, 2, ch, t);
        <- ch

@benchmark
    def primesToP2T2Spawn(): List[(Int, Bool)] =
        let n = 2**2;
        let ch = channel List[(Int, Bool)] 1;
        let t = 2;
        let _ = spawnPrimeList(n, 2, ch, t);
        <- ch

@benchmark
    def primesToP3T2Spawn(): List[(Int, Bool)] =
        let n = 2**3;
        let ch = channel List[(Int, Bool)] 1;
        let t = 2;
        let _ = spawnPrimeList(n, 2, ch, t);
        <- ch

@benchmark
    def primesToP4T2Spawn(): List[(Int, Bool)] =
        let n = 2**4;
        let ch = channel List[(Int, Bool)] 1;
        let t = 2;
        let _ = spawnPrimeList(n, 2, ch, t);
        <- ch

@benchmark
    def primesToP5T2Spawn(): List[(Int, Bool)] =
        let n = 2**5;
        let ch = channel List[(Int, Bool)] 1;
        let t = 2;
        let _ = spawnPrimeList(n, 2, ch, t);
        <- ch

@benchmark
    def primesToP6T2Spawn(): List[(Int, Bool)] =
        let n = 2**6;
        let ch = channel List[(Int, Bool)] 1;
        let t = 2;
        let _ = spawnPrimeList(n, 2, ch, t);
        <- ch

@benchmark
    def primesToP7T2Spawn(): List[(Int, Bool)] =
        let n = 2**7;
        let ch = channel List[(Int, Bool)] 1;
        let t = 2;
        let _ = spawnPrimeList(n, 2, ch, t);
        <- ch

@benchmark
    def primesToP8T2Spawn(): List[(Int, Bool)] =
        let n = 2**1;
        let ch = channel List[(Int, Bool)] 1;
        let t = 2;
        let _ = spawnPrimeList(n, 2, ch, t);
        <- ch

@benchmark
    def primesToP9T2Spawn(): List[(Int, Bool)] =
        let n = 2**9;
        let ch = channel List[(Int, Bool)] 1;
        let t = 2;
        let _ = spawnPrimeList(n, 2, ch, t);
        <- ch

@benchmark
    def primesToP10T2Spawn(): List[(Int, Bool)] =
        let n = 2**10;
        let ch = channel List[(Int, Bool)] 1;
        let t = 2;
        let _ = spawnPrimeList(n, 2, ch, t);
        <- ch

@benchmark
    def primesToP11T2Spawn(): List[(Int, Bool)] =
        let n = 2**11;
        let ch = channel List[(Int, Bool)] 1;
        let t = 2;
        let _ = spawnPrimeList(n, 2, ch, t);
        <- ch

@benchmark
    def primesToP12T2Spawn(): List[(Int, Bool)] =
        let n = 2**12;
        let ch = channel List[(Int, Bool)] 1;
        let t = 2;
        let _ = spawnPrimeList(n, 2, ch, t);
        <- ch


@benchmark
    def primesToP13T2Spawn(): List[(Int, Bool)] =
        let n = 2**13;
        let ch = channel List[(Int, Bool)] 1;
        let t = 2;
        let _ = spawnPrimeList(n, 2, ch, t);
        <- ch

@benchmark
    def primesToP14T2Spawn(): List[(Int, Bool)] =
        let n = 2**14;
        let ch = channel List[(Int, Bool)] 1;
        let t = 2;
        let _ = spawnPrimeList(n, 2, ch, t);
        <- ch



// 4 Threads

@benchmark
    def primesToP1T4Spawn(): List[(Int, Bool)] =
        let n = 2**1;
        let ch = channel List[(Int, Bool)] 1;
        let t = 4;
        let _ = spawnPrimeList(n, 2, ch, t);
        <- ch

@benchmark
    def primesToP2T4Spawn(): List[(Int, Bool)] =
        let n = 2**2;
        let ch = channel List[(Int, Bool)] 1;
        let t = 4;
        let _ = spawnPrimeList(n, 2, ch, t);
        <- ch

@benchmark
    def primesToP3T4Spawn(): List[(Int, Bool)] =
        let n = 2**3;
        let ch = channel List[(Int, Bool)] 1;
        let t = 4;
        let _ = spawnPrimeList(n, 2, ch, t);
        <- ch

@benchmark
    def primesToP4T4Spawn(): List[(Int, Bool)] =
        let n = 2**4;
        let ch = channel List[(Int, Bool)] 1;
        let t = 4;
        let _ = spawnPrimeList(n, 2, ch, t);
        <- ch

@benchmark
    def primesToP5T4Spawn(): List[(Int, Bool)] =
        let n = 2**5;
        let ch = channel List[(Int, Bool)] 1;
        let t = 4;
        let _ = spawnPrimeList(n, 2, ch, t);
        <- ch

@benchmark
    def primesToP6T4Spawn(): List[(Int, Bool)] =
        let n = 2**6;
        let ch = channel List[(Int, Bool)] 1;
        let t = 4;
        let _ = spawnPrimeList(n, 2, ch, t);
        <- ch

@benchmark
    def primesToP7T4Spawn(): List[(Int, Bool)] =
        let n = 2**7;
        let ch = channel List[(Int, Bool)] 1;
        let t = 4;
        let _ = spawnPrimeList(n, 2, ch, t);
        <- ch

@benchmark
    def primesToP8T4Spawn(): List[(Int, Bool)] =
        let n = 2**1;
        let ch = channel List[(Int, Bool)] 1;
        let t = 4;
        let _ = spawnPrimeList(n, 2, ch, t);
        <- ch

@benchmark
    def primesToP9T4Spawn(): List[(Int, Bool)] =
        let n = 2**9;
        let ch = channel List[(Int, Bool)] 1;
        let t = 4;
        let _ = spawnPrimeList(n, 2, ch, t);
        <- ch

@benchmark
    def primesToP10T4Spawn(): List[(Int, Bool)] =
        let n = 2**10;
        let ch = channel List[(Int, Bool)] 1;
        let t = 4;
        let _ = spawnPrimeList(n, 2, ch, t);
        <- ch

@benchmark
    def primesToP11T4Spawn(): List[(Int, Bool)] =
        let n = 2**11;
        let ch = channel List[(Int, Bool)] 1;
        let t = 4;
        let _ = spawnPrimeList(n, 2, ch, t);
        <- ch

@benchmark
    def primesToP12T4Spawn(): List[(Int, Bool)] =
        let n = 2**12;
        let ch = channel List[(Int, Bool)] 1;
        let t = 4;
        let _ = spawnPrimeList(n, 2, ch, t);
        <- ch

@benchmark
    def primesToP13T4Spawn(): List[(Int, Bool)] =
        let n = 2**13;
        let ch = channel List[(Int, Bool)] 1;
        let t = 4;
        let _ = spawnPrimeList(n, 2, ch, t);
        <- ch

@benchmark
    def primesToP14T4Spawn(): List[(Int, Bool)] =
        let n = 2**14;
        let ch = channel List[(Int, Bool)] 1;
        let t = 4;
        let _ = spawnPrimeList(n, 2, ch, t);
        <- ch



// 8 Threads
 
@benchmark
    def primesToP1T8Spawn(): List[(Int, Bool)] =
        let n = 2**1;
        let ch = channel List[(Int, Bool)] 1;
        let t = 8;
        let _ = spawnPrimeList(n, 2, ch, t);
        <- ch

@benchmark
    def primesToP2T8Spawn(): List[(Int, Bool)] =
        let n = 2**2;
        let ch = channel List[(Int, Bool)] 1;
        let t = 8;
        let _ = spawnPrimeList(n, 2, ch, t);
        <- ch

@benchmark
    def primesToP3T8Spawn(): List[(Int, Bool)] =
        let n = 2**3;
        let ch = channel List[(Int, Bool)] 1;
        let t = 8;
        let _ = spawnPrimeList(n, 2, ch, t);
        <- ch

@benchmark
    def primesToP4T8Spawn(): List[(Int, Bool)] =
        let n = 2**4;
        let ch = channel List[(Int, Bool)] 1;
        let t = 8;
        let _ = spawnPrimeList(n, 2, ch, t);
        <- ch

@benchmark
    def primesToP5T8Spawn(): List[(Int, Bool)] =
        let n = 2**5;
        let ch = channel List[(Int, Bool)] 1;
        let t = 8;
        let _ = spawnPrimeList(n, 2, ch, t);
        <- ch

@benchmark
    def primesToP6T8Spawn(): List[(Int, Bool)] =
        let n = 2**6;
        let ch = channel List[(Int, Bool)] 1;
        let t = 8;
        let _ = spawnPrimeList(n, 2, ch, t);
        <- ch

@benchmark
    def primesToP7T8Spawn(): List[(Int, Bool)] =
        let n = 2**7;
        let ch = channel List[(Int, Bool)] 1;
        let t = 8;
        let _ = spawnPrimeList(n, 2, ch, t);
        <- ch

@benchmark
    def primesToP8T8Spawn(): List[(Int, Bool)] =
        let n = 2**8;
        let ch = channel List[(Int, Bool)] 1;
        let t = 8;
        let _ = spawnPrimeList(n, 2, ch, t);
        <- ch

@benchmark
    def primesToP9T8Spawn(): List[(Int, Bool)] =
        let n = 2**9;
        let ch = channel List[(Int, Bool)] 1;
        let t = 8;
        let _ = spawnPrimeList(n, 2, ch, t);
        <- ch

@benchmark
    def primesToP10T8Spawn(): List[(Int, Bool)] =
        let n = 2**10;
        let ch = channel List[(Int, Bool)] 1;
        let t = 8;
        let _ = spawnPrimeList(n, 2, ch, t);
        <- ch

@benchmark
    def primesToP11T8Spawn(): List[(Int, Bool)] =
        let n = 2**11;
        let ch = channel List[(Int, Bool)] 1;
        let t = 8;
        let _ = spawnPrimeList(n, 2, ch, t);
        <- ch

@benchmark
    def primesToP12T8Spawn(): List[(Int, Bool)] =
        let n = 2**12;
        let ch = channel List[(Int, Bool)] 1;
        let t = 8;
        let _ = spawnPrimeList(n, 2, ch, t);
        <- ch

@benchmark
    def primesToP13T8Spawn(): List[(Int, Bool)] =
        let n = 2**13;
        let ch = channel List[(Int, Bool)] 1;
        let t = 8;
        let _ = spawnPrimeList(n, 2, ch, t);
        <- ch

@benchmark
    def primesToP14T8Spawn(): List[(Int, Bool)] =
        let n = 2**14;
        let ch = channel List[(Int, Bool)] 1;
        let t = 8;
        let _ = spawnPrimeList(n, 2, ch, t);
        <- ch
*/
/*
@benchmark
    def primesToP15T8Spawn(): List[(Int, Bool)] =
        let n = 2**15;
        let ch = channel List[(Int, Bool)] 1;
        let t = 8;
        let _ = spawnPrimeList(n, 2, ch, t);
        <- ch

@benchmark
    def primesToP16T8Spawn(): List[(Int, Bool)] =
        let n = 2**16;
        let ch = channel List[(Int, Bool)] 1;
        let t = 8;
        let _ = spawnPrimeList(n, 2, ch, t);
        <- ch

@benchmark
    def primesToP17T8Spawn(): List[(Int, Bool)] =
    let n = 2**17;
    let ch = channel List[(Int, Bool)] 1;
    let t = 8;
    let _ = spawnPrimeList(n, 2, ch, t);
    <- ch

@benchmark
    def primesToP18T8Spawn(): List[(Int, Bool)] =
    let n = 2**18;
    let ch = channel List[(Int, Bool)] 1;
    let t = 8;
    let _ = spawnPrimeList(n, 2, ch, t);
    <- ch

@benchmark
    def primesToP19T8Spawn(): List[(Int, Bool)] =
    let n = 2**19;
    let ch = channel List[(Int, Bool)] 1;
    let t = 8;
    let _ = spawnPrimeList(n, 2, ch, t);
    <- ch

@benchmark
    def primesToP20T8Spawn(): List[(Int, Bool)] =
    let n = 2**20;
    let ch = channel List[(Int, Bool)] 1;
    let t = 8;
    let _ = spawnPrimeList(n, 2, ch, t);
    <- ch
*/



// 16 Threads
 /*
@benchmark
    def primesToP1T16Spawn(): List[(Int, Bool)] =
        let n = 2**1;
        let ch = channel List[(Int, Bool)] 1;
        let t = 16;
        let _ = spawnPrimeList(n, 2, ch, t);
        <- ch

@benchmark
    def primesToP2T16Spawn(): List[(Int, Bool)] =
        let n = 2**2;
        let ch = channel List[(Int, Bool)] 1;
        let t = 16;
        let _ = spawnPrimeList(n, 2, ch, t);
        <- ch

@benchmark
    def primesToP3T16Spawn(): List[(Int, Bool)] =
        let n = 2**3;
        let ch = channel List[(Int, Bool)] 1;
        let t = 16;
        let _ = spawnPrimeList(n, 2, ch, t);
        <- ch

@benchmark
    def primesToP4T16Spawn(): List[(Int, Bool)] =
        let n = 2**4;
        let ch = channel List[(Int, Bool)] 1;
        let t = 16;
        let _ = spawnPrimeList(n, 2, ch, t);
        <- ch

@benchmark
    def primesToP5T16Spawn(): List[(Int, Bool)] =
        let n = 2**5;
        let ch = channel List[(Int, Bool)] 1;
        let t = 16;
        let _ = spawnPrimeList(n, 2, ch, t);
        <- ch

@benchmark
    def primesToP6T16Spawn(): List[(Int, Bool)] =
        let n = 2**6;
        let ch = channel List[(Int, Bool)] 1;
        let t = 16;
        let _ = spawnPrimeList(n, 2, ch, t);
        <- ch

@benchmark
    def primesToP7T16Spawn(): List[(Int, Bool)] =
        let n = 2**7;
        let ch = channel List[(Int, Bool)] 1;
        let t = 16;
        let _ = spawnPrimeList(n, 2, ch, t);
        <- ch

@benchmark
    def primesToP8T16Spawn(): List[(Int, Bool)] =
        let n = 2**8;
        let ch = channel List[(Int, Bool)] 1;
        let t = 16;
        let _ = spawnPrimeList(n, 2, ch, t);
        <- ch

@benchmark
    def primesToP9T16Spawn(): List[(Int, Bool)] =
        let n = 2**9;
        let ch = channel List[(Int, Bool)] 1;
        let t = 16;
        let _ = spawnPrimeList(n, 2, ch, t);
        <- ch

@benchmark
    def primesToP10T16Spawn(): List[(Int, Bool)] =
        let n = 2**10;
        let ch = channel List[(Int, Bool)] 1;
        let t = 16;
        let _ = spawnPrimeList(n, 2, ch, t);
        <- ch

@benchmark
    def primesToP11T16Spawn(): List[(Int, Bool)] =
        let n = 2**11;
        let ch = channel List[(Int, Bool)] 1;
        let t = 16;
        let _ = spawnPrimeList(n, 2, ch, t);
        <- ch

@benchmark
    def primesToP12T16Spawn(): List[(Int, Bool)] =
        let n = 2**12;
        let ch = channel List[(Int, Bool)] 1;
        let t = 16;
        let _ = spawnPrimeList(n, 2, ch, t);
        <- ch

@benchmark
    def primesToP13T16Spawn(): List[(Int, Bool)] =
        let n = 2**13;
        let ch = channel List[(Int, Bool)] 1;
        let t = 16;
        let _ = spawnPrimeList(n, 2, ch, t);
        <- ch

@benchmark
    def primesToP14T16Spawn(): List[(Int, Bool)] =
        let n = 2**14;
        let ch = channel List[(Int, Bool)] 1;
        let t = 16;
        let _ = spawnPrimeList(n, 2, ch, t);
        <- ch
        
        
        
// 32 Threads
 
@benchmark
    def primesToP1T32Spawn(): List[(Int, Bool)] =
        let n = 2**1;
        let ch = channel List[(Int, Bool)] 1;
        let t = 32;
        let _ = spawnPrimeList(n, 2, ch, t);
        <- ch

@benchmark
    def primesToP2T32Spawn(): List[(Int, Bool)] =
        let n = 2**2;
        let ch = channel List[(Int, Bool)] 1;
        let t = 32;
        let _ = spawnPrimeList(n, 2, ch, t);
        <- ch

@benchmark
    def primesToP3T32Spawn(): List[(Int, Bool)] =
        let n = 2**3;
        let ch = channel List[(Int, Bool)] 1;
        let t = 32;
        let _ = spawnPrimeList(n, 2, ch, t);
        <- ch

@benchmark
    def primesToP4T32Spawn(): List[(Int, Bool)] =
        let n = 2**4;
        let ch = channel List[(Int, Bool)] 1;
        let t = 32;
        let _ = spawnPrimeList(n, 2, ch, t);
        <- ch

@benchmark
    def primesToP5T32Spawn(): List[(Int, Bool)] =
        let n = 2**5;
        let ch = channel List[(Int, Bool)] 1;
        let t = 32;
        let _ = spawnPrimeList(n, 2, ch, t);
        <- ch

@benchmark
    def primesToP6T32Spawn(): List[(Int, Bool)] =
        let n = 2**6;
        let ch = channel List[(Int, Bool)] 1;
        let t = 32;
        let _ = spawnPrimeList(n, 2, ch, t);
        <- ch

@benchmark
    def primesToP7T32Spawn(): List[(Int, Bool)] =
        let n = 2**7;
        let ch = channel List[(Int, Bool)] 1;
        let t = 32;
        let _ = spawnPrimeList(n, 2, ch, t);
        <- ch

@benchmark
    def primesToP8T32Spawn(): List[(Int, Bool)] =
        let n = 2**8;
        let ch = channel List[(Int, Bool)] 1;
        let t = 32;
        let _ = spawnPrimeList(n, 2, ch, t);
        <- ch

@benchmark
    def primesToP9T32Spawn(): List[(Int, Bool)] =
        let n = 2**9;
        let ch = channel List[(Int, Bool)] 1;
        let t = 32;
        let _ = spawnPrimeList(n, 2, ch, t);
        <- ch

@benchmark
    def primesToP10T32Spawn(): List[(Int, Bool)] =
        let n = 2**10;
        let ch = channel List[(Int, Bool)] 1;
        let t = 32;
        let _ = spawnPrimeList(n, 2, ch, t);
        <- ch

@benchmark
    def primesToP11T32Spawn(): List[(Int, Bool)] =
        let n = 2**11;
        let ch = channel List[(Int, Bool)] 1;
        let t = 32;
        let _ = spawnPrimeList(n, 2, ch, t);
        <- ch

@benchmark
    def primesToP12T32Spawn(): List[(Int, Bool)] =
        let n = 2**12;
        let ch = channel List[(Int, Bool)] 1;
        let t = 32;
        let _ = spawnPrimeList(n, 2, ch, t);
        <- ch

@benchmark
    def primesToP13T32Spawn(): List[(Int, Bool)] =
        let n = 2**13;
        let ch = channel List[(Int, Bool)] 1;
        let t = 32;
        let _ = spawnPrimeList(n, 2, ch, t);
        <- ch

@benchmark
    def primesToP14T32Spawn(): List[(Int, Bool)] =
        let n = 2**14;
        let ch = channel List[(Int, Bool)] 1;
        let t = 32;
        let _ = spawnPrimeList(n, 2, ch, t);
        <- ch



// 64 Threads
 
@benchmark
    def primesToP1T64Spawn(): List[(Int, Bool)] =
        let n = 2**1;
        let ch = channel List[(Int, Bool)] 1;
        let t = 64;
        let _ = spawnPrimeList(n, 2, ch, t);
        <- ch

@benchmark
    def primesToP2T64Spawn(): List[(Int, Bool)] =
        let n = 2**2;
        let ch = channel List[(Int, Bool)] 1;
        let t = 64;
        let _ = spawnPrimeList(n, 2, ch, t);
        <- ch

@benchmark
    def primesToP3T64Spawn(): List[(Int, Bool)] =
        let n = 2**3;
        let ch = channel List[(Int, Bool)] 1;
        let t = 64;
        let _ = spawnPrimeList(n, 2, ch, t);
        <- ch

@benchmark
    def primesToP4T64Spawn(): List[(Int, Bool)] =
        let n = 2**4;
        let ch = channel List[(Int, Bool)] 1;
        let t = 64;
        let _ = spawnPrimeList(n, 2, ch, t);
        <- ch

@benchmark
    def primesToP5T64Spawn(): List[(Int, Bool)] =
        let n = 2**5;
        let ch = channel List[(Int, Bool)] 1;
        let t = 64;
        let _ = spawnPrimeList(n, 2, ch, t);
        <- ch

@benchmark
    def primesToP6T64Spawn(): List[(Int, Bool)] =
        let n = 2**6;
        let ch = channel List[(Int, Bool)] 1;
        let t = 64;
        let _ = spawnPrimeList(n, 2, ch, t);
        <- ch

@benchmark
    def primesToP7T64Spawn(): List[(Int, Bool)] =
        let n = 2**7;
        let ch = channel List[(Int, Bool)] 1;
        let t = 64;
        let _ = spawnPrimeList(n, 2, ch, t);
        <- ch

@benchmark
    def primesToP8T64Spawn(): List[(Int, Bool)] =
        let n = 2**8;
        let ch = channel List[(Int, Bool)] 1;
        let t = 64;
        let _ = spawnPrimeList(n, 2, ch, t);
        <- ch

@benchmark
    def primesToP9T64Spawn(): List[(Int, Bool)] =
        let n = 2**9;
        let ch = channel List[(Int, Bool)] 1;
        let t = 64;
        let _ = spawnPrimeList(n, 2, ch, t);
        <- ch

@benchmark
    def primesToP10T64Spawn(): List[(Int, Bool)] =
        let n = 2**10;
        let ch = channel List[(Int, Bool)] 1;
        let t = 64;
        let _ = spawnPrimeList(n, 2, ch, t);
        <- ch

@benchmark
    def primesToP11T64Spawn(): List[(Int, Bool)] =
        let n = 2**11;
        let ch = channel List[(Int, Bool)] 1;
        let t = 64;
        let _ = spawnPrimeList(n, 2, ch, t);
        <- ch

@benchmark
    def primesToP12T64Spawn(): List[(Int, Bool)] =
        let n = 2**12;
        let ch = channel List[(Int, Bool)] 1;
        let t = 64;
        let _ = spawnPrimeList(n, 2, ch, t);
        <- ch

@benchmark
    def primesToP13T64Spawn(): List[(Int, Bool)] =
        let n = 2**13;
        let ch = channel List[(Int, Bool)] 1;
        let t = 64;
        let _ = spawnPrimeList(n, 2, ch, t);
        <- ch

@benchmark
    def primesToP14T64Spawn(): List[(Int, Bool)] =
        let n = 2**14;
        let ch = channel List[(Int, Bool)] 1;
        let t = 64;
        let _ = spawnPrimeList(n, 2, ch, t);
        <- ch




// 80 Threads

@benchmark
    def primesToP1T80Spawn(): List[(Int, Bool)] =
        let n = 2**1;
        let ch = channel List[(Int, Bool)] 1;
        let t = 80;
        let _ = spawnPrimeList(n, 2, ch, t);
        <- ch

@benchmark
    def primesToP2T80Spawn(): List[(Int, Bool)] =
        let n = 2**2;
        let ch = channel List[(Int, Bool)] 1;
        let t = 80;
        let _ = spawnPrimeList(n, 2, ch, t);
        <- ch

@benchmark
    def primesToP3T80Spawn(): List[(Int, Bool)] =
        let n = 2**3;
        let ch = channel List[(Int, Bool)] 1;
        let t = 80;
        let _ = spawnPrimeList(n, 2, ch, t);
        <- ch

@benchmark
    def primesToP4T80Spawn(): List[(Int, Bool)] =
        let n = 2**4;
        let ch = channel List[(Int, Bool)] 1;
        let t = 80;
        let _ = spawnPrimeList(n, 2, ch, t);
        <- ch

@benchmark
    def primesToP5T80Spawn(): List[(Int, Bool)] =
        let n = 2**5;
        let ch = channel List[(Int, Bool)] 1;
        let t = 80;
        let _ = spawnPrimeList(n, 2, ch, t);
        <- ch

@benchmark
    def primesToP6T80Spawn(): List[(Int, Bool)] =
        let n = 2**6;
        let ch = channel List[(Int, Bool)] 1;
        let t = 80;
        let _ = spawnPrimeList(n, 2, ch, t);
        <- ch

@benchmark
    def primesToP7T80Spawn(): List[(Int, Bool)] =
        let n = 2**7;
        let ch = channel List[(Int, Bool)] 1;
        let t = 80;
        let _ = spawnPrimeList(n, 2, ch, t);
        <- ch

@benchmark
    def primesToP8T80Spawn(): List[(Int, Bool)] =
        let n = 2**8;
        let ch = channel List[(Int, Bool)] 1;
        let t = 80;
        let _ = spawnPrimeList(n, 2, ch, t);
        <- ch

@benchmark
    def primesToP9T80Spawn(): List[(Int, Bool)] =
        let n = 2**9;
        let ch = channel List[(Int, Bool)] 1;
        let t = 80;
        let _ = spawnPrimeList(n, 2, ch, t);
        <- ch

@benchmark
    def primesToP10T80Spawn(): List[(Int, Bool)] =
        let n = 2**10;
        let ch = channel List[(Int, Bool)] 1;
        let t = 80;
        let _ = spawnPrimeList(n, 2, ch, t);
        <- ch

@benchmark
    def primesToP11T80Spawn(): List[(Int, Bool)] =
        let n = 2**11;
        let ch = channel List[(Int, Bool)] 1;
        let t = 80;
        let _ = spawnPrimeList(n, 2, ch, t);
        <- ch

@benchmark
    def primesToP12T80Spawn(): List[(Int, Bool)] =
        let n = 2**12;
        let ch = channel List[(Int, Bool)] 1;
        let t = 80;
        let _ = spawnPrimeList(n, 2, ch, t);
        <- ch

@benchmark
    def primesToP13T80Spawn(): List[(Int, Bool)] =
        let n = 2**13;
        let ch = channel List[(Int, Bool)] 1;
        let t = 80;
        let _ = spawnPrimeList(n, 2, ch, t);
        <- ch

@benchmark
    def primesToP14T80Spawn(): List[(Int, Bool)] =
        let n = 2**14;
        let ch = channel List[(Int, Bool)] 1;
        let t = 80;
        let _ = spawnPrimeList(n, 2, ch, t);
        <- ch

*/

// Umlimited Threads
/*
@benchmark
    def primesToP1TUnlimitedSpawn(): List[(Int, Bool)] =
        let n = 2**1;
        let ch = channel List[(Int, Bool)] 1;
        let _ = spawnPrimeListUnLimited(n, 2, ch);
        <- ch

@benchmark
    def primesToP2TUnlimitedSpawn(): List[(Int, Bool)] =
        let n = 2**2;
        let ch = channel List[(Int, Bool)] 1;
        let t = 64;
        let _ = spawnPrimeListUnLimited(n, 2, ch);
        <- ch

@benchmark
    def primesToP3TUnlimitedSpawn(): List[(Int, Bool)] =
        let n = 2**3;
        let ch = channel List[(Int, Bool)] 1;
        let t = 64;
        let _ = spawnPrimeListUnLimited(n, 2, ch);
        <- ch

@benchmark
    def primesToP4TUnlimitedSpawn(): List[(Int, Bool)] =
        let n = 2**4;
        let ch = channel List[(Int, Bool)] 1;
        let t = 64;
        let _ = spawnPrimeListUnLimited(n, 2, ch);
        <- ch

@benchmark
    def primesToP5TUnlimitedSpawn(): List[(Int, Bool)] =
        let n = 2**5;
        let ch = channel List[(Int, Bool)] 1;
        let t = 64;
        let _ = spawnPrimeListUnLimited(n, 2, ch);
        <- ch

@benchmark
    def primesToP6TUnlimitedSpawn(): List[(Int, Bool)] =
        let n = 2**6;
        let ch = channel List[(Int, Bool)] 1;
        let t = 64;
        let _ = spawnPrimeListUnLimited(n, 2, ch);
        <- ch

@benchmark
    def primesToP7TUnlimitedSpawn(): List[(Int, Bool)] =
        let n = 2**7;
        let ch = channel List[(Int, Bool)] 1;
        let t = 64;
        let _ = spawnPrimeListUnLimited(n, 2, ch);
        <- ch

@benchmark
    def primesToP8TUnlimitedSpawn(): List[(Int, Bool)] =
        let n = 2**8;
        let ch = channel List[(Int, Bool)] 1;
        let t = 64;
        let _ = spawnPrimeListUnLimited(n, 2, ch);
        <- ch

@benchmark
    def primesToP9TUnlimitedSpawn(): List[(Int, Bool)] =
        let n = 2**9;
        let ch = channel List[(Int, Bool)] 1;
        let t = 64;
        let _ = spawnPrimeListUnLimited(n, 2, ch);
        <- ch

@benchmark
    def primesToP10TUnlimitedSpawn(): List[(Int, Bool)] =
        let n = 2**10;
        let ch = channel List[(Int, Bool)] 1;
        let t = 64;
        let _ = spawnPrimeListUnLimited(n, 2, ch);
        <- ch

@benchmark
    def primesToP11TUnlimitedSpawn(): List[(Int, Bool)] =
        let n = 2**11;
        let ch = channel List[(Int, Bool)] 1;
        let t = 64;
        let _ = spawnPrimeListUnLimited(n, 2, ch);
        <- ch

@benchmark
    def primesToP12TUnlimitedSpawn(): List[(Int, Bool)] =
        let n = 2**12;
        let ch = channel List[(Int, Bool)] 1;
        let t = 64;
        let _ = spawnPrimeListUnLimited(n, 2, ch);
        <- ch

@benchmark
    def primesToP13TUnlimitedSpawn(): List[(Int, Bool)] =
        let n = 2**13;
        let ch = channel List[(Int, Bool)] 1;
        let t = 64;
        let _ = spawnPrimeListUnLimited(n, 2, ch);
        <- ch

@benchmark
    def primesToP14TUnlimitedSpawn(): List[(Int, Bool)] =
        let n = 2**14;
        let ch = channel List[(Int, Bool)] 1;
        let t = 64;
        let _ = spawnPrimeListUnLimited(n, 2, ch);
        <- ch
*/




/*
@benchmark
    def primesToP7Spawn(): List[(Int, Bool)] =
        let ch1 = channel List[(Int, Bool)] 1;
        let ch2 = channel List[(Int, Bool)] 1;
        let ch3 = channel List[(Int, Bool)] 1;
        let ch4 = channel List[(Int, Bool)] 1;
        let ch5 = channel List[(Int, Bool)] 1;
        let ch6 = channel List[(Int, Bool)] 1;
        let ch7 = channel List[(Int, Bool)] 1;
        let ch8 = channel List[(Int, Bool)] 1;
        let ch9 = channel List[(Int, Bool)] 1;
        let ch10 = channel List[(Int, Bool)] 1;
        let numbers1 = List.range(2, 2**7);
        let numbers2 = List.range(2**7 * 2, 2**7 * 3);
        let numbers3 = List.range(2**7 * 3, 2**7 * 4);
        let numbers4 = List.range(2**7 * 4, 2**7 * 5);
        let numbers5 = List.range(2**7 * 5, 2**7 * 6);
        let numbers6 = List.range(2**7 * 6, 2**7 * 7);
        let numbers7 = List.range(2**7 * 7, 2**7 * 8);
        let numbers8 = List.range(2**7 * 8, 2**7 * 9);
        let numbers9 = List.range(2**7 * 9, 2**7 * 10);
        let numbers10 = List.range(2**7 * 10, 2**7 * 11);
        let _ = spawn spawnPrimeList(numbers1, 5, ch1);
        let _ = spawn spawnPrimeList(numbers2, 5, ch2);
        let _ = spawn spawnPrimeList(numbers3, 5, ch3);
        let _ = spawn spawnPrimeList(numbers4, 5, ch4);
        let _ = spawn spawnPrimeList(numbers5, 5, ch5);
        let _ = spawn spawnPrimeList(numbers6, 5, ch6);
        let _ = spawn spawnPrimeList(numbers7, 5, ch7);
        let _ = spawn spawnPrimeList(numbers8, 5, ch8);
        let _ = spawn spawnPrimeList(numbers9, 5, ch9);
        let _ = spawn spawnPrimeList(numbers10, 5, ch10);
        let _ = <- ch1;
        let _ = <- ch2;
        let _ = <- ch3;
        let _ = <- ch4;
        let _ = <- ch5;
        let _ = <- ch6;
        let _ = <- ch7;
        let _ = <- ch8;
        let _ = <- ch9;
        <- ch10

@benchmark
    def primesToP7(): List[(Int, Bool)] =
        let numbers1 = List.range(2, 2**7);
        let numbers2 = List.range(2**7 * 2, 2**7 * 3);
        let numbers3 = List.range(2**7 * 3, 2**7 * 4);
        let numbers4 = List.range(2**7 * 4, 2**7 * 5);
        let numbers5 = List.range(2**7 * 5, 2**7 * 6);
        let numbers6 = List.range(2**7 * 6, 2**7 * 7);
        let numbers7 = List.range(2**7 * 7, 2**7 * 8);
        let numbers8 = List.range(2**7 * 8, 2**7 * 9);
        let numbers9 = List.range(2**7 * 9, 2**7 * 10);
        let numbers10 = List.range(2**7 * 10, 2**7 * 11);
        let _ = primeList(numbers1, 1);
        let _ = primeList(numbers2, 2);
        let _ = primeList(numbers3, 3);
        let _ = primeList(numbers4, 4);
        let _ = primeList(numbers5, 5);
        let _ = primeList(numbers6, 6);
        let _ = primeList(numbers7, 7);
        let _ = primeList(numbers8, 8);
        let _ = primeList(numbers9, 9);
        primeList(numbers10, 10)
*/