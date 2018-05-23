def fac(n: Int): Int = if (n == 0) 1 else n * fac(n - 1)

def facs(l: List[Int]): List[Int] = List.map((a) -> fac(a), l)

def facs_ch(l: List[Int], ch: Channel[List[Int]]): Unit =
    let x = facs(l);
    ch <- x;
    ()

@benchmark
def dualSeqBenchmarkLow(): Unit =
    let list = List.Map((x) -> x * 2, List.range(1, 20));
    let _ = facs(list);
    let _ = facs(list);
    ()

@benchmark
def dualSeqBenchmarkMed(): Unit =
    let list = List.range(50, 70);
    let _ = facs(list);
    let _ = facs(list);
    ()

@benchmark
def dualSeqBenchmarkHigh(): Unit =
    let list = List.range(100, 120);
    let _ = facs(list);
    let _ = facs(list);
    ()

@benchmark
def quadSeqBenchmarkLow(): Unit =
    let list = List.range(1, 20);
    let _ = facs(list);
    let _ = facs(list);
    let _ = facs(list);
    let _ = facs(list);
    ()

@benchmark
def quadSeqBenchmarkMed(): Unit =
    let list = List.range(50, 70);
    let _ = facs(list);
    let _ = facs(list);
    let _ = facs(list);
    let _ = facs(list);
    ()

@benchmark
def quadSeqBenchmarkHigh(): Unit =
    let list = List.range(100, 120);
    let _ = facs(list);
    let _ = facs(list);
    let _ = facs(list);
    let _ = facs(list);
    ()

@benchmark
def octaSeqBenchmarkLow(): Unit =
    let list = List.range(1, 20);
    let _ = facs(list);
    let _ = facs(list);
    let _ = facs(list);
    let _ = facs(list);
    let _ = facs(list);
    let _ = facs(list);
    let _ = facs(list);
    let _ = facs(list);
    ()

@benchmark
def octaSeqBenchmarkMed(): Unit =
    let list = List.range(50, 70);
    let _ = facs(list);
    let _ = facs(list);
    let _ = facs(list);
    let _ = facs(list);
    let _ = facs(list);
    let _ = facs(list);
    let _ = facs(list);
    let _ = facs(list);
    ()

@benchmark
def octaSeqBenchmarkHigh(): Unit =
    let list = List.range(100, 120);
    let _ = facs(list);
    let _ = facs(list);
    let _ = facs(list);
    let _ = facs(list);
    let _ = facs(list);
    let _ = facs(list);
    let _ = facs(list);
    let _ = facs(list);
    ()

@benchmark
def hexadecaSeqBenchmarkLow(): Unit =
    let list = List.range(1, 20);
    let _ = facs(list);
    let _ = facs(list);
    let _ = facs(list);
    let _ = facs(list);
    let _ = facs(list);
    let _ = facs(list);
    let _ = facs(list);
    let _ = facs(list);
    let _ = facs(list);
    let _ = facs(list);
    let _ = facs(list);
    let _ = facs(list);
    let _ = facs(list);
    let _ = facs(list);
    let _ = facs(list);
    let _ = facs(list);
    ()

@benchmark
def hexadecaSeqBenchmarkMed(): Unit =
    let list = List.range(50, 70);
    let _ = facs(list);
    let _ = facs(list);
    let _ = facs(list);
    let _ = facs(list);
    let _ = facs(list);
    let _ = facs(list);
    let _ = facs(list);
    let _ = facs(list);
    let _ = facs(list);
    let _ = facs(list);
    let _ = facs(list);
    let _ = facs(list);
    let _ = facs(list);
    let _ = facs(list);
    let _ = facs(list);
    let _ = facs(list);
    ()

@benchmark
def hexadecaSeqBenchmarkHigh(): Unit =
    let list = List.range(100, 120);
    let _ = facs(list);
    let _ = facs(list);
    let _ = facs(list);
    let _ = facs(list);
    let _ = facs(list);
    let _ = facs(list);
    let _ = facs(list);
    let _ = facs(list);
    let _ = facs(list);
    let _ = facs(list);
    let _ = facs(list);
    let _ = facs(list);
    let _ = facs(list);
    let _ = facs(list);
    let _ = facs(list);
    let _ = facs(list);
    ()

@benchmark
def dualThreadBenchmarkLow(): Unit =
    let ch = channel List[Int] 2;
    let list = List.range(1, 20);
    let _ = spawn facs_ch(list, ch);
    let _ = spawn facs_ch(list, ch);
    let _ = <- ch;
    let _ = <- ch;
    ()

@benchmark
def dualThreadBenchmarkMed(): Unit =
    let ch = channel List[Int] 2;
    let list = List.range(20, 70);
    let _ = spawn facs_ch(list, ch);
    let _ = spawn facs_ch(list, ch);
    let _ = <- ch;
    let _ = <- ch;
    ()

@benchmark
def dualThreadBenchmarkHigh(): Unit =
    let ch = channel List[Int] 2;
    let list = List.range(100, 120);
    let _ = spawn facs_ch(list, ch);
    let _ = spawn facs_ch(list, ch);
    let _ = <- ch;
    let _ = <- ch;
    ()

@benchmark
def quadThreadBenchmarkLow(): Unit =
    let ch = channel List[Int] 4;
    let list = List.range(1, 20);
    let _ = spawn facs_ch(list, ch);
    let _ = spawn facs_ch(list, ch);
    let _ = spawn facs_ch(list, ch);
    let _ = spawn facs_ch(list, ch);
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    ()

@benchmark
def quadThreadBenchmarkMed(): Unit =
    let ch = channel List[Int] 4;
    let list = List.range(50, 70);
    let _ = spawn facs_ch(list, ch);
    let _ = spawn facs_ch(list, ch);
    let _ = spawn facs_ch(list, ch);
    let _ = spawn facs_ch(list, ch);
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    ()

@benchmark
def quadThreadBenchmarkHigh(): Unit =
    let ch = channel List[Int] 4;
    let list = List.range(100, 120);
    let _ = spawn facs_ch(list, ch);
    let _ = spawn facs_ch(list, ch);
    let _ = spawn facs_ch(list, ch);
    let _ = spawn facs_ch(list, ch);
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    let _ = <- ch;
    ()

@benchmark
def octaThreadBenchmarkLow(): Unit =
    let ch = channel List[Int] 8;
    let list = List.range(1, 20);
    let _ = spawn facs_ch(list, ch);
    let _ = spawn facs_ch(list, ch);
    let _ = spawn facs_ch(list, ch);
    let _ = spawn facs_ch(list, ch);
    let _ = spawn facs_ch(list, ch);
    let _ = spawn facs_ch(list, ch);
    let _ = spawn facs_ch(list, ch);
    let _ = spawn facs_ch(list, ch);
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
def octaThreadBenchmarkMed(): Unit =
    let ch = channel List[Int] 8;
    let list = List.range(50, 70);
    let _ = spawn facs_ch(list, ch);
    let _ = spawn facs_ch(list, ch);
    let _ = spawn facs_ch(list, ch);
    let _ = spawn facs_ch(list, ch);
    let _ = spawn facs_ch(list, ch);
    let _ = spawn facs_ch(list, ch);
    let _ = spawn facs_ch(list, ch);
    let _ = spawn facs_ch(list, ch);
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
def octaThreadBenchmarkHigh(): Unit =
    let ch = channel List[Int] 8;
    let list = List.range(100, 120);
    let _ = spawn facs_ch(list, ch);
    let _ = spawn facs_ch(list, ch);
    let _ = spawn facs_ch(list, ch);
    let _ = spawn facs_ch(list, ch);
    let _ = spawn facs_ch(list, ch);
    let _ = spawn facs_ch(list, ch);
    let _ = spawn facs_ch(list, ch);
    let _ = spawn facs_ch(list, ch);
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
def hexadecaThreadBenchmarkLow(): Unit =
    let ch = channel List[Int] 16;
    let list = List.range(1, 20);
    let _ = spawn facs_ch(list, ch);
    let _ = spawn facs_ch(list, ch);
    let _ = spawn facs_ch(list, ch);
    let _ = spawn facs_ch(list, ch);
    let _ = spawn facs_ch(list, ch);
    let _ = spawn facs_ch(list, ch);
    let _ = spawn facs_ch(list, ch);
    let _ = spawn facs_ch(list, ch);
    let _ = spawn facs_ch(list, ch);
    let _ = spawn facs_ch(list, ch);
    let _ = spawn facs_ch(list, ch);
    let _ = spawn facs_ch(list, ch);
    let _ = spawn facs_ch(list, ch);
    let _ = spawn facs_ch(list, ch);
    let _ = spawn facs_ch(list, ch);
    let _ = spawn facs_ch(list, ch);
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
def hexadecaThreadBenchmarkMed(): Unit =
    let ch = channel List[Int] 16;
    let list = List.range(50, 70);
    let _ = spawn facs_ch(list, ch);
    let _ = spawn facs_ch(list, ch);
    let _ = spawn facs_ch(list, ch);
    let _ = spawn facs_ch(list, ch);
    let _ = spawn facs_ch(list, ch);
    let _ = spawn facs_ch(list, ch);
    let _ = spawn facs_ch(list, ch);
    let _ = spawn facs_ch(list, ch);
    let _ = spawn facs_ch(list, ch);
    let _ = spawn facs_ch(list, ch);
    let _ = spawn facs_ch(list, ch);
    let _ = spawn facs_ch(list, ch);
    let _ = spawn facs_ch(list, ch);
    let _ = spawn facs_ch(list, ch);
    let _ = spawn facs_ch(list, ch);
    let _ = spawn facs_ch(list, ch);
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
def hexadecaThreadBenchmarkHigh(): Unit =
    let ch = channel List[Int] 16;
    let list = List.range(100, 120);
    let _ = spawn facs_ch(list, ch);
    let _ = spawn facs_ch(list, ch);
    let _ = spawn facs_ch(list, ch);
    let _ = spawn facs_ch(list, ch);
    let _ = spawn facs_ch(list, ch);
    let _ = spawn facs_ch(list, ch);
    let _ = spawn facs_ch(list, ch);
    let _ = spawn facs_ch(list, ch);
    let _ = spawn facs_ch(list, ch);
    let _ = spawn facs_ch(list, ch);
    let _ = spawn facs_ch(list, ch);
    let _ = spawn facs_ch(list, ch);
    let _ = spawn facs_ch(list, ch);
    let _ = spawn facs_ch(list, ch);
    let _ = spawn facs_ch(list, ch);
    let _ = spawn facs_ch(list, ch);
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

