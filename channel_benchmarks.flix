def fillChannel(ch: Channel[Int], n: Int): Unit =
    let _ = List.map((x) -> ch <- x, List.range(0, n));
    ()
def emptyChannel(ch: Channel[Int], n: Int): Unit =
    let _ = List.map((x) -> <- ch, List.range(0, n));
    ()

// 01 - Benchmark
@benchmark
def benchmark01(): Unit =
    let n = 120;
    let ch = channel Int n;
    let _ = fillChannel(ch, 120);
    let _ = emptyChannel(ch, 60);
    let _ = fillChannel(ch, 60);
    let _ = emptyChannel(ch, 120);
    ()
