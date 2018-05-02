def a(): Unit =
    //Console/StdOut.print("a()");
    ()
/*
def b(): Int =
    //Console/StdOut.print("b()");
    2 + 2

def c(i: Int): Int =
    //Console/StdOut.print("c()");
    i + i
*/

def main(): Unit =
    //Console/StdOut.print("main()");
    spawn a();
    //spawn b();
    //spawn c(2);
    ()
