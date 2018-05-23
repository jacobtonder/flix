namespace BenchmarkSpawn {
    def merge(left: List[Int], right: List[Int]): List[Int] =
        match (left, right) with {
            case (left, Nil) => left
            case (Nil, right) => right
            case (left, right) =>
                let leftHead = Option.getWithDefault(List.head(left), -1);
                let leftTail = List.drop(1, left);
                let rightHead = Option.getWithDefault(List.head(right), -1);
                let rightTail = List.drop(1, right);
                if (leftHead < rightHead) leftHead :: merge(leftTail, right)
                else rightHead :: merge(left, rightTail)
        }

    def mergeSort(list: List[Int]): List[Int] = {
        let n = List.length(list) / 2;
        if (n == 0) list
        else {
            let left = List.take(n, list);
            let right = List.drop(n, list);
            merge(mergeSort(left), mergeSort(right))
            }
        }

    def mergeSpawn(left: List[Int], right: List[Int], ch: Channel[List[Int]]): Unit = {
        let newCh = channel List[Int] 1;
        match (left, right) with {
            case (left, Nil) =>
                let _ = ch <- left;
                ()
            case (Nil, right) =>
                let _ = ch <- right;
                ()
            case (left, right) =>
                let leftHead = Option.getWithDefault(List.head(left), -1);
                let leftTail = List.drop(1, left);
                let rightHead = Option.getWithDefault(List.head(right), -1);
                let rightTail = List.drop(1, right);
                if (leftHead < rightHead)
                    let _ = mergeSpawn(leftTail, right, newCh);
                    let _ = ch <- leftHead :: <- newCh;
                    ()

                else {
                    let _ = mergeSpawn(left, rightTail, newCh);
                    let _ = ch <- rightHead :: <- newCh;
                    ()
                }
        }
    }

    def mergeSortSpawn(list: List[Int], ch: Channel[List[Int]], t: Int): Unit = {
        let n = List.length(list) / 2;
        let chLeft = channel List[Int] 1;
        let chRight = channel List[Int] 1;
        let newCh = channel List[Int] 1;
        if (n == 0)
            let _ = ch <- list;
            ()
        else {
            let left = List.take(n, list);
            let right = List.drop(n, list);
            if (t > 2)
                let t = t - 2;
                let _ = spawn mergeSortSpawn(left, chLeft, t);
                let _ = spawn mergeSortSpawn(right, chRight, t);
                let _ = mergeSpawn(<- chLeft, <- chRight, newCh);
                let _ = ch <- <- newCh;
                ()
            else if (t > 1)
                let t = t - 1;
                let _ = spawn mergeSortSpawn(left, chLeft, t);
                let _ = mergeSortSpawn(right, chRight, t);
                let _ = mergeSpawn(<- chLeft, <- chRight, newCh);
                let _ = ch <- <- newCh;
                ()
            else
                let _ = mergeSortSpawn(left, chLeft, t);
                let _ = mergeSortSpawn(right, chRight, t);
                let _ = mergeSpawn(<- chLeft, <- chRight, newCh);
                let _ = ch <- <- newCh;
                ()
            }
        }


    def genUnsortedList(n: Int): List[Int] =
        List.reverse(List.range(1, n))



// Sequential

    @benchmark
    def seq0(): List[Int] =
        let n = 2 ** 0;
        let list = genUnsortedList(n);
        mergeSort(list)

    @benchmark
    def seq1(): List[Int] =
        let n = 2 ** 1;
        let list = genUnsortedList(n);
        mergeSort(list)

    @benchmark
    def seq2(): List[Int] =
        let n = 2 ** 2;
        let list = genUnsortedList(n);
        mergeSort(list)

    @benchmark
    def seq3(): List[Int] =
        let n = 2 ** 3;
        let list = genUnsortedList(n);
        mergeSort(list)

    @benchmark
    def seq4(): List[Int] =
        let n = 2 ** 4;
        let list = genUnsortedList(n);
        mergeSort(list)

    @benchmark
    def seq5(): List[Int] =
        let n = 2 ** 5;
        let list = genUnsortedList(n);
        mergeSort(list)

    @benchmark
    def seq6(): List[Int] =
        let n = 2 ** 6;
        let list = genUnsortedList(n);
        mergeSort(list)

    @benchmark
    def seq7(): List[Int] =
        let n = 2 ** 7;
        let list = genUnsortedList(n);
        mergeSort(list)

    @benchmark
    def seq8(): List[Int] =
        let n = 2 ** 8;
        let list = genUnsortedList(n);
        mergeSort(list)

    @benchmark
    def seq9(): List[Int] =
        let n = 2 ** 9;
        let list = genUnsortedList(n);
        mergeSort(list)

    @benchmark
    def seq10(): List[Int] =
        let n = 2 ** 10;
        let list = genUnsortedList(n);
        mergeSort(list)

    @benchmark
    def seq11(): List[Int] =
        let n = 2 ** 11;
        let list = genUnsortedList(n);
        mergeSort(list)

    @benchmark
    def seq12(): List[Int] =
        let n = 2 ** 12;
        let list = genUnsortedList(n);
        mergeSort(list)

    @benchmark
    def seq13(): List[Int] =
        let n = 2 ** 13;
        let list = genUnsortedList(n);
        mergeSort(list)
/*
    @benchmark
    def seq14(): List[Int] =
        let n = 2 ** 14;
        let list = genUnsortedList(n);
        mergeSort(list)

    @benchmark
    def seq15(): List[Int] =
        let n = 2 ** 15;
        let list = genUnsortedList(n);
        mergeSort(list)

    @benchmark
    def seq16(): List[Int] =
        let n = 2 ** 16;
        let list = genUnsortedList(n);
        mergeSort(list)

    @benchmark
    def seq17(): List[Int] =
        let n = 2 ** 17;
        let list = genUnsortedList(n);
        mergeSort(list)

    @benchmark
    def seq18(): List[Int] =
        let n = 2 ** 18;
        let list = genUnsortedList(n);
        mergeSort(list)

    @benchmark
    def seq19(): List[Int] =
        let n = 2 ** 19;
        let list = genUnsortedList(n);
        mergeSort(list)

    @benchmark
    def seq20(): List[Int] =
        let n = 2 ** 20;
        let list = genUnsortedList(n);
        mergeSort(list)
*/


// 1 Thread

    @benchmark
    def sortP0T1(): List[Int] =
        let n = 2 ** 0;
        let list = genUnsortedList(n);
        let ch = channel List[Int] 1;
        let t = 1;
        let _ = mergeSortSpawn(list, ch, t);
        <- ch

    @benchmark
    def sortP1T1(): List[Int] =
        let n = 2 ** 1;
        let list = genUnsortedList(n);
        let ch = channel List[Int] 1;
        let t = 1;
        let _ = mergeSortSpawn(list, ch, t);
        <- ch

    @benchmark
    def sortP2T1(): List[Int] =
        let n = 2 ** 2;
        let list = genUnsortedList(n);
        let ch = channel List[Int] 1;
        let t = 1;
        let _ = mergeSortSpawn(list, ch, t);
        <- ch

    @benchmark
    def sortP3T1(): List[Int] =
        let n = 2 ** 3;
        let list = genUnsortedList(n);
        let ch = channel List[Int] 1;
        let t = 1;
        let _ = mergeSortSpawn(list, ch, t);
        <- ch

    @benchmark
    def sortP4T1(): List[Int] =
        let n = 2 ** 4;
        let list = genUnsortedList(n);
        let ch = channel List[Int] 1;
        let t = 1;
        let _ = mergeSortSpawn(list, ch, t);
        <- ch

    @benchmark
    def sortP5T1(): List[Int] =
        let n = 2 ** 5;
        let list = genUnsortedList(n);
        let ch = channel List[Int] 1;
        let t = 1;
        let _ = mergeSortSpawn(list, ch, t);
        <- ch

    @benchmark
    def sortP6T1(): List[Int] =
        let n = 2 ** 6;
        let list = genUnsortedList(n);
        let ch = channel List[Int] 1;
        let t = 1;
        let _ = mergeSortSpawn(list, ch, t);
        <- ch

    @benchmark
    def sortP7T1(): List[Int] =
        let n = 2 ** 7;
        let list = genUnsortedList(n);
        let ch = channel List[Int] 1;
        let t = 1;
        let _ = mergeSortSpawn(list, ch, t);
        <- ch

    @benchmark
    def sortP8T1(): List[Int] =
        let n = 2 ** 8;
        let list = genUnsortedList(n);
        let ch = channel List[Int] 1;
        let t = 1;
        let _ = mergeSortSpawn(list, ch, t);
        <- ch

    @benchmark
    def sortP9T1(): List[Int] =
        let n = 2 ** 9;
        let list = genUnsortedList(n);
        let ch = channel List[Int] 1;
        let t = 1;
        let _ = mergeSortSpawn(list, ch, t);
        <- ch

    @benchmark
    def sortP10T1(): List[Int] =
        let n = 2 ** 10;
        let list = genUnsortedList(n);
        let ch = channel List[Int] 1;
        let t = 1;
        let _ = mergeSortSpawn(list, ch, t);
        <- ch

    @benchmark
    def sortP11T1(): List[Int] =
        let n = 2 ** 11;
        let list = genUnsortedList(n);
        let ch = channel List[Int] 1;
        let t = 1;
        let _ = mergeSortSpawn(list, ch, t);
        <- ch

    @benchmark
    def sortP12T1(): List[Int] =
        let n = 2 ** 12;
        let list = genUnsortedList(n);
        let ch = channel List[Int] 1;
        let t = 1;
        let _ = mergeSortSpawn(list, ch, t);
        <- ch

    @benchmark
    def sortP13T1(): List[Int] =
        let n = 2 ** 13;
        let list = genUnsortedList(n);
        let ch = channel List[Int] 1;
        let t = 1;
        let _ = mergeSortSpawn(list, ch, t);
        <- ch

        

    // 2 Threads
    
    @benchmark
    def sortP0T2(): List[Int] =
        let n = 2 ** 0;
        let list = genUnsortedList(n);
        let ch = channel List[Int] 1;
        let t = 2;
        let _ = mergeSortSpawn(list, ch, t);
        <- ch

    @benchmark
    def sortP1T2(): List[Int] =
        let n = 2 ** 1;
        let list = genUnsortedList(n);
        let ch = channel List[Int] 1;
        let t = 2;
        let _ = mergeSortSpawn(list, ch, t);
        <- ch

    @benchmark
    def sortP2T2(): List[Int] =
        let n = 2 ** 2;
        let list = genUnsortedList(n);
        let ch = channel List[Int] 1;
        let t = 2;
        let _ = mergeSortSpawn(list, ch, t);
        <- ch

    @benchmark
    def sortP3T2(): List[Int] =
        let n = 2 ** 3;
        let list = genUnsortedList(n);
        let ch = channel List[Int] 1;
        let t = 2;
        let _ = mergeSortSpawn(list, ch, t);
        <- ch

    @benchmark
    def sortP4T2(): List[Int] =
        let n = 2 ** 4;
        let list = genUnsortedList(n);
        let ch = channel List[Int] 1;
        let t = 2;
        let _ = mergeSortSpawn(list, ch, t);
        <- ch

    @benchmark
    def sortP5T2(): List[Int] =
        let n = 2 ** 5;
        let list = genUnsortedList(n);
        let ch = channel List[Int] 1;
        let t = 2;
        let _ = mergeSortSpawn(list, ch, t);
        <- ch

    @benchmark
    def sortP6T2(): List[Int] =
        let n = 2 ** 6;
        let list = genUnsortedList(n);
        let ch = channel List[Int] 1;
        let t = 2;
        let _ = mergeSortSpawn(list, ch, t);
        <- ch

    @benchmark
    def sortP7T2(): List[Int] =
        let n = 2 ** 7;
        let list = genUnsortedList(n);
        let ch = channel List[Int] 1;
        let t = 2;
        let _ = mergeSortSpawn(list, ch, t);
        <- ch

    @benchmark
    def sortP8T2(): List[Int] =
        let n = 2 ** 8;
        let list = genUnsortedList(n);
        let ch = channel List[Int] 1;
        let t = 2;
        let _ = mergeSortSpawn(list, ch, t);
        <- ch

    @benchmark
    def sortP9T2(): List[Int] =
        let n = 2 ** 9;
        let list = genUnsortedList(n);
        let ch = channel List[Int] 1;
        let t = 2;
        let _ = mergeSortSpawn(list, ch, t);
        <- ch

    @benchmark
    def sortP10T2(): List[Int] =
        let n = 2 ** 10;
        let list = genUnsortedList(n);
        let ch = channel List[Int] 1;
        let t = 2;
        let _ = mergeSortSpawn(list, ch, t);
        <- ch

    @benchmark
    def sortP11T2(): List[Int] =
        let n = 2 ** 11;
        let list = genUnsortedList(n);
        let ch = channel List[Int] 1;
        let t = 2;
        let _ = mergeSortSpawn(list, ch, t);
        <- ch

    @benchmark
    def sortP12T2(): List[Int] =
        let n = 2 ** 12;
        let list = genUnsortedList(n);
        let ch = channel List[Int] 1;
        let t = 2;
        let _ = mergeSortSpawn(list, ch, t);
        <- ch

    @benchmark
    def sortP13T2(): List[Int] =
        let n = 2 ** 13;
        let list = genUnsortedList(n);
        let ch = channel List[Int] 1;
        let t = 2;
        let _ = mergeSortSpawn(list, ch, t);
        <- ch


    // 4 Threads
    
    @benchmark
    def sortP0T4(): List[Int] =
        let n = 2 ** 0;
        let list = genUnsortedList(n);
        let ch = channel List[Int] 1;
        let t = 4;
        let _ = mergeSortSpawn(list, ch, t);
        <- ch

    @benchmark
    def sortP1T4(): List[Int] =
        let n = 2 ** 1;
        let list = genUnsortedList(n);
        let ch = channel List[Int] 1;
        let t = 4;
        let _ = mergeSortSpawn(list, ch, t);
        <- ch

    @benchmark
    def sortP2T4(): List[Int] =
        let n = 2 ** 2;
        let list = genUnsortedList(n);
        let ch = channel List[Int] 1;
        let t = 4;
        let _ = mergeSortSpawn(list, ch, t);
        <- ch

    @benchmark
    def sortP3T4(): List[Int] =
        let n = 2 ** 3;
        let list = genUnsortedList(n);
        let ch = channel List[Int] 1;
        let t = 4;
        let _ = mergeSortSpawn(list, ch, t);
        <- ch

    @benchmark
    def sortP4T4(): List[Int] =
        let n = 2 ** 4;
        let list = genUnsortedList(n);
        let ch = channel List[Int] 1;
        let t = 4;
        let _ = mergeSortSpawn(list, ch, t);
        <- ch

    @benchmark
    def sortP5T4(): List[Int] =
        let n = 2 ** 5;
        let list = genUnsortedList(n);
        let ch = channel List[Int] 1;
        let t = 4;
        let _ = mergeSortSpawn(list, ch, t);
        <- ch

    @benchmark
    def sortP6T4(): List[Int] =
        let n = 2 ** 6;
        let list = genUnsortedList(n);
        let ch = channel List[Int] 1;
        let t = 4;
        let _ = mergeSortSpawn(list, ch, t);
        <- ch

    @benchmark
    def sortP7T4(): List[Int] =
        let n = 2 ** 7;
        let list = genUnsortedList(n);
        let ch = channel List[Int] 1;
        let t = 4;
        let _ = mergeSortSpawn(list, ch, t);
        <- ch

    @benchmark
    def sortP8T4(): List[Int] =
        let n = 2 ** 8;
        let list = genUnsortedList(n);
        let ch = channel List[Int] 1;
        let t = 4;
        let _ = mergeSortSpawn(list, ch, t);
        <- ch

    @benchmark
    def sortP9T4(): List[Int] =
        let n = 2 ** 9;
        let list = genUnsortedList(n);
        let ch = channel List[Int] 1;
        let t = 4;
        let _ = mergeSortSpawn(list, ch, t);
        <- ch

    @benchmark
    def sortP10T4(): List[Int] =
        let n = 2 ** 10;
        let list = genUnsortedList(n);
        let ch = channel List[Int] 1;
        let t = 4;
        let _ = mergeSortSpawn(list, ch, t);
        <- ch

    @benchmark
    def sortP11T4(): List[Int] =
        let n = 2 ** 11;
        let list = genUnsortedList(n);
        let ch = channel List[Int] 1;
        let t = 4;
        let _ = mergeSortSpawn(list, ch, t);
        <- ch

    @benchmark
    def sortP12T4(): List[Int] =
        let n = 2 ** 12;
        let list = genUnsortedList(n);
        let ch = channel List[Int] 1;
        let t = 4;
        let _ = mergeSortSpawn(list, ch, t);
        <- ch

    @benchmark
    def sortP13T4(): List[Int] =
        let n = 2 ** 13;
        let list = genUnsortedList(n);
        let ch = channel List[Int] 1;
        let t = 4;
        let _ = mergeSortSpawn(list, ch, t);
        <- ch


    // 8 Threads
    
    @benchmark
    def sortP0T8(): List[Int] =
        let n = 2 ** 0;
        let list = genUnsortedList(n);
        let ch = channel List[Int] 1;
        let t = 8;
        let _ = mergeSortSpawn(list, ch, t);
        <- ch

    @benchmark
    def sortP1T8(): List[Int] =
        let n = 2 ** 1;
        let list = genUnsortedList(n);
        let ch = channel List[Int] 1;
        let t = 8;
        let _ = mergeSortSpawn(list, ch, t);
        <- ch

    @benchmark
    def sortP2T8(): List[Int] =
        let n = 2 ** 2;
        let list = genUnsortedList(n);
        let ch = channel List[Int] 1;
        let t = 8;
        let _ = mergeSortSpawn(list, ch, t);
        <- ch

    @benchmark
    def sortP3T8(): List[Int] =
        let n = 2 ** 3;
        let list = genUnsortedList(n);
        let ch = channel List[Int] 1;
        let t = 8;
        let _ = mergeSortSpawn(list, ch, t);
        <- ch

    @benchmark
    def sortP4T8(): List[Int] =
        let n = 2 ** 4;
        let list = genUnsortedList(n);
        let ch = channel List[Int] 1;
        let t = 8;
        let _ = mergeSortSpawn(list, ch, t);
        <- ch

    @benchmark
    def sortP5T8(): List[Int] =
        let n = 2 ** 5;
        let list = genUnsortedList(n);
        let ch = channel List[Int] 1;
        let t = 8;
        let _ = mergeSortSpawn(list, ch, t);
        <- ch

    @benchmark
    def sortP6T8(): List[Int] =
        let n = 2 ** 6;
        let list = genUnsortedList(n);
        let ch = channel List[Int] 1;
        let t = 8;
        let _ = mergeSortSpawn(list, ch, t);
        <- ch

    @benchmark
    def sortP7T8(): List[Int] =
        let n = 2 ** 7;
        let list = genUnsortedList(n);
        let ch = channel List[Int] 1;
        let t = 8;
        let _ = mergeSortSpawn(list, ch, t);
        <- ch

    @benchmark
    def sortP8T8(): List[Int] =
        let n = 2 ** 8;
        let list = genUnsortedList(n);
        let ch = channel List[Int] 1;
        let t = 8;
        let _ = mergeSortSpawn(list, ch, t);
        <- ch

    @benchmark
    def sortP9T8(): List[Int] =
        let n = 2 ** 9;
        let list = genUnsortedList(n);
        let ch = channel List[Int] 1;
        let t = 8;
        let _ = mergeSortSpawn(list, ch, t);
        <- ch

    @benchmark
    def sortP10T8(): List[Int] =
        let n = 2 ** 10;
        let list = genUnsortedList(n);
        let ch = channel List[Int] 1;
        let t = 8;
        let _ = mergeSortSpawn(list, ch, t);
        <- ch

    @benchmark
    def sortP11T8(): List[Int] =
        let n = 2 ** 11;
        let list = genUnsortedList(n);
        let ch = channel List[Int] 1;
        let t = 8;
        let _ = mergeSortSpawn(list, ch, t);
        <- ch

    @benchmark
    def sortP12T8(): List[Int] =
        let n = 2 ** 12;
        let list = genUnsortedList(n);
        let ch = channel List[Int] 1;
        let t = 8;
        let _ = mergeSortSpawn(list, ch, t);
        <- ch

    @benchmark
    def sortP13T8(): List[Int] =
        let n = 2 ** 13;
        let list = genUnsortedList(n);
        let ch = channel List[Int] 1;
        let t = 8;
        let _ = mergeSortSpawn(list, ch, t);
        <- ch


    // 16 Threads

    @benchmark
    def sortP0T16(): List[Int] =
        let n = 2 ** 0;
        let list = genUnsortedList(n);
        let ch = channel List[Int] 1;
        let t = 16;
        let _ = mergeSortSpawn(list, ch, t);
        <- ch

    @benchmark
    def sortP1T16(): List[Int] =
        let n = 2 ** 1;
        let list = genUnsortedList(n);
        let ch = channel List[Int] 1;
        let t = 16;
        let _ = mergeSortSpawn(list, ch, t);
        <- ch

    @benchmark
    def sortP2T16(): List[Int] =
        let n = 2 ** 2;
        let list = genUnsortedList(n);
        let ch = channel List[Int] 1;
        let t = 16;
        let _ = mergeSortSpawn(list, ch, t);
        <- ch

    @benchmark
    def sortP3T16(): List[Int] =
        let n = 2 ** 3;
        let list = genUnsortedList(n);
        let ch = channel List[Int] 1;
        let t = 16;
        let _ = mergeSortSpawn(list, ch, t);
        <- ch

    @benchmark
    def sortP4T16(): List[Int] =
        let n = 2 ** 4;
        let list = genUnsortedList(n);
        let ch = channel List[Int] 1;
        let t = 16;
        let _ = mergeSortSpawn(list, ch, t);
        <- ch

    @benchmark
    def sortP5T16(): List[Int] =
        let n = 2 ** 5;
        let list = genUnsortedList(n);
        let ch = channel List[Int] 1;
        let t = 16;
        let _ = mergeSortSpawn(list, ch, t);
        <- ch

    @benchmark
    def sortP6T16(): List[Int] =
        let n = 2 ** 6;
        let list = genUnsortedList(n);
        let ch = channel List[Int] 1;
        let t = 16;
        let _ = mergeSortSpawn(list, ch, t);
        <- ch

    @benchmark
    def sortP7T16(): List[Int] =
        let n = 2 ** 7;
        let list = genUnsortedList(n);
        let ch = channel List[Int] 1;
        let t = 16;
        let _ = mergeSortSpawn(list, ch, t);
        <- ch

    @benchmark
    def sortP8T16(): List[Int] =
        let n = 2 ** 8;
        let list = genUnsortedList(n);
        let ch = channel List[Int] 1;
        let t = 16;
        let _ = mergeSortSpawn(list, ch, t);
        <- ch

    @benchmark
    def sortP9T16(): List[Int] =
        let n = 2 ** 9;
        let list = genUnsortedList(n);
        let ch = channel List[Int] 1;
        let t = 16;
        let _ = mergeSortSpawn(list, ch, t);
        <- ch

    @benchmark
    def sortP10T16(): List[Int] =
        let n = 2 ** 10;
        let list = genUnsortedList(n);
        let ch = channel List[Int] 1;
        let t = 16;
        let _ = mergeSortSpawn(list, ch, t);
        <- ch

    @benchmark
    def sortP11T16(): List[Int] =
        let n = 2 ** 11;
        let list = genUnsortedList(n);
        let ch = channel List[Int] 1;
        let t = 16;
        let _ = mergeSortSpawn(list, ch, t);
        <- ch

    @benchmark
    def sortP12T16(): List[Int] =
        let n = 2 ** 12;
        let list = genUnsortedList(n);
        let ch = channel List[Int] 1;
        let t = 16;
        let _ = mergeSortSpawn(list, ch, t);
        <- ch

    @benchmark
    def sortP13T16(): List[Int] =
        let n = 2 ** 13;
        let list = genUnsortedList(n);
        let ch = channel List[Int] 1;
        let t = 16;
        let _ = mergeSortSpawn(list, ch, t);
        <- ch


/*
    // 32 Threads

    @benchmark
    def sortP0T32(): List[Int] =
        let n = 2 ** 0;
        let list = genUnsortedList(n);
        let ch = channel List[Int] 1;
        let t = 32;
        let _ = mergeSortSpawn(list, ch, t);
        <- ch

    @benchmark
    def sortP1T32(): List[Int] =
        let n = 2 ** 1;
        let list = genUnsortedList(n);
        let ch = channel List[Int] 1;
        let t = 32;
        let _ = mergeSortSpawn(list, ch, t);
        <- ch

    @benchmark
    def sortP2T32(): List[Int] =
        let n = 2 ** 2;
        let list = genUnsortedList(n);
        let ch = channel List[Int] 1;
        let t = 32;
        let _ = mergeSortSpawn(list, ch, t);
        <- ch

    @benchmark
    def sortP3T32(): List[Int] =
        let n = 2 ** 3;
        let list = genUnsortedList(n);
        let ch = channel List[Int] 1;
        let t = 32;
        let _ = mergeSortSpawn(list, ch, t);
        <- ch

    @benchmark
    def sortP4T32(): List[Int] =
        let n = 2 ** 4;
        let list = genUnsortedList(n);
        let ch = channel List[Int] 1;
        let t = 32;
        let _ = mergeSortSpawn(list, ch, t);
        <- ch

    @benchmark
    def sortP5T32(): List[Int] =
        let n = 2 ** 5;
        let list = genUnsortedList(n);
        let ch = channel List[Int] 1;
        let t = 32;
        let _ = mergeSortSpawn(list, ch, t);
        <- ch

    @benchmark
    def sortP6T32(): List[Int] =
        let n = 2 ** 6;
        let list = genUnsortedList(n);
        let ch = channel List[Int] 1;
        let t = 32;
        let _ = mergeSortSpawn(list, ch, t);
        <- ch

    @benchmark
    def sortP7T32(): List[Int] =
        let n = 2 ** 7;
        let list = genUnsortedList(n);
        let ch = channel List[Int] 1;
        let t = 32;
        let _ = mergeSortSpawn(list, ch, t);
        <- ch

    @benchmark
    def sortP8T32(): List[Int] =
        let n = 2 ** 8;
        let list = genUnsortedList(n);
        let ch = channel List[Int] 1;
        let t = 32;
        let _ = mergeSortSpawn(list, ch, t);
        <- ch

    @benchmark
    def sortP9T32(): List[Int] =
        let n = 2 ** 9;
        let list = genUnsortedList(n);
        let ch = channel List[Int] 1;
        let t = 32;
        let _ = mergeSortSpawn(list, ch, t);
        <- ch

    @benchmark
    def sortP10T32(): List[Int] =
        let n = 2 ** 10;
        let list = genUnsortedList(n);
        let ch = channel List[Int] 1;
        let t = 32;
        let _ = mergeSortSpawn(list, ch, t);
        <- ch



    // 64 Threads

    @benchmark
    def sortP0T64(): List[Int] =
        let n = 2 ** 0;
        let list = genUnsortedList(n);
        let ch = channel List[Int] 1;
        let t = 64;
        let _ = mergeSortSpawn(list, ch, t);
        <- ch

    @benchmark
    def sortP1T64(): List[Int] =
        let n = 2 ** 1;
        let list = genUnsortedList(n);
        let ch = channel List[Int] 1;
        let t = 64;
        let _ = mergeSortSpawn(list, ch, t);
        <- ch

    @benchmark
    def sortP2T64(): List[Int] =
        let n = 2 ** 2;
        let list = genUnsortedList(n);
        let ch = channel List[Int] 1;
        let t = 64;
        let _ = mergeSortSpawn(list, ch, t);
        <- ch

    @benchmark
    def sortP3T64(): List[Int] =
        let n = 2 ** 3;
        let list = genUnsortedList(n);
        let ch = channel List[Int] 1;
        let t = 64;
        let _ = mergeSortSpawn(list, ch, t);
        <- ch

    @benchmark
    def sortP4T64(): List[Int] =
        let n = 2 ** 4;
        let list = genUnsortedList(n);
        let ch = channel List[Int] 1;
        let t = 64;
        let _ = mergeSortSpawn(list, ch, t);
        <- ch

    @benchmark
    def sortP5T64(): List[Int] =
        let n = 2 ** 5;
        let list = genUnsortedList(n);
        let ch = channel List[Int] 1;
        let t = 64;
        let _ = mergeSortSpawn(list, ch, t);
        <- ch

    @benchmark
    def sortP6T64(): List[Int] =
        let n = 2 ** 6;
        let list = genUnsortedList(n);
        let ch = channel List[Int] 1;
        let t = 64;
        let _ = mergeSortSpawn(list, ch, t);
        <- ch

    @benchmark
    def sortP7T64(): List[Int] =
        let n = 2 ** 7;
        let list = genUnsortedList(n);
        let ch = channel List[Int] 1;
        let t = 64;
        let _ = mergeSortSpawn(list, ch, t);
        <- ch

    @benchmark
    def sortP8T64(): List[Int] =
        let n = 2 ** 8;
        let list = genUnsortedList(n);
        let ch = channel List[Int] 1;
        let t = 64;
        let _ = mergeSortSpawn(list, ch, t);
        <- ch

    @benchmark
    def sortP9T64(): List[Int] =
        let n = 2 ** 9;
        let list = genUnsortedList(n);
        let ch = channel List[Int] 1;
        let t = 64;
        let _ = mergeSortSpawn(list, ch, t);
        <- ch

    @benchmark
    def sortP10T64(): List[Int] =
        let n = 2 ** 10;
        let list = genUnsortedList(n);
        let ch = channel List[Int] 1;
        let t = 64;
        let _ = mergeSortSpawn(list, ch, t);
        <- ch
*/
}