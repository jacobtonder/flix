package ca.uwaterloo.flix.runtime

import ca.uwaterloo.flix.api.{Invokable, IValue, Flix}
import ca.uwaterloo.flix.language.ast.Symbol
import org.scalatest.FunSuite

class TestSolver extends FunSuite {

  object Parity {
    val Top = Value.mkTag(Symbol.Resolved.mk(List("Parity")), "Top", Value.Unit)
    val Odd = Value.mkTag(Symbol.Resolved.mk(List("Parity")), "Odd", Value.Unit)
    val Even = Value.mkTag(Symbol.Resolved.mk(List("Parity")), "Even", Value.Unit)
    val Bot = Value.mkTag(Symbol.Resolved.mk(List("Parity")), "Bot", Value.Unit)

    val Definition =
      """
        |    enum Parity {
        |                  case Top,
        |        case Odd,          case Even,
        |                  case Bot
        |    };
        |
        |    def leq(e1: Parity, e2: Parity): Bool = match (e1, e2) with {
        |        case (Parity.Bot, _)              => true;
        |        case (Parity.Odd, Parity.Odd)     => true;
        |        case (Parity.Even, Parity.Even)   => true;
        |        case (_, Parity.Top)              => true;
        |        case _                            => false;
        |    };
        |
        |    def lub(e1: Parity, e2: Parity): Parity = match (e1, e2) with {
        |        case (Parity.Bot, _)              => e2;
        |        case (_, Parity.Bot)              => e1;
        |        case (Parity.Odd, Parity.Odd)     => Parity.Odd;
        |        case (Parity.Even, Parity.Even)   => Parity.Even;
        |        case _                            => Parity.Top;
        |    };
        |
        |    def glb(e1: Parity, e2: Parity): Parity = match (e1, e2) with {
        |        case (Parity.Top, _) => e2;
        |        case (_, Parity.Top) => e1;
        |        case (Parity.Odd, Parity.Odd) => Parity.Odd;
        |        case (Parity.Even, Parity.Even) => Parity.Even;
        |        case _ => Parity.Bot;
        |    }
        |
        |    def plus(e1: Parity, e2: Parity): Parity = match (e1, e2) with {
        |        case (_, Parity.Bot)              => Parity.Bot;
        |        case (Parity.Bot, _)              => Parity.Bot;
        |        case (Parity.Odd, Parity.Odd)     => Parity.Even;
        |        case (Parity.Odd, Parity.Even)    => Parity.Odd;
        |        case (Parity.Even, Parity.Odd)    => Parity.Odd;
        |        case (Parity.Even, Parity.Even)   => Parity.Even;
        |        case _                            => Parity.Top;
        |    };
        |
        |    def isMaybeZero(e: Parity): Bool = match e with {
        |        case Parity.Bot    => false;
        |        case Parity.Odd    => false;
        |        case Parity.Even   => true;
        |        case Parity.Top    => true;
        |    };
        |
        |    let Parity<> = (Parity.Bot, Parity.Top, leq, lub, glb);
      """.stripMargin

  }

  test("Cross01") {
    val s =
      """rel A(x: Int, y: Int);
        |
        |A(1, 2).
        |A(2, 3).
        |
        |A(x, z) :- A(x, y), A(y, z).
      """.stripMargin

    val model = new Flix().addStr(s).solve().get
    val A = model.getRelation("A").toList
    assert(A contains List(Value.mkInt32(1), Value.mkInt32(3)))
  }

  test("Cross02") {
    val s =
      """rel A(x: Int, y: Int);
        |
        |A(1, 2).
        |A(2, 3).
        |A(3, 4).
        |A(4, 5).
        |
        |A(x, z) :- A(x, y), A(y, z).
      """.stripMargin

    val model = new Flix().addStr(s).solve().get
    val A = model.getRelation("A").toList
    assert(A contains List(Value.mkInt32(1), Value.mkInt32(5)))
    assert(A contains List(Value.mkInt32(1), Value.mkInt32(5)))
  }

  test("Cross03") {
    val s =
      """rel A(x: Int, y: Int);
        |
        |A(1, 2).
        |A(2, 3).
        |A(3, 1).
        |
        |A(x, z) :- A(x, y), A(y, z).
      """.stripMargin

    val model = new Flix().addStr(s).solve().get
    val A = model.getRelation("A").toList
    assert(A contains List(Value.mkInt32(2), Value.mkInt32(2)))
  }

  test("Cross04") {
    val s =
      """rel A(x: Int, y: Str, z: Int);
        |
        |A(1, "a", 2).
        |A(2, "a", 3).
        |A(1, "b", 2).
        |
        |A(x, c, z) :- A(x, c, y), A(y, c, z).
      """.stripMargin

    val model = new Flix().addStr(s).solve().get
    val A = model.getRelation("A").toList
    assert(A contains List(Value.mkInt32(1), Value.mkStr("a"), Value.mkInt32(3)))
  }

  test("Cross05") {
    val s =
      """rel A(x: Int, y: Str, z: Int);
        |
        |A(1, "a", 2).
        |A(2, "a", 3).
        |A(3, "b", 1).
        |
        |A(x, "b", y) :- A(x, "a", y).
        |A(x, c, z) :- A(x, c, y), A(y, c, z).
      """.stripMargin

    val model = new Flix().addStr(s).solve().get
    val A = model.getRelation("A").toList
    assert(A contains List(Value.mkInt32(2), Value.mkStr("b"), Value.mkInt32(2)))
  }

  test("Cross06") {
    val s =
      """rel A(x: Int, y: Str, z: Int);
        |
        |A(1, "a", 2).
        |A(2, "a", 3).
        |A(3, "b", 1).
        |
        |A(x, c, y) :- A(y, c, x).
      """.stripMargin

    val model = new Flix().addStr(s).solve().get
    val A = model.getRelation("A").toList
    assert(A contains List(Value.mkInt32(1), Value.mkStr("b"), Value.mkInt32(3)))
  }

  test("Cross07") {
    val s =
      """rel A(x: Int, y: Str, z: Int);
        |
        |A(1, "a", 2).
        |A(2, "a", 3).
        |A(3, "b", 1).
        |
        |A(x, c, y) :- A(y, c, x).
      """.stripMargin

    val model = new Flix().addStr(s).solve().get
    val A = model.getRelation("A").toList
    assert(A contains List(Value.mkInt32(1), Value.mkStr("b"), Value.mkInt32(3)))
  }

  test("Cross08") {
    val s =
      """rel A(x: Int);
        |rel B(x: Int);
        |rel R(x: Int);
        |
        |A(1). A(2).
        |B(2). B(3).
        |
        |R(x) :- A(x), B(x).
      """.stripMargin

    val model = new Flix().addStr(s).solve().get
    val R = model.getRelation("R").toList
    assert(R contains List(Value.mkInt32(2)))
  }

  test("Cross09") {
    val s =
      """rel A(x: Int);
        |rel B(x: Int);
        |rel C(x: Int);
        |
        |rel R(x: Int);
        |
        |A(1). A(2). A(3).
        |B(2). B(3).
        |C(3).
        |
        |R(x) :- A(x), B(x), C(x).
      """.stripMargin

    val model = new Flix().addStr(s).solve().get
    val R = model.getRelation("R").toList
    assert(R contains List(Value.mkInt32(3)))
  }

  test("Cross10") {
    val s =
      """rel A(x: Int);
        |rel B(x: Int);
        |rel C(x: Int);
        |
        |rel R(x: Int);
        |
        |A(1). A(2). A(3).
        |B(2). B(3).
        |C(3).
        |
        |R(x) :- C(x), A(x), B(x), B(x), C(x), A(x).
      """.stripMargin

    val model = new Flix().addStr(s).solve().get
    val R = model.getRelation("R").toList
    assert(R contains List(Value.mkInt32(3)))
  }

  test("Cross11") {
    val s =
      """rel A(x: Int, y: Int);
        |rel B(x: Int, y: Int);
        |
        |rel R(x: Int, y: Int);
        |
        |A(1, 2). A(3, 4).
        |B(2, 3). B(4, 5).
        |
        |R(x, z) :- A(x, y), B(y, z).
      """.stripMargin

    val model = new Flix().addStr(s).solve().get
    val R = model.getRelation("R").toList
    assert(R contains List(Value.mkInt32(3), Value.mkInt32(5)))
  }

  test("Cross12") {
    val s =
      """rel A(x: Int, y: Int);
        |rel B(x: Int, y: Int);
        |rel C(x: Int, y: Int);
        |
        |rel R(x: Int, y: Int);
        |
        |A(1, 2). A(3, 4).
        |B(2, 3). B(4, 5).
        |C(3, 7).
        |
        |R(x, w) :- A(x, y), B(y, z), C(z, w).
      """.stripMargin

    val model = new Flix().addStr(s).solve().get
    val R = model.getRelation("R").toList
    assert(R contains List(Value.mkInt32(1), Value.mkInt32(7)))
  }

  test("Cross13") {
    val s =
      """rel A(x: Int, y: Int);
        |rel B(x: Int, y: Int);
        |rel C(x: Int, y: Int);
        |
        |A(1, 2). A(7, 1).
        |B(2, 3). B(3, 5).
        |C(3, 4). C(5, 6).
        |
        |C(x, z) :- A(x, y), B(y, z).
        |A(x, z) :- C(x, y), B(y, z).
        |B(x, z) :- A(x, y), C(y, z).
      """.stripMargin

    val model = new Flix().addStr(s).solve().get
    val A = model.getRelation("A").toList
    val B = model.getRelation("B").toList
    val C = model.getRelation("C").toList
    assert(C contains List(Value.mkInt32(1), Value.mkInt32(3)))
    assert(A contains List(Value.mkInt32(1), Value.mkInt32(5)))
    assert(B contains List(Value.mkInt32(1), Value.mkInt32(6)))
    assert(C contains List(Value.mkInt32(7), Value.mkInt32(6)))
  }

  test("Cross14") {
    val s =
      """rel A(x: Int);
        |rel B(y: Int);
        |rel C(z: Int);
        |
        |rel R(x: Int);
        |
        |A(1). A(2).
        |// B empty
        |C(1). C(2).
        |
        |R(x) :- A(x), B(x), C(x).
      """.stripMargin

    val model = new Flix().addStr(s).solve().get
    val R = model.getRelation("R").toList
    assert(R.isEmpty)
  }

  test("Wildcard01") {
    val s =
      """rel A(x: Int, y: Int);
        |rel B(x: Int, y: Int);
        |rel C(x: Int, y: Int);
        |
        |A(1, 2).
        |A(3, 4).
        |
        |B(5, 6).
        |B(7, 8).
        |
        |C(x, y) :- A(x, _), B(_, y).
      """.stripMargin

    val model = new Flix().addStr(s).solve().get
    val C = model.getRelation("C").toList
    assert(C contains List(Value.mkInt32(1), Value.mkInt32(6)))
    assert(C contains List(Value.mkInt32(1), Value.mkInt32(8)))
    assert(C contains List(Value.mkInt32(3), Value.mkInt32(6)))
    assert(C contains List(Value.mkInt32(3), Value.mkInt32(8)))
  }

  test("Lattice01") {
    val s =
      """lat A(x: Int, v: Parity<>);
        |
        |A(1, Parity.Odd).
        |A(2, Parity.Even).
        |A(3, Parity.Top).
        |
      """.stripMargin

    val model = new Flix().addStr(Parity.Definition).addStr(s).solve().get
    val A = model.getLattice("A").toMap
    assert(A(List(Value.mkInt32(1))).head == Parity.Odd)
    assert(A(List(Value.mkInt32(2))).head == Parity.Even)
    assert(A(List(Value.mkInt32(3))).head == Parity.Top)
  }

  test("Lattice02") {
    val s =
      """lat A(x: Int, v: Parity<>);
        |
        |A(1, Parity.Odd).
        |A(1, Parity.Even).
        |
      """.stripMargin

    val model = new Flix().addStr(Parity.Definition).addStr(s).solve().get
    val A = model.getLattice("A").toMap
    assert(A(List(Value.mkInt32(1))).head == Parity.Top)
  }

  test("Lattice03") {
    val s =
      """lat A(x: Int, v: Parity<>);
        |
        |A(1, Parity.Odd).
        |A(2, Parity.Even).
        |
        |A(3, x) :- A(1,x).
        |A(3, x) :- A(2,x).
      """.stripMargin

    val model = new Flix().addStr(Parity.Definition).addStr(s).solve().get
    val A = model.getLattice("A").toMap
    assert(A(List(Value.mkInt32(3))).head == Parity.Top)
  }

  test("NotEqual01") {
    val s =
      """rel A(x: Int);
        |rel B(x: Int, y: Int);
        |
        |A(1). A(2). A(3). A(4).
        |
        |B(x, y) :- A(x), A(y), x != y.
      """.stripMargin

    val model = new Flix().addStr(s).solve().get
    val B = model.getRelation("B").toList
    assert(B contains List(Value.mkInt32(1), Value.mkInt32(2)))
    assert(B contains List(Value.mkInt32(1), Value.mkInt32(3)))
    assert(B contains List(Value.mkInt32(1), Value.mkInt32(4)))
    assert(B contains List(Value.mkInt32(4), Value.mkInt32(1)))
    assert(!(B contains List(Value.mkInt32(1), Value.mkInt32(1))))
    assert(!(B contains List(Value.mkInt32(2), Value.mkInt32(2))))
    assert(!(B contains List(Value.mkInt32(3), Value.mkInt32(3))))
    assert(!(B contains List(Value.mkInt32(4), Value.mkInt32(4))))
  }

  test("NotEqual02") {
    val s =
      """rel A(x: Int, y: Int);
        |rel B(x: Int, y: Int);
        |
        |A(1, 2).
        |A(2, 2).
        |
        |B(x, y) :- A(x, y), x != y.
      """.stripMargin

    val model = new Flix().addStr(s).solve().get
    val B = model.getRelation("B").toList
    assert(B contains List(Value.mkInt32(1), Value.mkInt32(2)))
    assert(!(B contains List(Value.mkInt32(2), Value.mkInt32(2))))
  }

  test("NotEqual03") {
    val s =
      """rel A(x: Int, y: Int);
        |rel B(x: Int, y: Int);
        |
        |A(1, 2).
        |A(2, 1).
        |A(2, 3).
        |
        |B(x, z) :- A(x, y), A(y, z), x != z.
      """.stripMargin

    val model = new Flix().addStr(s).solve().get
    val B = model.getRelation("B").toList
    assert(B contains List(Value.mkInt32(1), Value.mkInt32(3)))
    assert(!(B contains List(Value.mkInt32(1), Value.mkInt32(1))))
  }

  ignore("Loop01") {
    val s =
      """rel A(x: Int);
        |rel B(x: Int);
        |
        |def f(x: Int): Set[Int] = #{x * x};
        |
        |A(1).
        |A(2).
        |
        |B(y) :- A(x), y <- f(x): Set[Int].
      """.stripMargin

    val model = new Flix().addStr(s).solve().get
    val B = model.getRelation("B").toSet
    assert(B contains List(Value.mkInt32(1)))
    assert(B contains List(Value.mkInt32(4)))
  }

  test("FilterHook01") {
    val s =
      """rel A(x: Int);
        |rel B(x: Int);
        |
        |A(1).
        |A(2).
        |
        |B(x) :- f(x), A(x).
      """.stripMargin

    val flix = new Flix()
    val tpe = flix.mkFunctionType(Array(flix.mkInt32Type), flix.mkStrType)
    flix
      .addStr(s)
      .addHook("f", tpe, new Invokable {
        override def apply(args: Array[IValue]): IValue = flix.mkBool(args(0) == flix.mkInt32(1))
      })

    val model = flix.solve()
      .get

    val B = model.getRelation("B").toSet
    assert(B contains List(Value.mkInt32(1)))
    assert(!(B contains List(Value.mkInt32(2))))
  }

}
