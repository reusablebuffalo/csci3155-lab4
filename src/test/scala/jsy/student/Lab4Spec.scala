package jsy.student

import jsy.lab4.Lab4Like
import jsy.lab4.ast._
import jsy.tester.JavascriptyTester
import org.scalatest._

class Lab4Spec(lab4: Lab4Like) extends FlatSpec {
  import lab4._

  /***** Higher-Function Exercises Tests *****/

  "compressRec/compressFold" should "compress List(1, 2, 2, 3, 3, 3)" in {
    val l1 = List(1, 2, 2, 3, 3, 3)
    val gold1 = List(1, 2, 3)
    assertResult(gold1) { compressRec(l1) }
    assertResult(gold1) { compressFold(l1) }
  } 
  
  "mapFirst" should "map the first element where f returns Some" in {
     val l1 = List(1, 2, -3, 4, -5)
     val gold1 = List(1, 2, 3, 4, -5)
     assertResult(gold1) {
       mapFirst(l1) { (i: Int) => if (i < 0) Some(-i) else None }
     }
  }
  
  "foldLeft" should "enable implementing treeFromList and sum" in {
    assertResult(6){
      sum(treeFromList(List(1, 2, 3)))
    }
    val testList = treeFromList(List(2,3,4))
    assertResult(16777216.0){
      foldLeft(testList)(2.0){(acc, elem) => math.pow(acc,elem.toDouble)}
    }
  }

  "strictlyOrdered" should "check strict ordering of a binary search tree" in {
    assert(!strictlyOrdered(treeFromList(List(1,1,2))))
    assert(strictlyOrdered(treeFromList(List(1,2))))
    assert(strictlyOrdered(treeFromList(List(2,1,3))))
  }


  /***** Interpreter Tests *****/

  {
    val xtype = TNumber
    val tenvx = extend(empty, "x", xtype)

    "TypeVar" should "perform TypeVar" in {
      assertResult(xtype) {
        typeof(tenvx, Var("x"))
      }
    }

    "TypeArith" should "perform TypeArith" in {
      assertResult(TNumber) {
        typeof(empty, Binary(Plus, N(1.0), N(1.0)))
      }
      assertResult(TNumber) {
        typeof(empty, Binary(Plus, Unary(Neg, N(1)), Binary(Div, N(2.0), N(3.0))))
      }
    }
    // custom tests
    "TypeNeg" should "perform TypeNeg" in {
      assertResult(TNumber){
        typeof(empty, Unary(Neg,N(1.0)))
      }
      intercept[StaticTypeError]{
        typeof(empty,Unary(Neg, S("hi")))
      }
    }
    "TypeNot" should "perform TypeNot" in {
      assertResult(TBool){
        typeof(empty, Unary(Not, B(true)))
      }
      intercept[StaticTypeError]{
        typeof(empty,Unary(Not, N(1.0)))
      }
    }
    "TypeSeq" should "perform TypeSeq" in {
      assertResult(TBool) {
        typeof(empty, Binary(Seq, B(false), Unary(Not, B(true))))
      }
      intercept[StaticTypeError] {
        typeof(empty, Binary(Seq, Unary(Not, N(1.0)), Unary(Not, B(true))))
      }
    }
    "TypeArith and TypePlusString" should "perform TypeArith and TypePlusString" in {
      assertResult(TNumber){
        typeof(empty, Binary(Plus, N(15),N(21)))
      }
      assertResult(TNumber){
        typeof(empty, Binary(Minus, N(15),N(21)))
      }
      assertResult(TString){
        typeof(empty, Binary(Plus, S("hi"), S("asdf")))
      }
      intercept[StaticTypeError]{
        typeof(empty,Binary(Plus, S("hi"), N(1.0)))
      }
      intercept[StaticTypeError]{
        typeof(empty, Binary(Minus, S("hi"), S("bye")))
      }
    }
    "TypeInequality (String and Number)" should "TypeInequality (string and number)" in{
      assertResult(TBool){
        typeof(empty, Binary(Ge, N(15), Binary(Plus, N(1.0), N(9.0))))
      }
      assertResult(TBool){
        typeof(empty, Binary(Lt, S("asdf"), S("asdf")))
      }
      intercept[StaticTypeError]{
        typeof(empty,Binary(Gt, Binary(Lt, S("asdf"), S("asdf")), Binary(Plus, N(1.0), N(9.0))))
      }
      intercept[StaticTypeError]{
        typeof(empty, Binary(Le, Print(S("hi")),Print(S("bye"))))
      }
    }
    "TypeCall" should "perform TypeCall" in {
      assertResult(TNumber){
        typeof(empty, Call(Function(None, List(("x", MTyp(MConst, TNumber)), ("y", MTyp(MConst, TNumber))), None, Binary(Plus,Var("x"), Var("y"))), List(N(1.0), N(2.0))))
      }
      assertResult(TBool){
        typeof(empty, Call(Function(None, List(("x", MTyp(MConst, TString)), ("y", MTyp(MConst, TString))), None, Binary(Le,Var("x"), Var("y"))), List(S("hi"), S("lajsf"))))
      }
    }
    "TypeRecFunction" should "perform TypeRecFunction" in {
      assertResult(TFunction(List(("n",MTyp(MConst,TNumber))), TNumber)){
        typeof(empty, Function(Some("f"),List(("n",MTyp(MConst,TNumber))),Some(TNumber),If(Binary(Eq,Var("n"),N(0.0)),N(0.0),Call(Var("f"),List(Binary(Minus,N(1.0),N(1.0)))))))
      }
    }
    "TypeObject" should "perform TypeObject" in {
      assertResult(TObj(Map("a" -> TNumber, "b" -> TString, "c" -> TUndefined))){
        typeof(empty, Obj(Map("b" -> Binary(Plus, S("hi"), S("bye")), "a" -> Unary(Neg, N(7)), "c" -> Print(S("print this")))))
      }
      assertResult(TObj(Map("a" -> TNumber, "b" -> TBool, "c" -> TUndefined))){
        typeof(empty, Obj(Map("b" -> Binary(Le, S("hi"), S("bye")), "a" -> Unary(Neg, N(7)), "c" -> Print(S("print this")))))
      }
      intercept[StaticTypeError] {
        typeof(empty, Obj(Map("b" -> Binary(Minus, S("hi"), S("bye")), "a" -> Unary(Neg, N(7)), "c" -> Print(S("print this")))))
      }
    }
    "TypeGetField" should "perform GetField" in {
      assertResult(TString) {
        typeof(empty, GetField(Obj(Map("b" -> Binary(Plus, S("hi"), S("bye")), "a" -> Unary(Neg, N(7)), "c" -> Print(S("print this")))), "b"))
      }
      assertResult(TUndefined){
        typeof(empty, GetField(Obj(Map("b" -> Binary(Plus, S("hi"), S("bye")), "a" -> Unary(Neg, N(7)), "c" -> Print(S("print this")))), "c"))
      }
      intercept[StaticTypeError]{
        typeof(empty, GetField(Obj(Map("b" -> Binary(Minus, S("hi"), S("bye")), "a" -> Unary(Neg, N(7)), "c" -> Print(S("print this")))), "b"))
      }
    }
    /* Substitute Tests */
    "substitute" should "perform proper substitutes" in {
      assertResult(Decl(MConst, "a$", N(1.0), Binary(Plus, Var("a$"), Binary(Plus, Var("a"), N(2.0))))) { // example with renaming (const a = 1; a+b) [b\ a +2]
        substitute(Decl(MConst, "a", N(1.0), Binary(Plus, Var("a"), Var("b"))), Binary(Plus, Var("a"), N(2.0)), "b")
      }
    }

    /* Step Tests */
    

  }

}

// An adapter class to pass in your Lab4 object.
class Lab4SpecRunner extends Lab4Spec(jsy.student.Lab4)

// The next bit of code runs a test for each .jsy file in src/test/resources/lab4.
// The test expects a corresponding .ans file with the expected result.
class Lab4JsyTests extends JavascriptyTester(None, "lab4", jsy.student.Lab4)

class Lab4Suite extends Suites(
  new Lab4SpecRunner,
  new Lab4JsyTests
)