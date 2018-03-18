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
    // do neg
    "DoNeg" should "perform DoNeg" in {
      assertResult(N(-9.0)){
        step(Unary(Neg, N(9.0)))
      }
    }
    "DoNot" should "perform DoNot" in {
      assert(step(Unary(Not, B(true))) === B(false))
    }
    "DoSeq" should "perform DoSeq" in {
      assert(step(Binary(Seq, N(1.0), Binary(Div, N(1.0), N(0.0)))) === Binary(Div, N(1.0), N(0.0)))
    }
    "DoArith" should "perform DoArith" in {
      assertResult(N(10)){
        step(Binary(Div, N(100), N(10)))
      }
      assertResult(N(110)){
        step(Binary(Plus, N(100), N(10)))
      }
      assertResult(N(90)){
        step(Binary(Minus, N(100), N(10)))
      }
      assertResult(N(1000)){
        step(Binary(Times, N(100), N(10)))
      }
    }
    "DoPlusString" should "perform DoPlusString" in {
      assertResult(S("hibye")) {
        step(Binary(Plus, S("hi"), S("bye")))
      }
      intercept[StuckError] {
        step(Binary(Plus, N(10), S("bye")))
      }
    }
    "DoInequalityNumber and String" should "perform DoInequalityNumber" in {
      assertResult(B(true)) {
        step(Binary(Lt, N(12), N(14)))
      }
      assertResult(B(true)) {
        step(Binary(Le, N(12), N(14)))
      }
      assertResult(B(false)) {
        step(Binary(Gt, N(12), N(14)))
      }
      assertResult(B(false)) {
        step(Binary(Ge, N(12), N(14)))
      }
      assertResult(B(true)) {
        step(Binary(Lt, S("a"), S("b")))
      }
      assertResult(B(true)) {
        step(Binary(Le, S("a"), S("a")))
      }
      assertResult(B(true)) {
        step(Binary(Gt, S("abc"), S("aaa")))
      }
      assertResult(B(false)) {
        step(Binary(Ge, S("a"), S("b")))
      }
    }
    "do equality" should "perform doEquality" in {
      assertResult(B(true)){
        step(Binary(Eq, Function(None, List(("x", MTyp(MName, TNumber))), None, Binary(Plus, N(1.0), Var("x"))), Function(None, List(("x", MTyp(MName, TNumber))), None, Binary(Plus, N(1.0), Var("x")))))
      }
      assertResult(B(false)){
        step(Binary(Eq, Function(None, List(("y", MTyp(MName, TNumber))), None, Binary(Plus, N(1.0), Var("x"))), Function(None, List(("x", MTyp(MName, TNumber))), None, Binary(Plus, N(1.0), Var("x")))))
      }
      assertResult(B(true))
      {
        step(step(Binary(Eq, N(1.0), Binary(Plus, N(1.0), N(0.0)))))
      }
    }
    "DoAndTrue, DoAndFalse, DoOrTrue, DoOrFalse " should "perform correct steps" in {
      assertResult(Binary(And, B(true), B(false))){
        step(Binary(And, B(true), Binary(And, B(true), B(false))))
      }
      assertResult(B(false)){
        step(Binary(And, B(false), Binary(And, B(true), B(false))))
      }
      assertResult(B(true)){
        step(Binary(Or, B(true), Binary(And, B(true), B(false))))
      }
      assertResult(Binary(And, B(true), B(false))){
        step(Binary(Or, B(false), Binary(And, B(true), B(false))))
      }
    }
    "doPrint" should "perform DoPrint" in {
      assertResult(Undefined){
        step(Print(N(1000)))
      }
    }
    "doIfTrue and doIfFalse" should "perform shortcicruits" in {
      assertResult(Function(None, List(("x", MTyp(MName, TNumber))), None, Binary(Plus, N(1.0), Var("x")))) {
        step(If(B(true), Function(None, List(("x", MTyp(MName, TNumber))), None, Binary(Plus, N(1.0), Var("x"))), S("fail")))
      }
      assertResult(S("fail")) {
        step(If(B(false), Function(None, List(("x", MTyp(MName, TNumber))), None, Binary(Plus, N(1.0), Var("x"))), S("fail")))
      }
    }
    "DoDecl" should "perform DoDecl" in {
      assertResult(Binary(Plus, Binary(Div, N(1.0), N(123)), S("g"))) {
        step(Decl(MName, "g", Binary(Div, N(1.0), N(123)), Binary(Plus, Var("g"), S("g"))))
      }
      assert(step(Decl(MConst, "x", N(1.50), Binary(Times, Var("x"), Undefined))) === Binary(Times, N(1.50), Undefined))
    }
    "DoCall" should "perform DoCall" in {
      val foo = Function(None, List(("x",MTyp(MConst, TBool))), None, Binary(Eq, Var("x"), B(true)))
      assertResult(Binary(Eq, B(false), B(true))){
        step(Call(foo, List(B(false))))
      }
      assertResult(Binary(Eq,B(true),Binary(Plus, N(1.0), Binary(Minus, N(1.0), N(1.0))))){
        step(Call(Function(None, List(("x",MTyp(MConst, TBool)),("y", MTyp(MName, TNumber))), None, Binary(Eq, Var("x"), Var("y"))), List(B(true), Binary(Plus, N(1.0), Binary(Minus, N(1.0), N(1.0))))))
      }
    }
    "DoCallRec" should "perform DoCallRec" in {
      val foo = Function(Some("f"),List(("n",MTyp(MConst,TNumber))),Some(TNumber),If(Binary(Eq,Var("n"),N(0.0)),N(0.0),Call(Var("f"),List(Binary(Minus,N(1.0),N(1.0))))))
      assertResult(If(Binary(Eq, N(1.0), N(0.0)), N(0.0), Call(Function(Some("f"),List(("n",MTyp(MConst,TNumber))),Some(TNumber),If(Binary(Eq,Var("n"),N(0.0)),N(0.0),Call(Var("f"),List(Binary(Minus,N(1.0),N(1.0)))))), List(Binary(Minus, N(1.0), N(1.0)))))) {
        step(Call(foo, List(N(1.0))))
      }
    }
    "DoGetField" should "perform DoGetField" in {
      val obj = Obj(Map("a" -> N(1), "b"-> N(8), "de" -> Function(None, List(), None, N(1.0))))
      assertResult(Function(None,List(),None,N(1.0))){
        step(GetField(obj, "de"))
      }
    }
    // search rules
    "SearchUnaryRule" should "perform SearchUnary" in {
      assert(step(Unary(Neg, Binary(Plus, N(1.0), N(2.0)))) === Unary(Neg, N(3.0)))
    }
    "SearchBinary" should "perform SearchBinary" in {
      assert(step(Binary(Plus, Binary(Plus, N(1.0), N(2.0)), N(3.0))) === Binary(Plus, N(3.0), N(3.0)))
    }
    "SearchBinary2" should "perform SearchBinary2" in {
      assertResult(Binary(Plus, N(1.0), N(2.0))){
        step(Binary(Plus, N(1.0), Binary(Plus, N(2.0), N(0.0))))
      }
    }
    "SearchPrint" should "perform SearchPrint" in {
      assert(step(Print(Binary(Minus, N(10), N(7.5)))) === Print(N(2.5))) // eval inner expression first
    }
    "SearchIf" should "perform SearchIf" in {
      assertResult(If(S("hi"), Print(N(1)), N(1.0))) {
        step(If(Binary(Plus, S("h"), S("i")), Print(N(1)), N(1.0))) // should step e1 first
      }
    }
    "SearchDecl" should "perform SearchDecl" in {
      assertResult(Decl(MConst, "x", B(true), Binary(And, Var("x"), Binary(And, B(true), B(true))))){
        step(Decl(MConst, "x", Binary(And, B(true), B(true)), Binary(And, Var("x"), Binary(And, B(true), B(true)))))
      }
      assertResult(Binary(And, Binary(And, B(true), B(true)), Binary(And, B(true), B(true)))){
        step(Decl(MName, "x", Binary(And, B(true), B(true)), Binary(And, Var("x"), Binary(And, B(true), B(true)))))
      }
    }
    "SearchCall1" should "perform SearchCall1" in {
      val fooTrue = Function(None, List(("x", MTyp(MConst, TNumber))), Some(TNumber), Var("x")) // identity function
      val fooFalse = Function(None, List(("x", MTyp(MConst, TNumber))), Some(TNumber), Unary(Neg,Var("x"))) // additive inverse function
      assertResult(Call(fooTrue, List(N(13)))) {
        step(Call(If(B(true), fooTrue,fooFalse ), List(N(13))))
      }
    }
    "SearchCall2" should "perform SearchCall2" in {
      val fooAdd = Function(None, List(("x", MTyp(MConst, TNumber)), ("y", MTyp(MConst, TNumber)), ("z", MTyp(MName, TNumber))), None, Binary(Plus, Binary(Plus, Var("x"), Var("y")), Var("z")))
      assertResult(Call(fooAdd, List(N(16), Binary(Minus, N(80), N(1)), If(B(true), N(100), N(1000))))){
        step(Call(fooAdd, List(Binary(Times, N(8), N(2)), Binary(Minus, N(80), N(1)), If(B(true), N(100), N(1000)))))
      }
      assertResult(Call(fooAdd, List(N(16), N(79), If(B(true), N(100), N(1000))))){
       step(step(Call(fooAdd, List(Binary(Times, N(8), N(2)), Binary(Minus, N(80), N(1)), If(B(true), N(100), N(1000))))))
      }
      assertResult(Binary(Plus, Binary(Plus, N(16), N(79)),If(B(true), N(100), N(1000)))) {
        step(step(step(Call(fooAdd, List(Binary(Times, N(8), N(2)), Binary(Minus, N(80), N(1)), If(B(true), N(100), N(1000)))))))
      }
    }
    "SearchObject" should "perform SearchObject" in {
      val myObjMap = Map("a" -> Binary(Times, N(8), N(2)), "b" -> Binary(Minus, N(80), N(1)), "c" -> If(B(true), N(100), N(1000)))
      assertResult(Obj(myObjMap + ("a" -> N(16)))){
        step(Obj(myObjMap))
      }
      assertResult(Obj(myObjMap + ("a" -> N(16)) + ("b" -> N(79)))){
        step(step(Obj(myObjMap)))
      }
      assertResult(Obj(myObjMap + ("a" -> N(16)) + ("b" -> N(79)) + ("c" -> N(100)))){
        step(step(step(Obj(myObjMap))))
      }
    }
    "SearchGetField" should "perform SearchGetField" in {
      val myObjMap = Map("a" -> Binary(Times, N(8), N(2)), "b" -> Binary(Minus, N(80), N(1)), "c" -> If(B(true), N(100), N(1000)))
      assertResult(GetField(Obj(myObjMap + ("a" -> N(16))), "a")){
        step(GetField(Obj(myObjMap), "a"))
      }
      assertResult(GetField(Obj(myObjMap + ("a" -> N(16)) + ("b" -> N(79)) + ("c" -> N(100))), "a")){
        step(step(step(GetField(Obj(myObjMap), "a"))))
      }
    }

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