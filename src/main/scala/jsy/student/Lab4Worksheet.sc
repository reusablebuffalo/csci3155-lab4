/*
 * CSCI 3155: Lab 4 Worksheet
 *
 * This worksheet demonstrates how you could experiment
 * interactively with your implementations in Lab4.scala.
 */

// Imports the parse function from jsy.lab1.Parser
import jsy.lab4.Parser.parse

// Imports the ast nodes
import jsy.lab4.ast._

// Imports all of the functions form jsy.student.Lab2 (your implementations in Lab2.scala)
import jsy.student.Lab4._

// Try compressRec
//val cr1 = compressRec(List(1, 2, 2, 3, 3, 3))

// Parse functions with possibly multiple parameters and type annotations.
parse("function fst(x: number, y: number): number { return x }")
parse("function (x: number) { return x }")
parse("function (f: (y: number) => number, x: number) { return f(x) }")

// Parse objects
parse("{ f: 0, g: true }")
parse("x.f")


// :: is right associative 3 :: ( 2 :: Nil)
val foo:List[Int] = 3 :: 2 :: Nil
// delete, add, search, size, append, get
// equal


// append
def append[T](x :T, xs: List[T]): List[T] = x :: xs

append(7, foo)
//append("foo" , foo)
append("foo", Nil)


// size
def size[T](xs : List[T]): Int = xs match {
  case Nil => 0
  case x :: tail => 1 + size(tail)
}

size(foo)
val foo2:List[Int] = 4::3::2::1::Nil
size(foo2)

// search through list, if found return index else return None
def search[T](t:T, xs: List[T]): Boolean = xs match {
  case Nil => false
  case x :: tail => if (x == t) true else search(t, tail)
}

search(3, foo)

def get[T](i:Int, xs:List[T]): Option[T] = {
  if (i < 0) None else
  xs match {
    case Nil => None
    case x :: tail => i match {
      case 0 => Some(x)
      case _ => get(i - 1, tail)
    }
  }
}

get(1, foo)

def equals[T](xs : List[T], ys : List[T]) : Boolean = (xs,ys) match {
  case (Nil, Nil) => true
  case (Nil, y:: ytail) => false
  case (x :: xtail, Nil) => false
  case (x:: xtail, y :: ytail) => if (x == y) equals(xtail,ytail) else false
}

equals(foo, foo)
equals(foo, foo2)

// we want some function that is going to iterate through the list and
// do something and either stop or iterate to next element in the list

//def map[A,B](f: A => B, xs :List[A]) : List[B] = xs match {
//  case Nil => Nil
//  case x :: tail => f(x) :: map(f(_), tail)
//}

// List(1,2,3) & List(4,5,6)
// we want the zip of these two lists to be List((1,4), (2,5), (3,6), Nil)

def zip[A,B](xs:List[A], ys:List[B]) : List[(A,B)] = (xs,ys) match {
  case (Nil, Nil) => Nil
  case (Nil, y:: ytail) => Nil
  case (x :: xtail, Nil) => Nil // truncates the zip
  case (x:: xtail, y :: ytail) => (x,y) :: zip (xtail, ytail)
}

// call by name vs call by value
// matters for

// def x(k) = {
//  k.myobj = 3
//
// }

// type checker first, add lab3 code, then object cases

// OBJECTS IN JAVASCRIPTY
// two different syntaxes that are equalivalent

// x.f or x["f"] are exactly the same
// similar to structs in c, or simplified version of python classes
//
compressRec(List(1,2,2,2,3,4,4))
val myT = treeFromList(List(2,1,3))
strictlyOrdered(myT)
parse("1+1")
typeof(empty, parse("const g = (x: const number)=>x+x"))



