package jsy.student

import jsy.lab4.Lab4Like

object Lab4 extends jsy.util.JsyApplication with Lab4Like {
  import jsy.lab4.ast._
  import jsy.lab4.Parser
  
  /*
   * CSCI 3155: Lab 4
   * Ian Smith
   * 
   * Partner: Joshua Griffiths
   * Collaborators: <Any Collaborators>
   */

  /*
   * Fill in the appropriate portions above by replacing things delimited
   * by '<'... '>'.
   * 
   * Replace the '???' expression with your code in each function.
   *
   * Do not make other modifications to this template, such as
   * - adding "extends App" or "extends Application" to your Lab object,
   * - adding a "main" method, and
   * - leaving any failing asserts.
   *
   * Your lab will not be graded if it does not compile.
   *
   * This template compiles without error. Before you submit comment out any
   * code that does not compile or causes a failing assert. Simply put in a
   * '???' as needed to get something that compiles without error. The '???'
   * is a Scala expression that throws the exception scala.NotImplementedError.
   */
  
  /* Collections and Higher-Order Functions */
  
  /* Lists */
  
  def compressRec[A](l: List[A]): List[A] = l match {
    case Nil | _ :: Nil => l // there is nothing to compress
    case h1 :: (t1 @ (h2 :: _)) => if (h1 == h2) compressRec(t1) else h1 :: compressRec(t1) // if h1 == h2, get rid of h1 and compress the rest,
      // else keep h1 and compress the rest
  }
  
  def compressFold[A](l: List[A]): List[A] = l.foldRight(Nil: List[A]){
    (h, acc) => acc match {
      case Nil => h:: acc // if its Nil then our accumulator just becomes h::Nil
      case head :: _ => if (head == h) acc else h :: acc // if h == head, dont add and return current acc other prepend to acc
    }
  }
  
  def mapFirst[A](l: List[A])(f: A => Option[A]): List[A] = l match { // function currying (named after Haskell Curry)
    case Nil => Nil // if empty return empty
    case h :: t => f(h) match { // else see if h is first element mapped by f
      case None => h :: mapFirst(t)(f) // f does nothing, so call mapFirst on rest of the list
      case Some(thing) => thing :: t // f does something, so return mapped thing with rest of list unchanged
    }
  }
  
  /* Trees */

  def foldLeft[A](t: Tree)(z: A)(f: (A, Int) => A): A = {
    def loop(acc: A, t: Tree): A = t match {
      case Empty => acc // just return accumulated value (i.e. go left till Empty then return; then right till empty and return)
      case Node(l, d, r) => loop(f(loop(acc,l),d),r) // loop left then eval current then loop right
    }
    loop(z, t)
  }

  // An example use of foldLeft
  def sum(t: Tree): Int = foldLeft(t)(0){ (acc, d) => acc + d }

  // Create a tree from a list. An example use of the
  // List.foldLeft method.
  def treeFromList(l: List[Int]): Tree =
    l.foldLeft(Empty: Tree){ (acc, i) => acc insert i }

  def strictlyOrdered(t: Tree): Boolean = {
    val (b, _) = foldLeft(t)((true, None: Option[Int])){
      (prev, d) => prev match {
        case (curBool, None) => (curBool , Some(d)) // first node in tree (this is temp min)
        case (curBool, Some(n)) => (curBool && (n < d), Some(d)) // return order bool && prev < next
      }
    }
    b
  }

  /* Type Inference */

  // While this helper function is completely given, this function is
  // worth studying to see how library methods are used.
  def hasFunctionTyp(t: Typ): Boolean = t match {
    case TFunction(_, _) => true
    case TObj(fields) if (fields exists { case (_, t) => hasFunctionTyp(t) }) => true
    case _ => false
  }
  
  def typeof(env: TEnv, e: Expr): Typ = {
    def err[T](tgot: Typ, e1: Expr): T = throw StaticTypeError(tgot, e1, e)

    e match {
      case Print(e1) => typeof(env, e1); TUndefined
      case N(_) => TNumber
      case B(_) => TBool
      case Undefined => TUndefined
      case S(_) => TString
      case Var(x) => if(env contains x) lookup(env, x) else err(TUndefined,e)
      case Decl(mode, x, e1, e2) => typeof(extend(env, x, typeof(env, e1)), e2)
      case Unary(Neg, e1) => typeof(env, e1) match {
        case TNumber => TNumber
        case tgot => err(tgot, e1)
      }
      case Unary(Not, e1) => typeof(env, e1) match {
        case TBool => TBool
        case tgot => err(tgot, e1) // if we didn't get bool through error
      }
      case Binary(Plus, e1, e2) => (typeof(env,e1), typeof(env,e2)) match {
        case (TNumber,TNumber) => TNumber
        case (TString, TString) => TString
        case (TNumber|TString,tgot) => err(tgot, e2)
        case (tgot, TString|TNumber) => err(tgot,e1)
        case (tgot1, _) => err(tgot1, e1) // this is correct (trial and error)
      }
      case Binary(Minus|Times|Div, e1, e2) => (typeof(env ,e1), typeof(env,e2)) match {
        case (TNumber, TNumber) => TNumber
        case (tgot,TNumber) => err(tgot,e1)
        case (TNumber, tgot) => err(tgot, e2)
        case (tgot,_)=> err(tgot,e1)
      }
      case Binary(Eq|Ne, e1, e2) => (typeof(env,e1), typeof(env,e2)) match {
        case (t1, _) if hasFunctionTyp(t1) => err(t1,e1)
        case (_, t2) if hasFunctionTyp(t2) => err(t2, e2)
        case (t1, t2) => if (t1 == t2) TBool else err(t2, e2)
      }
      case Binary(Lt|Le|Gt|Ge, e1, e2) => (typeof(env,e1), typeof(env,e2)) match {
        case (TNumber,TNumber) => TBool
        case (TString, TString) => TBool
        case (TNumber|TString,tgot) => err(tgot, e2)
        case (tgot, TString|TNumber) => err(tgot,e1)
        case (tgot1, _) => err(tgot1, e1)
      }
      case Binary(And|Or, e1, e2) => (typeof(env,e1),typeof(env,e2)) match {
        case (TBool, TBool) => TBool
        case (TBool, tgot) => err(tgot, e2)
        case (tgot,_) => err(tgot, e1)
      }
      case Binary(Seq, e1, e2) => (typeof(env,e1), typeof(env,e2)) match {
        case (_ , t2) => t2
      }
      case If(e1, e2, e3) => (typeof(env,e1), typeof(env,e2), typeof(env,e3)) match {
        case (TBool, t1, t2)  => if(t1 == t2) t1 else err(t2,e2) // if it doesn't match the first
        case (tgot, _, _) => err(tgot, e1) // maybe not necessary
      }
      case Function(p, params, tann, e1) => {
        // Bind to env1 an environment that extends env with an appropriate binding if
        // the function is potentially recursive.
        val env1 = (p, tann) match {
          /***** Add cases here *****/
          case (Some(x), Some(t)) => extend(env, x, TFunction(params, t)) // if function has a name, it can be recursive
          case (None, _) => env // no extension
          case _ => err(TUndefined, e1)   // error
        }
        // Bind to env2 an environment that extends env1 with bindings for params.
        val env2 = params.foldLeft(env1:TEnv)( {
            case (currEnv,(s: String, MTyp(_, t))) => extend(currEnv, s, t)
          }) // convert params (String,Mtyp) to map
        // Infer the type of the function body
        val t1 = typeof(env2, e1)
        // Check with the possibly annotated return type
        tann match {
          case None => TFunction(params, t1)
          case Some(t) => if(TFunction(params,t1) == TFunction(params,t)) TFunction(params, t1) else err(TFunction(params, t), e)
        }
      }
      case Call(e1, args) => typeof(env, e1) match {
        case TFunction(params, tret) if (params.length == args.length) => {
          (params zip args).foreach {
            case ((_, MTyp(_, t)), arg) => if (t != typeof(env, arg)) err(typeof(env, arg), arg) // check that they are equal types (otherwise there is an error)
        }
          tret
      }
        case tgot => err(tgot, e1)
      }
      case Obj(fields) => fields foreach {(ei) => typeof(env,ei._2)}; TObj(fields mapValues { (ei) => typeof(env, ei)}) // catch error, else map

      case GetField(e1, f) =>  typeof(env,e1) match { // get type of e1
        case TObj(tfields) => tfields.get(f) match {// e1 must be an obj
          case Some(value) => value // type of that field
          case None => err(TObj(tfields), e1) // error
        }
        case tgot => err(tgot, e1) // anything besides object type
      }

    }
  }
  
  
  /* Small-Step Interpreter */

  /*
   * Helper function that implements the semantics of inequality
   * operators Lt, Le, Gt, and Ge on values.
   *
   * We suggest a refactoring of code from Lab 2 to be able to
   * use this helper function in eval and step.
   *
   * This should the same code as from Lab 3.
   */
  def inequalityVal(bop: Bop, v1: Expr, v2: Expr): Boolean = {
    require(isValue(v1), s"inequalityVal: v1 ${v1} is not a value")
    require(isValue(v2), s"inequalityVal: v2 ${v2} is not a value")
    require(bop == Lt || bop == Le || bop == Gt || bop == Ge)
    // COPIED FROM LAB 3 (removed toNumber)
    (v1, v2) match {
      case (S(s1), S(s2)) => bop match { // must be string or number
        case Lt => s1<s2
        case Le => s1<=s2
        case Gt => s1>s2
        case Ge => s1>=s2
      }
      case (N(n1), N(n2)) => bop match {
        case Lt => n1 < n2
        case Le => n1 <= n2
        case Gt => n1 > n2
        case Ge => n1 >= n2
      }
    }
  }

  /* This should be the same code as from Lab 3 */
  // COPIED DIRECTLY FROM LAB 3
  def iterate(e0: Expr)(next: (Expr, Int) => Option[Expr]): Expr = { // iterate method will be called with step
    def loop(e: Expr, n: Int): Expr = next(e,n) match { // find next
      case None => e                    // if None, return e
      case Some(exp) => loop(exp, n+1) // else recurse
    }
    loop(e0, 0)
  }

  /* Capture-avoiding substitution in e replacing variables x with esub. */
  def substitute(e: Expr, esub: Expr, x: String): Expr = {
    def subst(e: Expr): Expr = e match {
      case N(_) | B(_) | Undefined | S(_) => e // base cases
      case Print(e1) => Print(substitute(e1, esub, x))
        /***** Cases from Lab 3 */
      case Unary(uop, e1) => Unary(uop, substitute(e1, esub, x)) // sub v for x in inner expressions
      case Binary(bop, e1, e2) => Binary(bop, substitute(e1, esub,x), substitute(e2, esub,x)) // sub inner expressions
      case If(e1, e2, e3) => If(substitute(e1, esub, x), substitute(e2, esub, x), substitute(e3, esub, x))
      case Var(y) => if (x == y) esub else Var(y)
      case Decl(mode, y, e1, e2) => {
        val new_e2 = if(x == y) e2 else substitute(e2, esub, x)
        Decl(mode, y, substitute(e1, esub, x), new_e2)
      }
        /***** Cases needing adapting from Lab 3 */
      case Function(p, params, tann, e1) => {
        if ((params exists ((pa)=> pa._1 == x )) || (p == Some(x))) e
        else Function(p, params, tann, substitute(e1, esub, x))
      }/*p match {
        case Some(pp) => if (pp == x || params.exists(pa => pa._1 == x)) e else Function(p, params, tann, substitute(e1, esub, x))
        case None => if (params.exists(pa => pa._1 == x)) e else Function(p, params, tann, substitute(e1, esub, x))
      }*/
      case Call(e1, args) => Call(substitute(e1, esub, x), args map {ei => substitute(ei,esub,x)}) // substitute in for all the args
        /***** New cases for Lab 4 */
      case Obj(fields) => Obj(fields mapValues { (exp) => substitute(exp, esub, x)}) // substitute all x in all value expr with esub
      case GetField(e1, f) => if (x==f) e else GetField(substitute(e1,esub,x), f)
    }
    val fvs = freeVars(esub)
    def fresh(x: String): String = if (fvs contains x) fresh(x + "$") else x // will use this later with rename
    subst(rename(e)(fresh))
  }

  /* Rename bound variables in e */
  def rename(e: Expr)(fresh: String => String): Expr = {
    def ren(env: Map[String,String], e: Expr): Expr = {
      e match {
        case N(_) | B(_) | Undefined | S(_) => e
        case Print(e1) => Print(ren(env, e1))

        case Unary(uop, e1) => Unary(uop, ren(env,e1))
        case Binary(bop, e1, e2) => Binary(bop, ren(env,e1), ren(env,e2))
        case If(e1, e2, e3) => If(ren(env,e1), ren(env,e2), ren(env,e3))

        case Var(y) =>
          if (env contains y) Var(lookup(env,y)) else Var(y) // either rename y or don't (if it is bound)
        case Decl(mode, y, e1, e2) =>
          val yp = fresh(y)
          Decl(mode, yp, ren(env,e1), ren(extend(env,y,yp),e2))

        case Function(p, params, retty, e1) => { // retty = return typ, e1 =body

          val (pp, envp): (Option[String], Map[String,String]) = p match {
            case None => (None, env) // no updated name, its anonymous; no update to environment
            case Some(x) => val xp = fresh(x); (Some(xp), env + (x -> xp))
          }
          val (paramsp, envpp) = params.foldRight( (Nil: List[(String,MTyp)], envp) ) { // freshify each param!
            case ((s, mt @ MTyp(_,_)), (prevList , envprev)) => val sp = fresh(s)
                  ((sp, mt) :: prevList, envprev + (s -> sp)) // don't necessarily need if
          }
          Function(pp, paramsp, retty, ren(envpp,e1))
        }

        case Call(e1, args) => Call(ren(env,e1), args map {case (ei) => ren(env,ei)} )

        case Obj(fields) => Obj(fields mapValues( (ei) => ren(env,ei)))
        case GetField(e1, f) => GetField(ren(env, e1), f)
      }
    }
    ren(empty, e)
  }

  /* Check whether or not an expression is reduced enough to be applied given a mode. */
  def isRedex(mode: Mode, e: Expr): Boolean = mode match {
    case MConst => if(!isValue(e)) true else false
    case MName => false
  }

  def step(e: Expr): Expr = {
    require(!isValue(e), s"step: e ${e} to step is a value")
    e match {
      /* Base Cases: Do Rules */
      case Print(v1) if isValue(v1) => println(pretty(v1)); Undefined
        /***** Cases needing adapting from Lab 3. */
      case Unary(Neg, v1) if isValue(v1) => v1 match { // do neg
        case N(n1) => N(-n1)
        case _ => throw StuckError(e)
      }
      case Unary(Not, v1) if isValue(v1) => v1 match {
        // do not
        case B(b1) => B(!b1)
        case _ => throw StuckError(e)
      }
      case Binary(Seq, v1, e2) if isValue(v1) => e2 // do seq
      case Binary(Plus, v1, v2) if isValue(v1) && isValue(v2) => (v1,v2) match { // do plus
        case (S(s1), S(s2)) => S(s1 + s2)
        case (N(n1), N(n2)) => N(n1+n2)
        case _ => throw StuckError(e)
      }
      case Binary(bop, v1, v2) if isValue(v1) && isValue(v2) => (v1,v2) match { // do arith
        case (N(n1), N(n2)) => bop match {
          case Minus => N(n1 - n2) // OMG I HAD N1+N2 instead of n1-n2
          case Div => N(n1/n2)
          case Times => N(n1*n2)
          case Lt | Le | Gt | Ge => B(inequalityVal(bop, v1, v2)) // do inequality (all cases handled by inequalityVal() )
          case Eq => B(v1 == v2)
          case Ne => B(v1 != v2)
        }
        case _ => throw StuckError(e)
      }
      case Binary(And, v1, e2) if isValue(v1) => v1 match {
        case B(b) => if(b) e2 else B(false)
        case _ => throw StuckError(e)
      } // match on And | Or
      case Binary(Or, v1, e2) if isValue(v1) => v1 match {
        case B(b) => if(b) B(true) else e2
        case _ => throw StuckError(e)
      }

      case If(v1, e2, e3) if isValue(v1) => v1 match {
        // DoIfTrue and DoIfFalse
        case B(b) => if(b) e2 else e3
        case _ => throw StuckError(e)
      }
        // Do Decl (x1 not redex)
      case Decl(mode, x, e1, e2) if !isRedex(mode, e1) => substitute(e2, e1, x) // just sub e1 for x in e2

        // doGetField
      case GetField(v1, f) if isValue(v1) => v1 match {
        case Obj(fields) => fields.get(f) match {
          case None => throw StuckError(e)
          case Some(v) => v
        }
        case _ => throw StuckError(e)
      }
        /***** More cases here */
      case Call(v1, args) if isValue(v1) =>
        v1 match {
          case Function(p, params, tann, e1) => {
            val pazip = params zip args
            if (pazip forall{  case ((_, MTyp(m,_)), arg) => !isRedex(m, arg) }) { // args should not be reducible
              val e1p = pazip.foldRight(e1) {
                case (((x, _), arg_value),acc) => substitute(acc, arg_value, x)
              }
              p match {
                case None => e1p // anonymous function; no more subs required
                case Some(x1) => substitute(e1p,v1, x1) // sub in function wherever its name appears // call by name
              }
            }
            else { // if args are reducible, reduce arg
              val pazipp = mapFirst(pazip) {
                case (param@(_: String, MTyp(m, _)), arg: Expr) if isRedex(m, arg) => Some((param, step(arg))) // map first reducible arg
                case _ => None
              }
              Call(v1, pazipp.unzip._2)
            }
          }
          case _ => throw StuckError(e)
        }
        /***** New cases for Lab 4. */

      /* Inductive Cases: Search Rules */
      case Print(e1) => Print(step(e1)) // searchprint
        /***** Cases from Lab 3. */
      case Unary(uop, e1) => Unary(uop, step(e1)) // searchUnary
      case Binary(bop, e1, e2) if !isValue(e1) => Binary(bop, step(e1) ,e2) // searchBinary1
      case Binary(bop, v1, e2) if isValue(v1) => Binary(bop, v1, step(e2)) // searchBinary2
      case If(e1, e2, e3) => If(step(e1) , e2, e3) // searchIf
      case Decl(mode, x, e1, e2) if isRedex(mode,e1)=> Decl(mode, x, step(e1), e2) // searchDecl
      /***** More cases here */
        // search object
      case Obj(fields) if !isValue(e) => fields find {(f) => !isValue(f._2)} match { // finds first key that doesn't map to value
        case None => throw StuckError(e) // we shouldn't reach this
        case Some((ff,e1)) => Obj(fields + (ff -> step(e1))) // update this key to map to stepped e
      }
          // search getfield
      case GetField(e1, f) => e1 match {
        case Obj(_) => GetField(step(e1), f) // step object
        case _=> throw StuckError(e)
      }
        /***** Cases needing adapting from Lab 3 */
      //case Call(v1 @ Function(_, _, _, _), args) => ???
      case Call(e1, args) => Call(step(e1), args)
        /***** New cases for Lab 4. */

      /* Everything else is a stuck error. Should not happen if e is well-typed.
       *
       * Tip: you might want to first develop by comment out the following line to see which
       * cases you have missing. You then uncomment this line when you are sure all the cases
       * that you have left the ones that should be stuck.
       */
      case _ => throw StuckError(e)
    }
  }
  
  
  /* External Interfaces */
  
  //this.debug = true // uncomment this if you want to print debugging information
  this.keepGoing = true // comment this out if you want to stop at first exception when processing a file
}

