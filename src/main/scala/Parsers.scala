package fpinscala.parsing

import java.util.regex._
import scala.util.matching.Regex
//import fpinscala.testing._
//import fpinscala.testing.Prop._
import language.higherKinds
import language.implicitConversions
case class Hole() {}
trait Parsers[Parser[+_]] { self => // so inner classes may call methods of trait
  type  Parser[+A] = Location => Result[A]
  val json =  """
{"foo" : [ 1, 3, 4, 5  ]}
"""
  "ab a"
//  implicit def tok(s: String) = P.string(s)
//  val aba =  string("a") ** string("b") ** string("a")
//  def jsonParser: Parser[Any] = {
//    lazy val word = regex("[^\\s]+".r)
//    lazy val none = "null".map(x => null)
//    lazy val digit = regex("[0-9]+(\\.[0-9]+)?".r).map(_.toInt)
//    lazy val bool  = "true" | "false"
//    lazy val array = "[" +> manyWithSep(",")(value) ++ "]"
//    lazy val value: Parser[Any] = obj | word | array | bool | none | digit
//    lazy val obj = "{" + manyWithSep(",")(word + ":" + value) + "}"
//
//  }
  def run[A](p: Parser[A])(input: String): Either[ParseError,A] =
    p(Location(input, 0)) match {
      case Success(a, _) => Right(a)
      case Failure(e, _) => Left(e)
    }

  def formatParseError(e: ParseError): String = {
    def formatSingle(loc: Location, locs: List[(Location, String)]): String =
    {
      val msgs = locs.map(_._2)
      val badChar = loc.input.charAt(loc.offset)
      val formattedMsgs = msgs.mkString("\n")
      val badLine = loc.input.split('\n')(loc.line)
      val pointy = (" "* (loc.col)) + "^"
"""
bad character 'Z' in line 1 column 2
abZbaba
  ^
"""
//      s"""Errors $formattedMsgs
//       found at character $badChar
//       at line ${loc.line}, column ${loc.col}
//       $badLine
//       $pointy"""
       "Errors "  + formattedMsgs + 
       "found at character " + badChar + 
       "at line "  + loc.line + ",column " + loc.col + "\n" + badLine + "\n" + pointy
    }
    e.stack.groupBy(_._1).map((formatSingle _).tupled).mkString("\n")
    }

  implicit def string(s: String): Parser[String] =
    //label()
    loc => { 
      val sub = loc.input.drop(loc.offset)
      val matching = (sub, s).zipped.map({case (x,y) => x == y})
      if (matching.length == s.length)
        Success(s, s.length)
      else
        Failure(ParseError(List((loc, "Expected " + s)), List()), (matching.length > 0 )) 
    }

  def manyWithSep[A](sep: String)(p: Parser[A]): Parser[List[A]] = {
    lazy val once = map(productWS(p, string(sep)))(_._1)
    lazy val continue: Parser[List[A]] = map(productWS(once, manyWithSep(sep)(p)))({case (x,y) => x :: y})
    or(map(p)(List(_)),  continue)
      }
  
  implicit def operators[A](p: Parser[A]) = ParserOps[A](p)
  implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]):
    ParserOps[String] = ParserOps(f(a))
  def char(c: Char): Parser[Char] = string(c.toString) map (_.charAt(0))

  def flatMap[A,B](a: Parser[A])(f: (A => Parser[B])): Parser[B] =
    loc => a(loc) match {
      case Success(a, charsConsumed) => {
        val res = f(a)(loc.advanceBy(charsConsumed))
                     .advanceSuccess(charsConsumed)
        if (charsConsumed != 0 ) res.commit else res
        }
      case err@Failure(_,_) => err
    }

  def or[A](p1: Parser[A], p2: => Parser[A]): Parser[A] =
    loc => {
      p1(loc) match {
        case Failure(e, false) => p2(loc).mapError(x => ParseError(x.stack ++ e.stack, x.otherFailures))
        case res => res
      }
    }

  def attempt[A](p: Parser[A]): Parser[A] = loc => p(loc).unCommit

  def map[A,B](p: Parser[A])(f: (A => B)): Parser[B] = p flatMap(x => succeed(f(x)))

  def many[A](p: Parser[A]): Parser[List[A]] =
      map2(p, many(p) | succeed(List[A]()))(_ :: _)
    //map2(p, many(p))(_ :: _) or succeed(List())

  def productWS[A,B](a: Parser[A], b: Parser[B]): Parser[(A, B)] = 
    (a ** regex("\\s*".r)).map(_._1) ** b

  def many1[A](p: Parser[A]): Parser[List[A]] =
    (p ** many(p)) map {case (x, xs) => x :: xs}

  val numA: Parser[Int] = char('a').many.map(_.size)

  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]] =
    if (n == 0) succeed(List[A]())
    else map2(p, listOfN(n-1, p))(_::_)

  def parseInt: Parser[Int] = digits.map(_.toInt)
  val bencode = "3fff"
  val bencoded = "5fffaa"
  val foo = parseInt.flatMap(n => listOfN(n, string("f")))

  def map2[A,B,C](a: Parser[A], b: => Parser[B])(f: (A, B) => C): Parser[C] =
    flatMap(a)(a => b.map( f(a, _) ))

  def product[A,B](p: Parser[A], p2: Parser[B]): Parser[(A,B)] = map2(p, p2)((_,_))
// probably use implicits for optional whitespace
  def skipLWS(p: Parser[A], p2: Parser[B]): Parser[B] = 
    productWS(p, p2) map (_._2)
  def skipRWS(p: Parser[A], p2: Parser[B]): Parser[B] = 
    productWS(p, p2) map (_._1)
  //map2(p, p2)((x, y) => y)

  def parseBEncode(c: Char): Parser[List[Char]] =
    flatMap(digits)(x => listOfN(x.toInt, char(c)))

  def regex(r: Regex): Parser[String] = loc =>
  r.findPrefixOf(loc.input.toCharArray)  match {
    case Some(s) => Success(s, s.length)
    case None => Failure(ParseError(List((loc, "Expected " + r)), List()), false)
      }

  val digits = regex("[0-9]+?(\\.[0-9]+)".r)
  val ws = regex("\\s+".r)
  val aba =  string("a") ** string("b") ** string("a")
  /*
   * A default `succeed` implementation in terms of `string` and `map`.
   * We leave `succeed` abstract, since `map` is defined below in terms of
   * `flatMap` and `succeed`, which would be a circular definition! But we include
   * the definition here in case implementations wish to use it
   * (say if they provide a custom implementation of `map`, breaking the cycle)
   */
  def defaultSucceed[A](a: A): Parser[A] =
    string("") map (_ => a)

  def succeed[A](a: A): Parser[A] = loc => Success(a, 0)

  def slice[A](p: Parser[A]): Parser[String] =
    loc => p(loc) match { 
      case Success(_, charsConsumed) => Success(loc.input.slice(loc.offset, loc.offset + charsConsumed), charsConsumed)
      case f@Failure(_,_) => f
    }

  object Laws {
  }

  case class ParserOps[A](p: Parser[A]) {
    def |[B>:A](p2: Parser[B]): Parser[B] = self.or(p, p2)
    def or[B>:A](p2: => Parser[B]): Parser[B] = self.or(p, p2)
    def map[B](f: A => B): Parser[B] = self.map(p)(f)
    def many: Parser[List[A]] = self.many(p)
    def **[B](p2: Parser[B]): Parser[(A,B)] = self.product(p, p2)
    def flatMap[B](f: (A => Parser[B])) = self.flatMap(p)(f)
    def +>[B](p2: Parser[B]): Parser[(A,B)] = self.productWS(p, p2) 
    def *>[B](p2: Parser[B]): Parser[(A,B)] = self.skipWS(p, p2) 
    def map2[B,C](p2: Parser[B])(f: (A,B) => C): Parser[C] = self.map2(p, p2)(f)
    //def product[B](p2: Parser[B]): Parser[(A,B)] = self.product(p2)
    }
}
case class Location(input: String, offset: Int = 0) {

  lazy val line = input.slice(0,offset+1).count(_ == '\n') + 1
  lazy val col = input.slice(0,offset+1).reverse.indexOf('\n')

  def toError(msg: String): ParseError =
    ParseError(List((this, msg)))

  def advanceBy(n: Int) = copy(offset = offset+n)

  /* Returns the line corresponding to this location */
  def currentLine: String = 
    if (input.length > 1) input.lines.drop(line-1).next
    else ""
}

trait Result[+A] {
  def mapError(f: ParseError => ParseError): Result[A] = this match {
    case Failure(e,x) => Failure(f(e), x)
    case Success(_,_) => this 
  }
  def commit = this withCommit true
  def unCommit = this withCommit false
  def withCommit(b: Boolean) = this match { 
      case Failure(e,c) => Failure(e, b)
      case Success(_,_) => this
  }
  def advanceSuccess(n: Int) = this match {
    case Success(a, x) => Success(a, n + x)
    case Failure(_,_) => this
  }
  }
case class Success[+A](get: A, charsConsumed: Int) extends Result[A]
case class Failure(get: ParseError, isCommited: Boolean) extends Result[Nothing] 

case class ParseError(stack: List[(Location,String)] = List(),
                      otherFailures: List[ParseError] = List()) { 
  def push(x: (Location,String)): ParseError = copy(stack = x :: stack)

  def main(args: Array[String]) = {
    //println(jsonParser(Reference))
  }

}

//object JSON {
//  def jsonParser[Parser[+_]](P: Parsers[Parser]): Parser[String] = {
//  import P.{string => _, _}
//  val afoo = string("a") +> "foo" +> "bar"
//  val res = run(afoo)("afoobar")
//
//}



//object Parsers {
//
//  type  Parser[+A] = Location => Result[A]
//  object Reference extends Parsers[Parser] { }
//  def jsonParser[Parser[+_]](P: Parsers[Parser]): String = {
//    // we'll hide the string implicit conversion and promote strings to tokens instead
//    // this is a bit nicer than having to write token everywhere
//    import P.{string => _, _}
//    implicit def tok(s: String) = (P.string(s))
//
//    val afoo = "a" +> "foo" +> "bar"
//    val res = run(afoo)("afoobar")
//    println(res)
//    res.toString
//  }
//
//  def main(args: Array[String]) = { 
//    println("Hello there")
//    //println(jsonParser(Reference))
//  }
//}
//
