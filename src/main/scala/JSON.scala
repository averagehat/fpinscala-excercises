package fpinscala.parsing

import language.higherKinds
import language.implicitConversions
object JSON {


    type  Parser[+A] = Location => Result[A]
    object Reference extends Parsers[Parser] { }
    def jsonParser[Parser[+_]](P: Parsers[Parser]): String = {
      // we'll hide the string implicit conversion and promote strings to tokens instead
      // this is a bit nicer than having to write token everywhere
      import P.{string => _, _}
      implicit def tok(s: String) = (P.string(s))

      val afoo = "a" +> "foo" +> "bar"
      val res = run(afoo)("afoobar")
      println(res)
      res.toString
    }

    def main(args: Array[String]) = {
      println("Hello there")
      println(jsonParser(Reference))
    }


//
//val json =  """
//{"foo" : [ 1, 3, 4, 5  ]}
//"""
//  "ab a"
//  val aba =  "a" +> "b" +> "a"
//  def jsonParser[Parser[+_]](P: Parsers[Parser]): Parser[Any] = {
//    import P.{string => _, _}
//    implicit def tok(s: String) = P.string(s) 
//    lazy val word = regex("[^\\s]+".r)
//    lazy val none = "null".map(x => null)
//    lazy val digit = regex("[0-9]+(\\.[0-9]+)?".r).map(_.toInt)
//    lazy val bool  = "true" | "false"
//    lazy val array = "[" +> manyWithSep(",")(value) ++ "]"
//    lazy val value: Parser[Any] = obj | word | array | bool | none | digit
//    lazy val obj = "{" + manyWithSep(",")(word + ":" + value) + "}"
//
//  }

}
