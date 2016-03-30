package fpinscala.errorhandling


import com.sun.xml.internal.ws.resources.AddressingMessages
import sun.management.resources.agent_zh_CN

import scala.{Option => _, Some => _, Either => _, _}
import scala.math
// hide std library `Option`, `Some` and `Either`, since we are writing our own in this chapter

sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = this match {
    case Some(x) => Some(f(x))
    case None => None
  }

  def getOrElse[B>:A](default: => B): B = this match {
    case Some(x) => x
    case None => default
  }

// getOrElse issort of like "flatten."
  // so create an Option[Option[B]] then flatten it back to Option[B]
  def flatMap[B](f: A => Option[B]): Option[B] = {
    (this map f) getOrElse None
  }
  def orElse[B>:A](ob: => Option[B]): Option[B] = //if (this != None) this else ob
    this map (Some(_)) getOrElse ob

  def filter(f: A => Boolean): Option[A] = this flatMap (x => if (f(x)) Some(x) else None)

case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  def variance(xs: Seq[Double]): Option[Double] =  {
    mean(xs) flatMap(m => mean(xs.map(x => math.pow(x - m, 2))))
  }

  // how get an Option(c)?
  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    a flatMap(x => b map(y => f(x, y)))

 //sometimes just using a.method over a method gives type inferrence to method args
//  def sequence[A](a: List[Option[A]]): Option[List[A]] =
//    a.foldRight(Some(List()):Option[List[A]])((o, l) => map2(o, l)(_ +: _))

  def sequence[A](a: List[Option[A]]): Option[List[A]] =
    traverse(a)(x => x)
  //val sequence = traverse(_)(x => x)

  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] =
    a.foldRight(Some(List()):Option[List[B]])((o, l) => map2(f(o), l)(_ +: _))
   val x = { 1 }
  sequence( List(Some(1), Some(2)))
  sequence(List(Some(3), None, Some(2)))
  def Try[A](x: => A): Option[A] =
    try Some(x)
    catch { case e: Exception => None }
  traverse(List("a", "1"))((x:String) => Try{x.toInt})
  traverse(List("3", "1"))((x:String) => Try{x.toInt})

}
