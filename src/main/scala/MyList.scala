import scala.util.Random

sealed trait MyList[+A]
case object Nil extends MyList[Nothing]
case class Cons[+A](head: A, tail: MyList[A]) extends MyList[A]

object MyList {

  def sum(ints: MyList[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) =>
      x + sum(
        xs
      )
  }

  def _product(ds: MyList[Double]): Double = ds match {
    case Nil => 1
    case Cons(x, xs) => x * _product(xs)
  }

  def product(ds: MyList[Double]): Double = ds match {
    case Nil => 0
    case _ => _product(ds)
  }

  def apply[A](as: A*): MyList[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))


  def append[A](a1: MyList[A], a2: MyList[A]): MyList[A] = a1 match {
    case Nil => a2
    case Cons(head, tail) => Cons(head, append(tail, a2))
  }

  def tail[A](l: MyList[A]): MyList[A] = l match {
    case Nil => Nil
    case Cons(_, tail) => tail
  }

  def head[A](l: MyList[A]): A = l match {
    case Nil => throw new Exception("Empty list")
    case Cons(head, _) => head
  }

  def setHead[A](l: MyList[A], h: A): MyList[A] = l match {
    case Nil => Cons(h, Nil)
    case Cons(_, tail) => Cons(h, tail)
  }

  def drop[A](l: MyList[A], n: Int): MyList[A] = {
    if (l == Nil)
     Nil
    else
      n match {
        case 0 => tail(l)
        case n => Cons(head(l), drop(tail(l), n-1))
    }
  }

  def dropWhile[A](l: MyList[A], f: A => Boolean): MyList[A] = {
    if (l == Nil)
      Nil
    else
      f(head(l)) match {
        case true => dropWhile(tail(l), f)
        case false => l
      }
  }

  def filter[A](l: MyList[A], f: A => Boolean): MyList[A] = {
    if (l == Nil)
      Nil
    else
      f(head(l)) match {
        case true => Cons(head(l), filter(tail(l), f))
        case false => filter(tail(l), f)
      }
  }

  def init[A](l: MyList[A]): MyList[A] = l match {
    case Nil => Nil
    case Cons(_, Nil) => Nil
    case Cons(head, tail) => Cons(head, init(tail))
  }

  def _length[A](l: MyList[A], acc: Int): Int = l match {
    case Nil => acc
    case Cons(_, tail) => _length(tail, acc + 1)
  }

  def length[A](l: MyList[A]): Int = {
    _length(l, 0)
  }

  def reverse[A](l: MyList[A]): MyList[A] = l match {
    case Nil => Nil
    case Cons(_, Nil) => l
    case Cons(h1, Cons(h2, Nil)) => Cons(h2, Cons(h1, Nil))
    case Cons(head, tail) => append(reverse(tail), Cons(head, Nil))
  }

  def get[A](n: Int, l: MyList[A]): A = n match{
    case 0 => head(l)
    case n => get(n-1, tail(l))
  }

  def getRandom[A](l: MyList[A]): A = {
    val rand = new scala.util.Random
    val n = rand.nextInt(length(l))
    get(n, l)
  }

}

object main {
  def main(args: Array[String]): Unit = {
    val l = Cons(1.0, Cons(2.0, Cons(3.0, Cons(3.0, Cons(8.0, Nil)))))
  }
}