import scala.collection.immutable
import scala.collection.immutable.Nil

object targil1 {

  /**----------Functions----------**/
  /**Q1**/
  def evenPlaces[A](alist: List[A]): List[A] = {

    if (alist.isEmpty) {
      return List()
    }

    val RecursionList = evenPlaces(alist.tail)

    if (alist.length % 2 != 0) {

      val headList = List(alist.head)
      val toReturn = List(headList, RecursionList).flatten
      toReturn

    } else {
      RecursionList
    }
  }

  /**Q2**/
  def zipMap[A, B](alist: List[A], funcList: List[A => B]): List[B] = {

    if (funcList.isEmpty | alist.isEmpty) {
      return List()
    }

    val RecursionList = zipMap(alist.tail, funcList.tail)

    val listToReturn = List(List(funcList.head(alist.head)), RecursionList).flatten
    listToReturn
  }
  /**Q3**/
  //*trait cannot be written with constructor in the signature, like MyList[+A](...) {...}, only if it was abstract class
  //*a class can extend many traits, a class can extend many traits and one abstract
  //*to define a method in a trait/abstract class that cannot be changed by an extender use final def ...
  sealed trait MyList[+A] {

    def isEmpty: Boolean

    def head: A

    def tail: MyList[A]

    def foldLeft[B](initial: B, f: (B, A) => B): B

    def takeWhile(p: A => Boolean): MyList[A]

    def enumerate: MyList[(Int, A)]

    def dmap[B](f: A => B, g: A => B): MyList[B]

    def checkTheLength[A](myListToCheck: MyList[A]): Int

  }

  //*cannot extend final and cannot extend object
  final case object Empty extends MyList[Nothing] {

    override def head: Nothing = throw new NoSuchElementException("head of empty list")

    override def tail: MyList[Nothing] = throw new NoSuchElementException("tail of empty list")

    override def isEmpty = true

    override def foldLeft[B](initial: B, f: (B, Nothing) => B): B = return initial

    override def takeWhile(p: Nothing => Boolean): MyList[Nothing] = return tail

    override def enumerate: MyList[(Int, Nothing)] = return Empty

    override def dmap[B](f: Nothing => B, g: Nothing => B): MyList[B] = return Empty

    override def checkTheLength[A](myListToCheck: MyList[A]): Int = return 0
  }

  //*cannot extend object
  final case class Pair[A](head: A, tail: MyList[A]) extends MyList[A] {

    override def isEmpty = false

    override def foldLeft[B](initial: B, f: (B, A) => B): B = {
      if (head == Empty) return initial                              //Stop condition for the recursion

      val funcValueEachRecursion = f(initial, head)                  //calculate the function before each call of the recursion on the initial value of the method and the head value of the class(each recursion is opens with "a new class" as the next item in the tail
      val recursionReturn = tail.foldLeft(funcValueEachRecursion, f) //call the method in recursion each time, with the value on the first and the head of the current recursion class, in the last call when the tail is empty the initial value is the value to return, it returned through all the recursion stages and back to the main
      return recursionReturn
    }

    override def takeWhile(p: A => Boolean): MyList[A] = {
      if (head == Empty) return Empty                                //Stop condition for the recursion, return Empty if ran on the first pair until the end

      val funcValueEachRecursion = p(head)                           //check each recursion the value of the function, if true go down for another recursion iteration

      if (funcValueEachRecursion) {
        val recursionPairReturned = tail.takeWhile(p)
        return Pair(head, recursionPairReturned)
      } else Empty                                                   //return Empty for the first head of a pair in some recursion returned false for p function
    }

    override def enumerate: MyList[(Int, A)] = {
      if (head == Empty) return Empty                                //Stop condition for the recursion
      else {
        val recursionPairReturned = tail.enumerate

        return Pair((1, head), recursionPairReturned)
      }
    }

    override def dmap[B](f: A => B, g: A => B): MyList[B] = {
      if (head == Empty) return Empty
      else {
        val checkTheLength = this.checkTheLength(this)
        if (checkTheLength % 2 == 0) {
          return new Pair(f(head), tail.dmap(f, g))
        } else {
          return new Pair(g(head), tail.dmap(f, g))
        }
      }
    }

    override def checkTheLength[A](myListToCheck: MyList[A]): Int = {
      if (myListToCheck.head == Empty) return 0
      else {
        return 1 + tail.checkTheLength(myListToCheck)
      }
    }
  }

  /**Q4**/
  sealed trait MyList2[+A] {

    def isEmpty: Boolean

    def head: A

    def tail: MyList2[A]

    def foldLeft[B](initial: B, f: (B, A) => B): B = this match {
      case Empty2 => initial
      case pairTo: MyList2[A] => {
        val funcValueEachRecursion = f(initial, head)
        val recursionReturn = tail.foldLeft(funcValueEachRecursion, f)
        return recursionReturn
      }
    }

    def takeWhile(p: A => Boolean): MyList2[A] = this match {

      case Empty2 => Empty2
      case pairTo: MyList2[A] => {
        val funcValueEachRecursion = p(head)
        if (funcValueEachRecursion) {
          val recursionPairReturned = tail.takeWhile(p)
          return Pair2(head, recursionPairReturned)
        } else Empty2
      }

    }
  }

  case class Pair2[A](head: A, tail: MyList2[A]) extends MyList2[A] {
    def isEmpty = false
  }

  case object Empty2 extends MyList2[Nothing] {
    def isEmpty = true
    def head: Nothing = throw new NoSuchElementException("head of empty list")
    def tail: MyList2[Nothing] = throw new NoSuchElementException("tail of empty list")
  }
  /**Q5 - 1**/
  sealed trait Expr {

    def eval: Int

    def complexity: Int
  }

  final case class Plus(left: Expr, right: Expr) extends Expr {
    override def eval: Int = return left.eval + right.eval
    override def complexity: Int = return 1 + right.complexity
  }

  final case class Minus(left: Expr, right: Expr) extends Expr {
    def eval: Int = return left.eval - right.eval
    override def complexity: Int = return 1 + right.complexity
  }

  final case class Mul(left: Expr, right: Expr) extends Expr {
    def eval: Int = return left.eval * right.eval
    override def complexity: Int = return 2 + right.complexity
  }

  final case class Number(n: Int) extends Expr {
    def eval: Int = return n
    override def complexity: Int = return 0
  }

  /**Q6**/
  def map2[A, B, C](o1: Option[A], o2: Option[B], f: (A, B) => C): Option[C] = {
    if (!(o1.isDefined)) return None
    if (!(o2.isDefined)) return None
    Option(f(o1.get, o2.get))
  }

  /**-----------------------------------------------main-----------------------------------------------**/
  def main(args: Array[String]) {

    /**Q1**/
    val listToCast = List(1, 2, 3, 4, 5, 6, 7)                                            //integer list
    val listToCast2 = List("1", "2", "3", "4", "5")                                       //strings list
    val listToCast3 = listToCast.map((x: Int) => x * 2)                                   //integer list, applied the function first on each element first and then sent it
    val listToCast4 = List(List(1, 2, 3), List(4, 5, 6), List(7, 8, 9), List(10, 11, 12)) //lists list

    //print(evenPlaces(listToCast))
    //println
    //print(evenPlaces(listToCast2))
    //println
    //print(evenPlaces(listToCast3))
    //println
    //print(evenPlaces(listToCast4))
    //println

    /**Q2**/
    val example1func1 = (x: Int) => x + 1                                                 //a function literal
    val example1func2 = (x: Int) => x + 2                                                 //a function literal
    val example1func3 = (x: Int) => x + 3                                                 //a function literal
    val example1funcList: List[Int => Int] = List(example1func1, example1func2, example1func3)
    val example1ListToSend = List(1, 2, 3)
    //print(zipMap(example1ListToSend, example1funcList))
    //println()
    val example2func1 = (stringToCalculateOn: String) => stringToCalculateOn.toLowerCase()//a function literal
    val example2func2 = (stringToCalculateOn: String) => stringToCalculateOn.toString()   //a function literal
    val example2func3 = (stringToCalculateOn: String) => stringToCalculateOn.toUpperCase()//a function literal
    val example2funcList: List[String => String] = List(example2func1, example2func2, example2func3)
    val example2ListToSend = List("SHALEV", "Ozi", "Dovev")
    //print(zipMap(example2ListToSend, example2funcList))
    //println()
    val example3func1 = (IntToCalculate: String) => IntToCalculate.hashCode()             //a function literal
    val example3func2 = (IntToCalculate: String) => IntToCalculate.length()               //a function literal
    val example3func3 = (IntToCalculate: String) => IntToCalculate.codePointAt(3)         //a function literal
    val example3funcList: List[String => Int] = List(example3func1, example3func2, example3func3)
    val example3ListToSend = List("Shalev", "Ayala", "Dovev", "Moshe")
    //print(zipMap(example3ListToSend, example3funcList))
    //println()

    /**Q3**/
    //---first function
    val tempPair1 = new Pair(10, new Pair(20, new Pair(30, Empty)))
    //print(tempPair1)
    //println
    //print(tempPair1.foldLeft(0, (x: Int, y: Int) => x + y))

    //---second function
    val alist = Pair(5, Pair(6, Pair(7, Pair(8, Empty))))
    //print(alist)
    //println
    //print(alist.takeWhile(_ < 7))
    //println

    //---third function
    val alist1 = Pair(5, Pair(6, Pair(7, Pair(8, Empty))))
    //print(alist1.enumerate)
    //println

    //---forth function
    val alist2 = Pair(5, Pair(6, Pair(7, Pair(8, Empty))))
    //print(alist2.dmap((x => x.toString + x), (y => y.toString + y + y)))

    /**Q4**/
    //---first function
    val tempPair3 = new Pair2(10, new Pair2(20, new Pair2(30, Empty2)))
    //print(tempPair3)
    //println
    //print(tempPair3.foldLeft(0, (x: Int, y: Int) => x + y))
    //println
    //---second function
    val alist3 = Pair2(5, Pair2(6, Pair2(7, Pair2(8, Empty2))))
    //print(alist3)
    //println
    //print(alist3.takeWhile(_ < 7))
    //println

    /**Q5**/
    //---first function
    val n1 = Plus(Number(3), Mul(Number(4), Number(5)))
    //print(n1.eval)
    //println
    val n2 = Plus(Number(3), Number(5))
    //print(n2.eval)
    //println
    val n3 = Minus(Number(53), Mul(Number(3), Mul(Number(4), Number(5))))
    //print(n3.eval)
    //println

    //---second function
    //print(n1.complexity)
    //println
    //print(n2.complexity)
    //println
    //print(n3.complexity)
    //println

    //---third function

    /**Q6**/
    //to print without the some when returned need to check with pattern much if not None, if not print(Options explanation Page)
    //print(map2(Some(4), Some(5), (x: Int, y: Int) => x + y))
    //println
    //print(map2(None, Some(5), (x: Int, y: Int) => x + y))
    //println
    //print(map2(Some(4), None, (x: Int, y: Int) => x + y))
    //println
    //print(map2(Some("hello"), Some("world"), (x: String, y: String) => x + " " + y))
    //println
    //print(map2(Some(List(1.5, 2.5)), Some(List(3.5, 4.5)), (x: List[Double], y: List[Double]) => List(x, y).flatten))
    //println
  }
}