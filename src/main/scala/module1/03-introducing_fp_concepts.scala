package module1


import java.util.UUID
import scala.annotation.tailrec
import java.time.Instant
import scala.language.postfixOps
import scala.runtime.Nothing$



/**
 * referential transparency
 */
 object referential_transparency{


  case class Abiturient(id: String, email: String, fio: String)

  type Html = String

  sealed trait Notification

  object Notification{
    case class Email(email: String, text: Html) extends Notification
    case class Sms(telephone: String, msg: String) extends Notification
  }


  case class AbiturientDTO(email: String, fio: String, password: String)

  trait NotificationService{
    def sendNotification(notification: Notification): Unit
    def createNotification(abiturient: Abiturient): Notification
  }

  trait AbiturientService{

    def registerAbiturient(abiturientDTO: AbiturientDTO, uuid: String): Abiturient
  }

  class AbiturientServiceImpl(notificationService: NotificationService) extends AbiturientService{

    override def registerAbiturient(abiturientDTO: AbiturientDTO, uuid: String): Abiturient = {
      val abiturient = Abiturient(uuid, abiturientDTO.email, abiturientDTO.fio)
      notificationService.sendNotification(Notification.Email(abiturient.email, "Some message"))
      abiturient
    }

  }
}


 // recursion

object recursion {

  /**
   * Реализовать метод вычисления n!
   * n! = 1 * 2 * ... n
   */

   def fact(n: Int): Int = {
       var _n = 1
       var i = 2
       while(i <= n){
           _n *=  i
           i += 1
       }
       _n
   }


   def factRec(n: Int): Int = {
    if (n<=0) 1 else n * factRec(n-1) 
   }

 
   def factTailRec(n: Int): Int = {
       @tailrec
       def loop(n: Int, accum: Int): Int = 
           if(n == 1) accum
           else loop(n - 1, n * accum)
        loop(n, 1)
   }

  


  /**
   * реализовать вычисление N числа Фибоначчи
   * F0 = 0, F1 = 1, Fn = Fn-1 + Fn - 2
   *
   */


}

object hof{

   trait Consumer{
       def subscribe(topic: String): LazyList[Record]
   }

   case class Record(value: String)

   case class Request()
   
   object Request {
       def parse(str: String): Request = ???
   }

   def createRequestSubscription() = {
       val cons: Consumer = ???

       val stream: LazyList[Record] = cons.subscribe("request")

       stream.foreach{ rec =>
            val req: Request = Request.parse(rec.value)
            // save(request)
       }
   }

   def createSubscription[T](topic: String, action: Record => T): Unit = {
       val cons: Consumer = ???

       val stream: LazyList[Record] = cons.subscribe(topic)
       stream.foreach{ rec =>
            action(rec)
       }
   }
   def createRequestSubscription2 = createSubscription("request", r => {
       val req: Request = Request.parse(r.value)
            // save(request)
   })
  

  // обертки

  def logRunningTime[A, B](f: A => B): A => B = { a =>
        val start = System.currentTimeMillis()
        val result = f(a)
        val end = System.currentTimeMillis()
        println(end - start)
        result
  }






  // изменение поведения ф-ции

  val arr = Array(1, 2, 3, 4, 5)

  def isOdd(i: Int): Boolean = i % 2 > 0

  
   
  def not[A](f: A => Boolean): A => Boolean = a => ! f(a) 
  
  
  lazy val isEven = not(isOdd)




  // изменение самой функции

  // Follow type implementation

  def partial[A, B, C](a: A, f: (A, B) => C): B => C = b => f(a, b)

  def sum(x: Int, y: Int): Int = x + y

  val res: Int => Int = partial(1, sum)

  res(3) 


}






/**
 *  Реализуем тип Option
 */


 object opt {

  /**
   *
   * Реализовать тип Option, который будет указывать на присутствие либо отсутсвие результата
   */

  sealed trait Option[+T] {
    def isEmpty: Boolean = this match {
      case Option.Some(v) => false
      case Option.None => true
    }

    def get: T = this match {
      case Option.Some(v) => v
      case Option.None => throw new Exception("Get on empty option")
    }

    def map[B](f: T => B): Option[B] = this match {
      case Option.Some(v) => Option.Some(f(v))
      case Option.None => Option.None
    }

    def flatMap[B](f: T => Option[B]): Option[B] = this match {
      case Option.Some(v) => f(v)
      case Option.None => Option.None
    }

    def printIfAny: Unit = this match {
      case Option.Some(v) => println(v)
      case Option.None =>
    }


    object Option {
      case class Some[T](v: T) extends Option[T]

      case object None extends Option[Nothing]

      def apply[T](v: T): Option[T] = ???


    }



    /**
     *
     * Реализовать метод printIfAny, который будет печатать значение, если оно есть
     */

    def printIfAny[A](v: Option[A]) = {
      v match {
        case Option.Some(v) => println(v)
        case Option.None => throw new Exception("There is nothing to print")
      }
    }

    /**
     *
     * Реализовать метод zip, который будет создавать Option от пары значений из 2-х Option
     */


    final def zip[A, B](other: Option[B]) = {
      if (!other.isEmpty && !this.isEmpty) Some((other.get, this.get))
      else if (other.isEmpty && !this.isEmpty) Some((null, this.get))
      else if (!other.isEmpty && this.isEmpty) Some((other.get, null))
      else None
    }

    /**
     *
     * Реализовать метод filter, который будет возвращать не пустой Option
     * в случае если исходный не пуст и предикат от значения = true
     */

    def filter(f: T => Boolean) = if (f(this.get)) this else None
  }

  object list {
    /**
     *
     * Реализовать односвязанный иммутабельный список List
     * Список имеет два случая:
     * Nil - пустой список
     * Cons - непустой, содердит первый элемент (голову) и хвост (оставшийся список)
     */

    import List._
    sealed trait List[+A]{
      def ::[AA >: A](head: AA): List[AA] = Cons(head, this)

      def head(): A
      def tail(): List[A]
      def mkString: String = mkString(", ")

      def mkString(sep: String): String = {
        def go(l: List[A], acc: StringBuilder): StringBuilder = {
          l match {
            case List.Nil => acc
            case h :: Nil => acc.append(s"$h")
            case h :: t => go(t, acc.append(s"$h$sep"))
          }
        }
        go(this, new StringBuilder()).toString()
      }

      def reverse: List[A] = {
        def go(input: List[A], acc: List[A] = List[A]()): List[A] = {
          input match {
            case List.Nil => acc
            case List.::(head, tail) => go(tail, head :: acc)
          }
        }
        go(this)
      }

      def funcElem[B](f: A => B): List[B] = {
        def go(xs: List[A], acc: List[B] = List[B]()): List[B] = {
          xs match {
            case List.Nil => acc
            case List.::(head, tail) => go(tail, f(head) :: acc)
          }
        }
        go(this)
      }

      def map[B](f: (A) => B): List[B] = {
        this match {
          case Cons(xh: A, Nil) => Cons(f(xh), Nil)
          case Cons(xh: A, xs: List[A]) => Cons(f(xh), xs.map(f))
          case a: List[A] => Cons(f(a.head()), a.tail.map(f))
        }
      }
      def   fail(m: String) = throw new NoSuchElementException(m)
    }


    object List{
      case object Nil extends List[Nothing] {
        override def head: Nothing = fail("An empty list.")
        override def tail: List[Nothing] = fail("An empty list.")
      }
      case class ::[A](head: A, tail: List[A]) extends List[A]
      val Cons = ::

      def apply[T](arg: T*): List[T] = {
        var l: List[T] = List.Nil
        arg.foreach(el => l = el :: l)
        l
      }

      def incList(xs: List[Int]): List[Int] = {
        def go(xs: List[Int], acc: List[Int] = List[Int]()): List[Int] = {
          xs match {
            case List.Nil => acc
            case List.::(head, tail) => go(tail, head + 1 :: acc)
          }
        }
        go(xs)
      }

      def shoutString(xs: List[String]): List[String] = {
        def go(xs: List[String], acc: List[String] = List[String]()): List[String] = {
          xs match {
            case Nil => acc
            case ::(head, tail) => go(tail, "!" + head :: acc )
          }
        }
        go(xs)
      }
    }



    val list = 1 :: List.Nil

      /**
       * Метод cons, добавляет элемент в голову списка, для этого метода можно воспользоваться названием `::`
       *
       */



      /**
       * Метод mkString возвращает строковое представление списка, с учетом переданного разделителя
       *
       */



      /**
       * Конструктор, позволяющий создать список из N - го числа аргументов
       * Для этого можно воспользоваться *
       *
       * Например вот этот метод принимает некую последовательность аргументов с типом Int и выводит их на печать
       * def printArgs(args: Int*) = args.foreach(println(_))
       */

      /**
       *
       * Реализовать метод reverse который позволит заменить порядок элементов в списке на противоположный
       */

      /**
       *
       * Реализовать метод map для списка который будет применять некую ф-цию к элементам данного списка
       */


      /**
       *
       * Реализовать метод filter для списка который будет фильтровать список по некому условию
       */

      /**
       *
       * Написать функцию incList котрая будет принимать список Int и возвращать список,
       * где каждый элемент будет увеличен на 1
       */


      /**
       *
       * Написать функцию shoutString котрая будет принимать список String и возвращать список,
       * где к каждому элементу будет добавлен префикс в виде '!'
       */
    }

}