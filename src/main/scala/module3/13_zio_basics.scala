package module3

import zio.clock.{Clock, nanoTime}
import zio.console.{Console, getStrLn}

import java.io.IOException
import scala.concurrent.Future
import scala.io.{BufferedSource, Source, StdIn}
import scala.util.Try
import zio.duration._

import scala.language.postfixOps
import zio.{IO, Managed, RIO, Task, UIO, URIO, ZIO, ZManaged}



/** **
 * ZIO[-R, +E, +A] ----> R => Either[E, A]
 *
 */

// val f: String => Either[Throwable, Int] = ???

// f()

object toyModel {


  /**
   * Используя executable encoding реализуем свой zio
   */

  case class ZIO[-R, +E, +A](run: R => Either[E, A]){ self =>

    def map[B](f: A => B): ZIO[R, E, B] =
      ZIO(r => self.run(r).map(f))

    def flatMap[R1 <: R, E1 >: E, B](f: A => ZIO[R1, E1, B]): ZIO[R1, E1, B] =
      ZIO( r => self.run(r).fold(ZIO.fail, f).run(r))
  }




  /**
   * Реализуем конструкторы под названием effect и fail
   */
  object ZIO{

    def effect[A](a: => A): ZIO[Any, Throwable, A] = try{
      ZIO(_ => Right(a))
    } catch{
      case e: Throwable => fail(e)
    }

    def fail[E](e: E): ZIO[Any, E, Nothing] =
      ZIO(_ => Left(e))
  }







  /** *
   * Напишите консольное echo приложение с помощью нашего игрушечного ZIO
   */

  lazy val echo: ZIO[Any, Throwable, Unit] = for{
    str <- ZIO.effect(StdIn.readLine())
    _ <- ZIO.effect(println(str))
  } yield ()




  type Error
  type Environment

  // ZIO[-R, +E, +A]

  //type T[A] = ZIO[Any, Throwable, A]



  lazy val _: Task[Int] = Task(2) // ZIO[Any, Throwable, Int]
  lazy val _: IO[Error, Int] = IO(2) // ZIO[Any, Error, Int]
  lazy val _: RIO[Environment, Int] = RIO(StdIn.readLine) // ZIO[Environment, Throwable, Int]
  lazy val _: URIO[Environment, Int] = URIO(3) // ZIO[Environment, Nothing, Int]
  lazy val _: UIO[Int] = UIO(0) // ZIO[Any, Nothing, Int]
}

object zioConstructors {


  // константа
  val z1: UIO[Int] = ZIO.succeed(7)


  // любой эффект
  val z2: Task[Unit] = ZIO.effect(println("Hello"))

  // любой не падающий эффект

  val z3: UIO[Unit] = ZIO.effectTotal(println("Hello"))




  // From Future
  val f: Future[Int] = Future.successful(7)
  val z4: Task[Int] = ZIO.fromFuture(ec => f)


  // From try
  lazy val t: Try[String] = Try("Jopa")
  lazy val z5: Task[String] = ZIO.fromTry(t)



  // From either
  lazy val e: Either[String, Int] = Right(23)
  lazy val z6: IO[String,Int] = ZIO.fromEither(e)




  // From option
  lazy val opt : Option[Int] = Some(1)
  lazy val z7: IO[Option[Nothing], Int] = ZIO.fromOption(opt)
  val z77: URIO[Any,Option[Int]] = z7.option
  val z78: ZIO[Any,Option[Nothing],Int] = z77.some




  // From function
  lazy val z8: URIO[String, Int] = ZIO.fromFunction[String, Int](str => str.toInt)

  // особые версии конструкторов

  lazy val _: UIO[Unit] = ZIO.unit

  lazy val _: UIO[Option[Nothing]] = ZIO.none

  lazy val _: UIO[Nothing] = ZIO.never // while(true)

  lazy val _: ZIO[Any, Nothing, Nothing] = ZIO.die(new Throwable("Ooops"))

  lazy val _: ZIO[Any, Int, Nothing] = ZIO.fail(1)

}



object zioOperators extends App {

  /** *
   *
   * 1. Создать ZIO эффект который будет читать строку из консоли
   */

  lazy val readLine: Task[String] = Task(StdIn.readLine)

  /** *
   *
   * 2. Создать ZIO эффект который будет писать строку в консоль
   */

  def writeLine(str: String) = Task(println(str))

  /** *
   * 3. Создать ZIO эффект котрый будет трансформировать эффект содержащий строку в эффект содержащий Int
   */

  lazy val lineToInt = Task("123") flatMap(elem => Task(elem.toInt))
  /** *
   * 3.Создать ZIO эффект, который будет работать как echo для консоли
   *
   */

  lazy val echo = for {
    str <- RIO(StdIn.readLine)
    _ <- Task(println(str))
  } yield ()

  /**
   * Создать ZIO эффект, который будет привествовать пользователя и говорить, что он работает как echo
   */

  lazy val greetAndEcho = for {
    _ <- ZIO.succeed(println("Hi I'm echo server, please enter something"))
    str <- RIO(StdIn.readLine)
    _ <- Task(println(s" echo -> $str"))
  } yield ()

  // Другие варианты композиции

  lazy val a1: Task[Unit] = ZIO.succeed(println("Hi I'm echo server, please enter something"))
  lazy val b1: Task[String] = Task("Hi")


  lazy val z9: ZIO[Any,Throwable,(Unit, String)] = a1 zip b1

  lazy val z10: ZIO[Any,Throwable, String] = a1 *> b1

  lazy val z11: ZIO[Any,Throwable, Unit] = a1 <* b1


  // greet and echo улучшенный
  lazy val _: ZIO[Any, Throwable, Unit] = ???


  /**
   * Используя уже созданные эффекты, написать программу, которая будет считывать поочереди считывать две
   * строки из консоли, преобразовывать их в числа, а затем складывать их
   */

  val r1 = for {
    _ <- Task(println("Hi I'm server, please enter number 1"))
    str1 <- RIO(StdIn.readLine)
    _ <- Task(println("Hi I'm server, please enter number 2"))
    str2 <- RIO(StdIn.readLine)
    res <- ZIO(str1.toInt + str2.toInt)
    _ <- Task(println(s"Result is: $res"))
  } yield ()

  /**
   * Второй вариант
   */

  //val r2: ZIO[Any, Throwable, Int] =

  /**
   * Доработать написанную программу, чтобы она еще печатала результат вычисления в консоль
   */

  //lazy val r3 = ???


  lazy val a: Task[Int] = Task(0)
  lazy val b: Task[String] = Task("11")

  /**
   * последовательная комбинация эффектов a и b
   */
  lazy val ab1: ZIO[Any, Throwable, (Int, String)] = a zip b

  /**
   * последовательная комбинация эффектов a и b
   */
  lazy val ab2: ZIO[Any, Throwable, Int] = a <* b

  /**
   * последовательная комбинация эффектов a и b
   */
  lazy val ab3: ZIO[Any, Throwable, String] = a *> b


  /**
   * Последовательная комбинация эффета b и b, при этом результатом должна быть конкатенация
   * возвращаемых значений
   */
  lazy val ab4: ZIO[Any,Throwable, String] = b.zipWith(b)(_ + _)


  /**
   *
   * Другой эффект в случае ошибки
   */

  val ab5 = ab2.orElse(ab3)

  /**
   *
   * A as B
   */



  /**
   * Read from file using Zmanaged
   * */

  def readFileZio(file: String): Task[Iterator[String]] =
    ZIO(Source.fromFile(file))
      .bracket(
        s => URIO(s.close),
        s => ZIO(s.getLines())
      )

  def managedSource(file: String): ZManaged[Any, Throwable, BufferedSource] =
    Managed.make(ZIO(Source.fromFile(file)))(s => URIO(s.close))

  def readFileZioManaged(file: String): Task[List[String]] =
    managedSource(file).use(s => ZIO(s.getLines().toList))

  val d2 = for {
    res <- readFileZioManaged("build.sbt")
    _ <- Task(println(s"Result is: ${res.toList.mkString("\n")}"))
  } yield ()
  // echo server run
  //zio.Runtime.default.unsafeRun(r1)
  zio.Runtime.default.unsafeRun(d2)

  //def readFile(fileName: String): ZIO[Any, IOException, String] = IO()

  def parseFile(fileName: String): ZIO[Any, Throwable, Seq[String]] = {
    val path = "/Users/ar11/Downloads/1_scala/scala-dev-mooc-2021-11/src/main/resources/" + fileName
    val lines: zio.stream.Stream[Throwable, String] = {
      zio.stream.Stream.fromIteratorManaged(
        ZManaged
          .fromAutoCloseable(
            Task(Source.fromFile(path))
          )
          .map(_.getLines())
      )

    }

    val x = lines
      .runCollect
    // apart from formatting, this is actually new (evaluates all Chunks):
    //.map(_.toList)
    x
  }
  // из эффекта с ошибкой, в эффект который не падает

  //val d: URIO[Any,String] = readFile("").orDie
  val d: ZIO[Any, Throwable, Unit] = for {
    list <-  parseFile("build.sbt")
    _ <- Task(list.foreach(println))
  } yield ()


}
