package module3

import module2.type_classes.Eq.eqStr.===
import module3.zioConcurrency.printEffectRunningTime
import zio.{Has, Layer, RIO, Schedule, Task, UIO, ULayer, URIO, ZIO, ZLayer}
import zio.clock.{Clock, sleep}
import zio.console._
import zio.duration.durationInt
import zio.macros.accessible
import zio.random._

import java.io.IOException
import java.util.concurrent.TimeUnit
import scala.io.StdIn
import scala.language.postfixOps

package object zio_homework {
  /**
   * 1.
   * Используя сервисы Random и Console, напишите консольную ZIO программу которая будет предлагать пользователю угадать число от 1 до 3
   * и печатать в когнсоль угадал или нет. Подумайте, на какие наиболее простые эффекты ее можно декомпозировать.
   */

  import zio.console._

  //def getUserInput(message: String): ZIO[Console, IOException, Int] =
  //  putStrLn(message).flatMap(_ => getStrLn.map(el => el.toInt))

  import cats.implicits._

  lazy val guessProgram: ZIO[Console, IOException, Unit] = for {
    _ <- putStrLn("Hi, u need to guess the number")
    _ <- putStrLn("Please write one number 1, 2 or 3:")
    //input <- getUserInput(StdIn.readLine)
    input <- ZIO.effectTotal(StdIn.readLine())
    num <- Random.Service.live.nextIntBetween(0, 4)
    success <- ZIO.effectTotal(if (input.toInt === num) true else false)
    _ <- putStrLn(if (success) s"Your input is $input == $num! You won!" else s"Your input is $input != $num! You lost!")
  } yield ()

  /**
   * 2. реализовать функцию doWhile (общего назначения), которая будет выполнять эффект до тех пор, пока его значение в условии не даст true
   *
   */

  def isTrue(num: Int): Boolean = if (num < 7) false else true

  def randomizer(e: Int = 100): ZIO[Console, Nothing, Boolean] = for {
    num <- Random.Service.live.nextIntBetween(0, 10)
    _ <- putStrLn(if (num < 7) s"Current number $num is smaller than 7" else s"Current number $num is bigger than 7")
    s <- ZIO.effectTotal(isTrue(num))
  } yield s

  // тут конечно можно дженериков наделать, но у нас же типа компайл тайм проверки все дела...
  def doWhile(f: ZIO[Console, Nothing, Boolean]): ZIO[Console, Nothing, Boolean] = f.repeatWhile(bool => !bool)


  // ZIO 2.0 => извините, не завезли пока на курсы
  /*ZIO.ifZIO(
  Random.nextIntBounded(10)
    .debug("random number")
    .map(_ % 2)
    .debug("remainder")
    .map(_ == 0)
)(
  onTrue = ZIO.succeed("Success"),
  onFalse = ZIO.succeed("Failure")
).debug.repeatWhile(_ != "Success")*/

  /**
   * 3. Реализовать метод, который безопасно прочитает конфиг из файла, а в случае ошибки вернет дефолтный конфиг
   * и выведет его в консоль
   * Используйте эффект "load" из пакета config
   */

  // Implementation 1

  import zio.config._
  import zio.config.magnolia.DeriveConfigDescriptor.{Descriptor, descriptor}

  import zio.IO

  import zio.config._, ConfigDescriptor._, ConfigSource._

  case class MyConfig(ldap: String, port: Int, dburl: String)

  val myConfigAutomatic = descriptor[MyConfig]

  val map =
    Map(
      "LDAP" -> "xyz",
      "PORT" -> "8888",
      "DB_URL" -> "postgres"
    )

  val source = ConfigSource.fromMap(map)

  read(myConfigAutomatic from source)
  // Either[ReadError[String], MyConfig]

  // Alternatively, you can rely on `Config.from..` pattern to get ZLayers.
  val result: Layer[ReadError[String], Has[MyConfig]] =
    ZConfig.fromMap(map, myConfigAutomatic)


  def loadConfigOrDefault = result

  // Implementation 2 Эта повеселее
  // Она позволяет поискать данные не только в файлах, но и в стринге или System properties или env vars

  import java.io.File

  import zio.{App, ExitCode, URIO, ZIO, system}
  import zio.config.typesafe._
  import zio.console.{Console, putStrLn}

  final case class Config(username: String, password: String)

  val getDesc: ZIO[system.System, ReadError[String], ConfigDescriptor[Config]] =
    for {
      hoconFile <- ZIO.fromEither(TypesafeConfigSource.fromHoconFile(new File("/invalid/path")))
      constant <- ZIO.fromEither(
        TypesafeConfigSource
          .fromHoconString(
            s"""
                                      {
                                        port : 123,
                                        url  : bla,
                                        region: useast,
                                        username: Jora,
                                        password: 12345
                                      }
                                  """))
      env <- ConfigSource.fromSystemEnv
      sysProp <- ConfigSource.fromSystemProperties
      source = hoconFile <> constant <> env <> sysProp
    } yield (descriptor[Config] from source)

  val application: ZIO[Console with system.System, String, Unit] =
    for {
      desc <- getDesc.mapError(_.prettyPrint())
      configValue <- ZIO.fromEither(read(desc)).mapError(_.prettyPrint())
      string <- ZIO.fromEither(configValue.toJson(desc))
      _ <- putStrLn(string)
    } yield ()


  /**
   * 4. Следуйте инструкциям ниже для написания 2-х ZIO программ,
   * обратите внимание на сигнатуры эффектов, которые будут у вас получаться,
   * на изменение этих сигнатур
   */


  /**
   * 4.1 Создайте эффект, который будет возвращать случайеым образом выбранное число от 0 до 10 спустя 1 секунду
   * Используйте сервис zio Random
   */
  lazy val eff: ZIO[Clock, Nothing, Int] = Random.Service.live.nextIntBetween(0, 10) <* ZIO.sleep(1 second)

  /**
   * 4.2 Создайте коллукцию из 10 выше описанных эффектов (eff)
   */

  lazy val effects: (Int, ZIO[Clock, Nothing, Int]) =>
    ZIO[Console with Clock, Nothing, IndexedSeq[Int]] =
    (n: Int, eff: ZIO[Clock, Nothing, Int]) =>
      ZIO.collectAll((0 to n).map { _ =>
        val z = for {
          s <- eff
          _ <- putStrLn(s.toString)
        } yield s
        z
      })


  /**
   * 4.3 Напишите программу которая вычислит сумму элементов коллекци "effects",
   * напечатает ее в консоль и вернет результат, а также залогирует затраченное время на выполнение,
   * можно использовать ф-цию printEffectRunningTime, которую мы разработали на занятиях
   */

  lazy val app = (effs: ZIO[Console with Clock, Nothing, IndexedSeq[Int]]) => for {
    sum <- effs.map(el => el.sum)
    _ <- putStrLn(s"Collection sum = $sum")
  } yield ZIO.effectTotal(sum)


  /**
   * 4.4 Усовершенствуйте программу 4.3 так, чтобы минимизировать время ее выполнения
   */

  lazy val appSpeedUp = (n: Int, eff: ZIO[Clock, Nothing, Int]) => for {
    seq <- ZIO.collectAllPar((0 to n).map { _ =>
      val z = for {
        s <- eff
        _ <- putStrLn(s.toString)
      } yield s
      z
    })
    sum <- ZIO.effect(seq.sum)
    _ <- putStrLn(s"Collection sum = $sum")
  } yield sum



  /**
   * 5. Оформите ф-цию printEffectRunningTime разработанную на занятиях в отдельный сервис, так чтобы ее
   * молжно было использовать аналогично zio.console.putStrLn например
   */


  /**
   * 6.
   * Воспользуйтесь написанным сервисом, чтобы созадть эффект, который будет логировать время выполнения прогаммы из пункта 4.3
   *
   *
   */

  lazy val appWithTimeLogg = (eff: ZIO[Clock, Nothing, Int]) => for {
    _ <- printEffectRunningTime(eff)
  } yield ()

  /**
   *
   * Подготовьте его к запуску и затем запустите воспользовавшись ZioHomeWorkApp
   */

  lazy val runApp = ???

}
