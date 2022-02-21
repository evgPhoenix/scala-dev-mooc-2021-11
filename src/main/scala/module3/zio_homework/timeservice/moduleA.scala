package module3.zio_homework.timeservice

import zio._
import zio.clock._
import zio.console._
import zio.duration.Duration._
import java.io.IOException

object moduleA {
  type ModuleA = Has[ModuleA.Service]

  object ModuleA {
    trait Service {
      def letsGoA(v: Int): UIO[String]
    }

    val any: ZLayer[ModuleA, Nothing, ModuleA] =
      ZLayer.requires[ModuleA]

    val live: Layer[Nothing, Has[Service]] = ZLayer.succeed {
      new Service {
        def letsGoA(v: Int): UIO[String] = UIO(s"done: v = $v ")
      }
    }
  }

  def letsGoA(v: Int): URIO[ModuleA, String] =
    ZIO.accessM(_.get.letsGoA(v))
}

