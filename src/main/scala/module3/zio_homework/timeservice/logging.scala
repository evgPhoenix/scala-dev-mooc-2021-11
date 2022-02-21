package module3.zio_homework.timeservice

import zio.{Has, UIO, ULayer, URIO, ZIO, ZLayer}

object logging {
  type Logging = Has[Logging.Service]

  // Companion object exists to hold service definition and also the live implementation.
  object Logging {
    trait Service {
      def log(line: String): UIO[Unit]
    }

    val live: ULayer[Logging] = ZLayer.succeed {
      new Service {
        override def log(line: String): UIO[Unit] =
          ZIO.effectTotal(println(line))
      }
    }
  }

  // Accessor Methods
  def log(line: => String): URIO[Logging, Unit] =
    ZIO.accessM(_.get.log(line))
}
