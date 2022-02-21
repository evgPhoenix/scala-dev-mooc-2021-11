package module3.zio_homework

import module3.zioConcurrency.currentTime
import zio.clock.Clock
import zio.console.{Console, putStrLn}
import zio.macros.accessible
import zio.{Has, ULayer, URIO, ZIO, ZLayer, clock}

import java.util.concurrent.TimeUnit


package object timeservice {
    type TimeService = Has[TimeService.Service]

    //@accessible
    object TimeService{
        trait Service{
            def printEffectRunningTime[R, E, A](zio: ZIO[R, E, A]): ZIO[Console with Clock with R, E, A]
        }

        val live: ULayer[TimeService] = ZLayer.succeed(
            new Service {
                override def printEffectRunningTime[R, E, A](zio: ZIO[R, E, A]): ZIO[Console with Clock with R, E, A] = for {
                    start <- clock.currentTime(TimeUnit.SECONDS)
                    z <- zio
                    finish <- clock.currentTime(TimeUnit.SECONDS)
                    _ <- putStrLn(s"Running time: ${finish - start}")
                } yield z
        })

    }

    // Тут не компилируется \/\/\/
    def printEffectRunningTime[R, E, A](zio: ZIO[R, E, A]): ZIO[Console with Clock with R, E, A] = {
       ZIO.accessM(_.get.printEffectRunningTime(zio))
    }

}
