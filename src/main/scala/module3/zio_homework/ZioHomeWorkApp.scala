package module3.zio_homework
import module3.zioConcurrency
import zio.clock.Clock
import zio.console.Console
import zio.random.Random
import zio.{ExitCode, URIO}

object ZioHomeWorkApp extends zio.App {
  //  override def run(args: List[String]): URIO[Clock with Random with Console, ExitCode] =
  //    zio.Runtime.default.unsafeRun(???)

  override def run(args: List[String]) = {

    // first hw
    //guessProgram.exitCode

    //loadConfigOrDefault

    // second hw
    //doWhile(randomizer()).exitCode

    // 3rd
    //loadConfigOrDefault
    //application.either.flatMap(r => putStrLn(s"Result: ${r}")).exitCode

    //4.2
    //effects(10, eff).exitCode
    //4.3
    //Running time: 11
    //    zioConcurrency.printEffectRunningTime(
    //      app(effects(10, eff)).exitCode
    //    ).exitCode
    zioConcurrency.printEffectRunningTime(effects(10, eff)).exitCode
    //4.4
    //Running time: 1
    //    zioConcurrency.printEffectRunningTime(
    //      appSpeedUp(10, eff)
    //    ).exitCode
  }
}
