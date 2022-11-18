package module3.zio_homework

import zio.clock.Clock
import zio.console.Console
import zio.random.Random
import zio.{ExitCode, URIO, ZIO}

import scala.io.StdIn

object ZioHomeWorkApp extends zio.App {
  override def run(args: List[String]): URIO[Clock with Random with Console, ExitCode] = {
  //guessProgram.exitCode
  //doWhile(ZIO.effect(StdIn.readLine()).flatMap(x => ZIO.effect(x)))(x  => x == "10").exitCode
  //loadConfigOrDefault.exitCode
  //eff.exitCode
    runApp.exitCode
  }
}
