package module3

import cats.effect.IO
import module3.cats_effect_homework.{FileWallet, Wallet}
import module3.zio_homework.config
import module3.zio_homework.config.AppConfig
import zio.console.Console
import zio.{ExitCode, URIO, ZIO}

object App {
  def main(args: Array[String]): Unit = {

    println(new FileWallet[IO]("555").balance)
//    zio.Runtime.default.unsafeRun(multipleErrors.app)

    // toyModel.echo.run()


  }
}

object ZioApp extends zio.App{
  override def run(args: List[String]): URIO[zio.ZEnv, ExitCode] =
    zioDS.ref.updateCounterRef.flatMap(i => zio.console.putStrLn(i.toString))
      .exitCode
}