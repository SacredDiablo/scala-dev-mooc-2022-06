package module3

import module3.zioConcurrency.{currentTime, printEffectRunningTime}
import zio.{Has, Task, ULayer, URIO, ZIO, ZLayer}
import zio.clock.{Clock, sleep}
import zio.console._
import zio.duration.durationInt
import zio.random._

import java.io.IOException
import scala.language.postfixOps

package object zio_homework {
  /**
   * 1.
   * Используя сервисы Random и Console, напишите консольную ZIO программу которая будет предлагать пользователю угадать число от 1 до 3
   * и печатать в когнсоль угадал или нет. Подумайте, на какие наиболее простые эффекты ее можно декомпозировать.
   */



  lazy val guessProgram: ZIO[Console with Random, IOException, Unit] = for {
    rand <- nextIntBetween(1, 4)
    _ <- putStrLn(s"Угадай число от 1 до 3")
    num <- getStrLn.flatMap(input => ZIO.effect(input.toInt)).orDie /*или обрабатывать ошибку?*/
    _ <- putStrLn(if (num == rand) "Угадал" else s"Не угадал, я загадал $rand, твоё число ${num}")
  } yield ()
  /**
   * 2. реализовать функцию doWhile (общего назначения), которая будет выполнять эффект до тех пор, пока его значение в условии не даст true
   * 
   */

  def doWhile[R, E, A](effect: ZIO[R, E, A])(predicate: A => Boolean): ZIO[R, Any, Unit] =
    effect.flatMap(x => if (!predicate(x)) ZIO.effectTotal(println("Не корректный ввод, попробуй еще")) *> doWhile(effect)(predicate) else ZIO.succeed())



  /**
   * 3. Реализовать метод, который безопасно прочитает конфиг из файла, а в случае ошибки вернет дефолтный конфиг
   * и выведет его в консоль
   * Используйте эффект "load" из пакета config
   */


  def loadConfigOrDefault: URIO[Console, config.AppConfig] = config.load.orElse(
    zio.console.putStrLn(config.AppConfig("google.com", "8080").toString).as(config.AppConfig("google.com", "8080")))


  /**
   * 4. Следуйте инструкциям ниже для написания 2-х ZIO программ,
   * обратите внимание на сигнатуры эффектов, которые будут у вас получаться,
   * на изменение этих сигнатур
   */


  /**
   * 4.1 Создайте эффект, который будет возвращать случайеым образом выбранное число от 0 до 10 спустя 1 секунду
   * Используйте сервис zio Random
   */
    /*ZIO[Random with Clock, Nothing, Int] = URIO[Random with Clock, Int]*/
  lazy val eff: URIO[Random with Clock, Int] = nextIntBetween(0, 11).delay(1 second)

  /**
   * 4.2 Создайте коллукцию из 10 выше описанных эффектов (eff)
   */
  lazy val effects: URIO[Random with Clock, List[Int]] =  ZIO.collectAll(List.fill(10)(eff))

  
  /**
   * 4.3 Напишите программу которая вычислит сумму элементов коллекци "effects",
   * напечатает ее в консоль и вернет результат, а также залогирует затраченное время на выполнение,
   * можно использовать ф-цию printEffectRunningTime, которую мы разработали на занятиях
   */

  lazy val app = /*комичу для последнего задания printEffectRunningTime(*/for {
    list <- effects
    sum = list.sum
    _ <- putStrLn(s"$sum")
  } yield sum/*)*/

  /**
   * 4.4 Усовершенствуйте программу 4.3 так, чтобы минимизировать время ее выполнения
   */

  lazy val appSpeedUp = printEffectRunningTime(for {
    sum <- ZIO.reduceAllPar(eff, List.fill(10)(eff))(_ + _)
    _ <- putStrLn(s"$sum")
  } yield sum
  )


  /**
   * 5. Оформите ф-цию printEffectRunningTime разработанную на занятиях в отдельный сервис, так чтобы ее
   * молжно было использовать аналогично zio.console.putStrLn например
   */

  type PrintEffectRunningTime2 = Has[PrintEffectRunningTime2.Service]

  object PrintEffectRunningTime2 extends Serializable {
    trait Service extends Serializable {
      def printEffectRunningTime2[R, E, A](effect: ZIO[R, E, A]): ZIO[Console with Clock with R, E, A]
    }

    object Service extends Serializable {
      val live2: Service = new Service {
        override def printEffectRunningTime2[R, E, A](zio: ZIO[R, E, A]): ZIO[Console with Clock with R, E, A] = for{
          start <- currentTime
          z <- zio
          finish <- currentTime
          _ <- putStrLn(s"Running time(result from service): ${finish - start}")
        } yield z      }
    }


    val live: ULayer[PrintEffectRunningTime2] = ZLayer.succeed(new Service {
      override def printEffectRunningTime2[R, E, A](effect: ZIO[R, E, A]): ZIO[Console with Clock with R, E, A] = for{
        start <- currentTime
        z <- effect
        finish <- currentTime
        _ <- putStrLn(s"Running time(result from service): ${finish - start}")
      } yield z      } /*как в zio.console.putStrLn сделали live?*/
    )
  }

   /**
     * 6.
     * Воспользуйтесь написанным сервисом, чтобы созадть эффект, который будет логировать время выполнения прогаммы из пункта 4.3
     *
     * 
     */

  lazy val appWithTimeLogg: ZIO[PrintEffectRunningTime2 with Console with Clock with Random, Throwable, Int] =
    PrintEffectRunningTime2.Service.live2.printEffectRunningTime2(app)

  /**
    * 
    * Подготовьте его к запуску и затем запустите воспользовавшись ZioHomeWorkApp
    */

  lazy val runApp = appWithTimeLogg.provideSomeLayer[Console with Random with Clock](PrintEffectRunningTime2.live)
  
}
