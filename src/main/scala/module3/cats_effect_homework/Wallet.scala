package module3.cats_effect_homework

import cats.effect.Sync
import cats.implicits._
import Wallet._
import java.nio.file._
// DSL управления электронным кошельком
trait Wallet[F[_]] {
  // возвращает текущий баланс
  def balance: F[BigDecimal]
  // пополняет баланс на указанную сумму
  def topup(amount: BigDecimal): F[Unit]
  // списывает указанную сумму с баланса (ошибка если средств недостаточно)
  def withdraw(amount: BigDecimal): F[Either[WalletError, Unit]]
}

// Игрушечный кошелек который сохраняет свой баланс в файл
// todo: реализовать используя java.nio.file._
// Насчёт безопасного конкуррентного доступа и производительности не заморачиваемся, делаем максимально простую рабочую имплементацию. (Подсказка - можно читать и сохранять файл на каждую операцию).
// Важно аккуратно и правильно завернуть в IO все возможные побочные эффекты.
//
// функции которые пригодятся:
// - java.nio.file.Files.write
// - java.nio.file.Files.readString
// - java.nio.file.Files.exists
// - java.nio.file.Paths.get
final class FileWallet[F[_]: Sync](id: WalletId) extends Wallet[F] {
  //private val path = Paths.get(id)
  //private val read = Files.readString(path)

  //def balance: F[BigDecimal] =  Sync[F].delay(read).map(x => BigDecimal(x))

  /*меня на получение пути каждый раз снова, вместо первоначального получения*/
  def balance: F[BigDecimal] = for{
    path <- Sync[F].delay(Paths.get(id))
    read <- Sync[F].delay(Files.readAllLines(path))
    bal <- Sync[F].delay(read.get(0))
    dcml <- Sync[F].delay(BigDecimal(bal))
  } yield dcml

  //def topup(amount: BigDecimal): F[Unit] = Sync[F].delay(Files.write(path, balance.map(x => x + amount).toString.getBytes()))

  def topup(amount: BigDecimal): F[Unit] = for {
    dcml <- balance
    newBal <- Sync[F].delay(dcml + amount) //deffer?
    path <- Sync[F].delay(Paths.get(id))
    _ <- Sync[F].delay(Files.write(path, newBal.toString().getBytes()))
  } yield ()

  def withdraw(amount: BigDecimal): F[Either[WalletError, Unit]] = for {
    dcml <- balance
    newBal <- Sync[F].delay(dcml - amount)
    path <- Sync[F].delay(Paths.get(id))
    res <- /*Sync[F].delay(*/
      if (newBal >= 0) Sync[F].delay(Files.write(path, newBal.toString.getBytes())).as(Right())
      else Sync[F].delay(BalanceTooLow.asLeft)
  } yield(res)

}

object Wallet {

  // todo: реализовать конструктор
  // внимание на сигнатуру результата - инициализация кошелька имеет сайд-эффекты
  // Здесь нужно использовать обобщенную версию уже пройденного вами метода IO.delay,
  // вызывается она так: Sync[F].delay(...)
  // Тайпкласс Sync из cats-effect описывает возможность заворачивания сайд-эффектов
  def fileWallet[F[_]: Sync](id: WalletId): F[Wallet[F]] =   for {
    _ <- Sync[F].delay(Files.write(Paths.get(id), "0".getBytes()))
    fileWallet <- Sync[F].delay(new FileWallet(id))
  } yield fileWallet

  type WalletId = String

  sealed trait WalletError
  case object BalanceTooLow extends WalletError
}
