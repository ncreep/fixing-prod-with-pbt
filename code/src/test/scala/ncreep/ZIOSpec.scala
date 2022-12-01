package ncreep

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpecLike
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import zio.Unsafe.unsafe
import zio.{Exit, Task}

trait ZIOSpec extends AnyWordSpecLike with ScalaCheckDrivenPropertyChecks with Matchers {
  implicit class UnsafeRun[A](task: Task[A]) {
    def unsafeRun(): A = unsafe { implicit u =>
      zio.Runtime.default.unsafe.run(task) match {
        case Exit.Success(value) => value
        case Exit.Failure(cause) => fail(cause.prettyPrint)
      }
    }
  }
}
