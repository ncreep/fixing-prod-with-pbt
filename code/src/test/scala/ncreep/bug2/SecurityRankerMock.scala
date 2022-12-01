package ncreep.bug2

import ncreep.bug2.Behavior._
import ncreep.bug2.SecurityRankerMock._
import ncreep.{JobBatch, RankedJob, SecurityRating}
import zio.ZIO._
import zio._
import scala.util.control.NoStackTrace

class SecurityRankerMock(behaviors: zio.Queue[Behavior]) extends SecurityRanker {
  def predictSecurityLevel(jobBatch: JobBatch): Task[RankedBatch] =
    withNextBehavior {
      case Fail => ZIO.fail(FailedBatch)
      case EmptyList => succeed(List.empty)
      case AssignRandomRanking => assignRandomRankings(jobBatch)
    }.map(RankedBatch(jobBatch.provider, _))

  private def randomRating = Random.nextIntBetween(0, 100).map(SecurityRating.fromOrdinal)

  private def assignRandomRankings(jobBatch: JobBatch) =
    foreach(jobBatch.jobs) { job =>
      randomRating.map(RankedJob(job, _))
    }

  def addBehaviors(newBehaviors: List[Behavior]): UIO[Unit] =
    behaviors.offerAll(newBehaviors).unit

  private def withNextBehavior[A](f: Behavior => Task[A]): Task[A] =
    behaviors.poll
      .someOrFail(emptyQueue).orDie
      .flatMap(f)
}

object SecurityRankerMock {
  private val emptyQueue: Exception =
    new Exception("Security provider mock was not yet initialized") with NoStackTrace {}

  val init: UIO[SecurityRankerMock] =
    Queue.unbounded[Behavior].map(new SecurityRankerMock(_))
}

sealed trait Behavior extends Product with Serializable

object Behavior {
  case object Fail extends Behavior
  case object EmptyList extends Behavior
  case object AssignRandomRanking extends Behavior
}

object FailedBatch extends Exception("Failed batch")
