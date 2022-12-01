package ncreep.bug1

import ncreep._
import ncreep.bug1.Behavior._
import ncreep.bug1.SecurityRankerMock._
import zio.ZIO._
import zio._
import scala.util.control.NoStackTrace

class SecurityRankerMock(behaviors: zio.Queue[Behavior]) extends SecurityRanker {
  def predictSecurityLevel(jobBatch: JobBatch): UIO[RankedBatch] =
    withNextBehavior {
      case EmptyOption => succeed(None)
      case EmptyList => succeed(Some(List.empty))
      case AssignRandomRanking => assignRandomRankings(jobBatch)
    }.map(RankedBatch(jobBatch.provider, _))

  private def randomRating = Random.nextIntBetween(0, 100).map(SecurityRating.fromOrdinal)

  private def assignRandomRankings(jobBatch: JobBatch) =
    foreach(jobBatch.jobs) { job =>
      randomRating.map(RankedJob(job, _))
    }.map(Some.apply)

  def addBehaviors(newBehaviors: List[Behavior]): UIO[Unit] =
    behaviors.offerAll(newBehaviors).unit

  private def withNextBehavior[A](f: Behavior => UIO[A]): UIO[A] =
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
  case object EmptyOption extends Behavior
  case object EmptyList extends Behavior
  case object AssignRandomRanking extends Behavior
}
