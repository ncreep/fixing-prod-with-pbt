package ncreep.bug2

import ncreep.{JobBatch, RankedJob, RankedBatch => _, _}
import zio.ZIO._
import zio._
import scala.Ordering.Implicits._

case class RankedBatch(provider: JobProvider,
                       values: List[RankedJob])

trait SecurityRanker {
  def predictSecurityLevel(jobBatch: JobBatch): Task[RankedBatch]
}

object SecurityRanker {
  class Default {
    def predictSecurityLevel(jobBatch: JobBatch): Task[RankedBatch] =
      attemptBlocking {
        val ranking = rankLegacy(jobBatch.jobs)

        RankedBatch(jobBatch.provider, ranking)
      }

    private def rankLegacy(jobBatch: List[Job]): List[RankedJob] =
      jobBatch
        .map(assignSecurityRating)
        .filterNot(_.securityRating >= SecurityRating.Shady)
        .sorted

    private def assignSecurityRating(job: Job): RankedJob = {
      val ordinal = util.Random.nextInt(100)
      val rating = SecurityRating.fromOrdinal(ordinal)

      RankedJob(job, rating)
    }
  }
}
