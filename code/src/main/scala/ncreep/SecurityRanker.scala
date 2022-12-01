package ncreep

import zio.UIO
import zio.ZIO._
import scala.Ordering.Implicits._
import scala.util.Try

trait SecurityRanker {
  def predictSecurityLevel(jobBatch: JobBatch): UIO[RankedBatch]
}

object SecurityRanker {

  class Default extends SecurityRanker {
    def predictSecurityLevel(jobBatch: JobBatch): UIO[RankedBatch] =
      succeedBlocking {
        val ranking = Try(rankLegacy(jobBatch.jobs)).toOption

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
