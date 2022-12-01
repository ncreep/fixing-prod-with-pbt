package ncreep.bug1

import ncreep._
import zio.Task
import zio.ZIO._

class SecurityManager(securityRanker: SecurityRanker,
                      sandboxOptimizer: SecuritySandboxOptimizer,
                      securityTracer: SecurityTracer,
                      jobQueue: JobQueue) {

  def secureRunJobs(jobs: List[JobBatch]): Task[Unit] =
    for {
      //      _ <- debug("Ranking batches")
      rankedBatches <-
        foreach(jobs)(securityRanker.predictSecurityLevel)
          .map(sandboxOptimizer.addPrePost)
      _ <-
        foreachDiscard(rankedBatches) { rankedBatch =>
          foreachDiscard(rankedBatch.value.jobs) { jobs =>
            acquireReleaseWith(rankedBatch.pre)(_ => rankedBatch.post) { _ =>
              //              debug(s"Tracing batch - ${rankedBatch.value.provider.id}") *>
              securityTracer
                .rejectUnsafe(
                  rankedBatch.value.provider,
                  callback = jobQueue.submit)(jobs)

            }
          }
        }
    } yield ()
}
