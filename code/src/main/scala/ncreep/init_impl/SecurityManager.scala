package ncreep.init_impl

import ncreep._
import zio.Task
import zio.ZIO._

class SecurityManager(securityRanker: SecurityRanker,
                      sandboxManager: SecuritySandboxManager,
                      securityTracer: SecurityTracer,
                      jobQueue: JobQueue) {
  def secureRunJobs(jobs: List[JobBatch]): Task[Unit] =
    for {
      rankedBatches <- foreach(jobs)(securityRanker.predictSecurityLevel)
      _ <- foreachDiscard(rankedBatches) { rankedBatch =>
        foreachDiscard(rankedBatch.jobs) { jobs =>
          val provider = rankedBatch.provider

          acquireReleaseWith(open(provider))(_ => close(provider)) { _ =>
            securityTracer
              .rejectUnsafe(
                rankedBatch.provider,
                callback = jobQueue.submit)(jobs)
          }
        }
      }
    }
    yield ()

  private def open(provider: JobProvider) =
    sandboxManager.open(provider)

  private def close(provider: JobProvider) =
    sandboxManager.close(provider).orDie
}
