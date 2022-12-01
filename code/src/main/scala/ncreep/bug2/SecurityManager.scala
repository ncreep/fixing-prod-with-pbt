package ncreep.bug2

import ncreep.{JobBatch, JobQueue, SecurityTracer}
import zio.Task
import zio.ZIO._

class SecurityManager(securityRanker: SecurityRanker,
                      sandboxOptimizer: SecuritySandboxOptimizer,
                      securityTracer: SecurityTracer,
                      jobQueue: JobQueue) {

  def secureRunJobs(jobs: List[JobBatch]): Task[Unit] =
    for {
      rankedBatches <-
        foreach(jobs)(securityRanker.predictSecurityLevel)
          .map(sandboxOptimizer.addPrePost)
      _ <-
        foreachDiscard(rankedBatches) { rankedBatch =>
          acquireReleaseWith(rankedBatch.pre)(_ => rankedBatch.post) { _ =>
            val jobs = rankedBatch.value.values

            securityTracer
              .rejectUnsafe(
                rankedBatch.value.provider,
                callback = jobQueue.submit)(jobs)
          }
        }
    } yield ()
}
