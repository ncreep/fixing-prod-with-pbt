package ncreep

import ncreep.SecurityRating._
import zio.ZIO._
import zio.{Unsafe => _, _}

class SecurityTracer(sandboxManager: SecuritySandboxManager) {
  def rejectUnsafe(provider: JobProvider,
                   callback: Job => UIO[Unit])
                  (jobs: List[RankedJob]): Task[Unit] =
    ifZIO(sandboxManager.isProviderOpen(provider))(
      onTrue = foreachDiscard(jobs)(securityTrace(callback)),
      onFalse = fail(SandboxIsNotOpen(provider)))

  private def securityTrace(callback: Job => UIO[Unit])
                           (job: RankedJob) = {
    val maybeJob = job.securityRating match {
      case Safe => succeed(Some(job.value))
      case Questionable | Shady | _: Unsafe => traceUnsafeJob(job.value)
    }

    for {
      job <- maybeJob
      _ <- foreachDiscard(job)(callback)
    } yield ()
  }

  private def traceUnsafeJob(job: Job) =
    Random.nextBoolean.map { isSafe =>
      if (isSafe) Some(job)
      else None
    }
}


