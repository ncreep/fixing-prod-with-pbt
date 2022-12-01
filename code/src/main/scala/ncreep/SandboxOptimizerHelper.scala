package ncreep

import zio.{Task, UIO, ZIO}

case class PrePostJob[A](value: A, pre: Task[Unit], post: UIO[Unit], name: String)

class SandboxOptimizerHelper[JobBatch](sandboxManager: SecuritySandboxManager,
                                       getProvider: JobBatch => JobProvider) {
  def addPrePost(jobs: List[JobBatch]): List[PrePostJob[JobBatch]] = {
    val byProvider = jobs
      .zipWithIndex
      .groupBy { case (job, _) => getProvider(job) }

    val withPrePost = byProvider.values.flatMap {
      jobs =>
        val size = jobs.size
        val last = size - 1

        jobs
          .zipWithIndex
          .map {
            case ((job, origIndex), inProviderIndex) =>
              val withPrePost = if (size <= 1) openAndClose(job)
              else inProviderIndex match {
                case 0 => onlyOpen(job)
                case `last` => onlyClose(job)
                case _ => noPrePost(job)
              }

              origIndex -> withPrePost
          }
    }.toList.sortBy(_._1).map(_._2)

    withPrePost
  }

  private def onlyOpen(job: JobBatch) =
    PrePostJob(job, sandboxManager.open(getProvider(job)), ZIO.unit, "onlyOpen")

  private def onlyClose(job: JobBatch) =
    PrePostJob(job, ZIO.unit, sandboxManager.close(getProvider(job)).orDie, "onlyClose")

  private def openAndClose(job: JobBatch) =
    PrePostJob(
      job,
      sandboxManager.open(getProvider(job)),
      sandboxManager.close(getProvider(job)).orDie, "openAndClose")

  private def noPrePost(job: JobBatch) =
    PrePostJob(job, ZIO.unit, ZIO.unit, "noPrePost")
}
