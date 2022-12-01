package ncreep

import zio.{UIO, ZIO}

class JobQueue {
  def submit(job: Job): UIO[Unit] = run(job)

  private def run(job: Job) = {
    ZIO.unit
    // uncomment to see how things are running
    //    val description = job match {
    //      case Job.MineCrypto(cpu, ram) => s"mining crypto: [$cpu, $ram]"
    //      case Job.TrainDeepLearning(cpu, ram) => s"training deep learning: [$cpu, $ram]"
    //      case Job.FoldProteins(cpu, ram) => s"folding proteins: [$cpu, $ram]"
    //    }
    //
    //    ZIO.debug(description)
  }
}
