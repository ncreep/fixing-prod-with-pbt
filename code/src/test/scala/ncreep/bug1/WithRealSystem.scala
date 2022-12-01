package ncreep.bug1

import ncreep._
import zio.ZIO._

trait WithRealSystem {
  self: ZIOSpec =>

  def submitBatches(batches: List[List[JobBatch]]) = {
    for {
      (securityManager, sandboxManager) <- createSecurityManager
      _ <- foreachDiscard(batches)(securityManager.secureRunJobs(_).exit)
      isSandboxCorrupt <- sandboxManager.isCorrupt
    } yield isSandboxCorrupt
  }.unsafeRun()

  def submitBatches(batches: List[JobBatch])(implicit dummyImplicit: DummyImplicit) = {
    for {
      (securityManager, sandboxManager) <- createSecurityManager
      _ <- securityManager.secureRunJobs(batches).exit
      isSandboxCorrupt <- sandboxManager.isCorrupt
    } yield isSandboxCorrupt
  }.unsafeRun()

  def createSecurityManager =
    SecuritySandboxManager.start.map { sandboxManager =>
      val securityManager = new SecurityManager(
        new SecurityRanker.Default,
        new SecuritySandboxOptimizer(sandboxManager),
        new SecurityTracer(sandboxManager),
        new JobQueue)

      (securityManager, sandboxManager)
    }
}
