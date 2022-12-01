package ncreep.bug2

import ncreep.Generators._
import ncreep.{JobQueue, SecuritySandboxManager, SecurityTracer, ZIOSpec}
import zio.ZIO._
import scala.language.implicitConversions

class SecurityManagerMockTest extends ZIOSpec {
  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    PropertyCheckConfiguration(minSuccessful = 2000)

  "The security manager" should {
    "not corrupt the sandbox" in {
      forAll { actions: BatchOfBatchesAndBehaviors =>
        val isSandboxCorrupt = submitBatches(actions)

        assert(!isSandboxCorrupt)
      }
    }
  }

  def submitBatches(actions: BatchOfBatchesAndBehaviors) = {
    for {
      (jobManager, securityRanker, sandboxManager) <- createSecurityManager
      _ <- foreachDiscard(actions.values) {
        jobsAndBehaviors =>
          val (batches, behaviors) = jobsAndBehaviors.unzip

          for {
            // note that because the SecurityRanker can fail in this version
            // the behaviors might not correspond 1-1 with the batches
            // as a failing batch will cancel all others in its group, but the behaviors
            // will remain
            _ <- securityRanker.addBehaviors(behaviors)
            _ <- jobManager.secureRunJobs(batches).exit
          } yield ()
      }
      isSandboxCorrupt <- sandboxManager.isCorrupt
    } yield isSandboxCorrupt
  }.unsafeRun()

  private val createSecurityManager = for {
    securityRanker <- SecurityRankerMock.init
    sandboxManager <- SecuritySandboxManager.start
    jobManager = new SecurityManager(
      securityRanker,
      new SecuritySandboxOptimizer(sandboxManager),
      new SecurityTracer(sandboxManager),
      new JobQueue)
  } yield (jobManager, securityRanker, sandboxManager)
}


