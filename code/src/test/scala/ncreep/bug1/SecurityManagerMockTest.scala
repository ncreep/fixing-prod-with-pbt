package ncreep.bug1

import ncreep.Generators._
import ncreep._
import ncreep.bug1.Behavior._
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

  "Minimal reproduction" should {
    "not corrupt the sandbox" in {
      val actions = BatchOfBatchesAndBehaviors(
        List(
          List(
            (JobBatch(JobProvider("p3"), List()), EmptyList),
            (JobBatch(JobProvider("p3"), List()), EmptyOption)
          ),
          List((JobBatch(JobProvider("p3"), List()), EmptyList))))

      val isSandboxCorrupt = submitBatches(actions)

      assert(!isSandboxCorrupt)
    }
  }

  "Example of a valid run" should {
    "not corrupt the sandbox" in {
      val actions = BatchOfBatchesAndBehaviors(
        List(
          List(
            (JobBatch(JobProvider("p1"), List()), AssignRandomRanking),
            (JobBatch(JobProvider("p2"), List()), AssignRandomRanking),
            (JobBatch(JobProvider("p1"), List()), AssignRandomRanking),
          ),
          List(
            (JobBatch(JobProvider("p3"), List()), AssignRandomRanking),
            (JobBatch(JobProvider("p1"), List()), AssignRandomRanking),
            (JobBatch(JobProvider("p3"), List()), AssignRandomRanking),
          )))

      val isSandboxCorrupt = submitBatches(actions)

      assert(!isSandboxCorrupt)
    }
  }


  def submitBatches(actions: BatchOfBatchesAndBehaviors) = {
    for {
      (jobManager, securityRanker, sandboxManager) <- createSecurityManager
      _ <- foreachDiscard(actions.values) {
        jobsAndBehaviors =>
          val (batches, behaviors) = jobsAndBehaviors.unzip

          for {
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


