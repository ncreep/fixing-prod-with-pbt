package ncreep

import ncreep.bug1.Behavior
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Shrink.shrink
import org.scalacheck.{Arbitrary, Gen, Shrink}
import scala.annotation.nowarn

object Generators {
  implicit val jobProviderGen: Arbitrary[JobProvider] = Arbitrary {
    // bad generator, too random
    //    for {
    //      size <- Gen.choose(min = 5, max = 10)
    //      str <- Gen.stringOfN(size, Gen.alphaNumChar)
    //    } yield JobProvider(str)

    Gen.oneOf("p1", "p2", "p3").map(JobProvider.apply)
  }

  @nowarn("msg=deprecated")
  implicit val jobProviderShrink: Shrink[JobProvider] = Shrink { provider =>
    val shrinks = // shrinking towards p1 as the "smallest" provider
      if (provider.id == "p1") LazyList.empty
      else LazyList(JobProvider("p1"))

    shrinks.toStream
  }

  implicit val cpuGen: Arbitrary[CPU] = Arbitrary {
    Gen.choose(1, 1000).map(CPU.apply)
  }

  implicit val ramGen: Arbitrary[RAM] = Arbitrary {
    Gen.choose(1, 1000).map(RAM.apply)
  }

  implicit val shrinkCPU: Shrink[CPU] =
    Shrink.xmap[Int, CPU](CPU.apply, _.value)

  implicit val shrinkRAM: Shrink[RAM] =
    Shrink.xmap[Int, RAM](RAM.apply, _.value)

  implicit val jobGen: Arbitrary[Job] = Arbitrary {
    val cpuAndRAM = arbitrary[(CPU, RAM)]

    val crypto = cpuAndRAM.map((Job.MineCrypto.apply _).tupled)
    val deepLearning = cpuAndRAM.map((Job.TrainDeepLearning.apply _).tupled)
    val foldProteins = cpuAndRAM.map((Job.FoldProteins.apply _).tupled)

    Gen.oneOf(crypto, deepLearning, foldProteins)
  }

  @nowarn("msg=deprecated")
  implicit val jobShrink: Shrink[Job] = {
    import Job._

    Shrink {
      case MineCrypto(cpu, ram) => shrink((cpu, ram)).map((MineCrypto.apply _).tupled)
      case TrainDeepLearning(cpu, ram) => shrink((cpu, ram)).map((TrainDeepLearning.apply _).tupled)
      case FoldProteins(cpu, ram) => shrink((cpu, ram)).map((FoldProteins.apply _).tupled)
    }
  }

  implicit val jobBatchGen: Arbitrary[JobBatch] = Arbitrary {
    Gen.zip(arbitrary[JobProvider], arbitrary[List[Job]]).map((JobBatch.apply _).tupled)
  }

  implicit val jobBatchShrink: Shrink[JobBatch] =
    Shrink.xmap[(JobProvider, List[Job]), JobBatch](
      (JobBatch.apply _).tupled,
      batch => (batch.provider, batch.jobs))

  implicit val securityRatingGen: Arbitrary[SecurityRating] = Arbitrary {
    import SecurityRating._

    Gen.oneOf(
      Gen.const(Safe),
      Gen.const(Questionable),
      Gen.const(Shady),
      Gen.choose(0, 100).map(Unsafe.apply))
  }

  @nowarn("msg=deprecated")
  implicit val securityRatingShrink: Shrink[SecurityRating] = {
    import SecurityRating._

    Shrink {
      case Safe => Stream.empty
      case Questionable => Stream(Safe)
      case Shady => Stream(Questionable)
      case Unsafe(value) =>
        shrink(value).filter(v => v >= 0 && v <= 100).map(Unsafe.apply) :+ Shady
    }
  }

  implicit val jobWithSecurityRankingGen: Arbitrary[RankedJob] = Arbitrary {
    Gen.zip(arbitrary[Job], arbitrary[SecurityRating])
      .map((RankedJob.apply _).tupled)
  }

  implicit val jobWithSecurityRankingShrink: Shrink[RankedJob] =
    Shrink.xmap[(Job, SecurityRating), RankedJob](
      (RankedJob.apply _).tupled,
      j => (j.value, j.securityRating))

  implicit val securityRankerBehaviorGen: Arbitrary[Behavior] = Arbitrary {
    import ncreep.bug1.Behavior._

    Gen.oneOf(EmptyOption, EmptyList, AssignRandomRanking)
  }

  @nowarn("msg=deprecated")
  implicit val securityRankerBehaviorShrink: Shrink[Behavior] =
    Shrink { behavior =>
      import ncreep.bug1.Behavior._

      val shrinks = behavior match {
        case EmptyOption => LazyList.empty
        case EmptyList => LazyList(EmptyOption)
        case AssignRandomRanking => LazyList(EmptyList)
      }

      shrinks.toStream
    }

  implicit val batchOfBatchesGen: Arbitrary[BatchOfBatches] = Arbitrary {
    arbitrary[List[List[JobBatch]]].map(BatchOfBatches.apply)
  }

  implicit val batchOfBatchesShrink: Shrink[BatchOfBatches] =
    Shrink.xmap[List[List[JobBatch]], BatchOfBatches](
      BatchOfBatches.apply,
      batches => batches.values)

  implicit val rankedJobsGen: Arbitrary[RankedJobs] = Arbitrary {
    arbitrary[List[RankedJob]].map(RankedJobs.apply)
  }

  implicit val rankedJobsShrink: Shrink[RankedJobs] =
    Shrink.xmap[List[RankedJob], RankedJobs](
      RankedJobs.apply,
      batches => batches.values)

  implicit val batchOfBatchesAndBehaviorsGen: Arbitrary[BatchOfBatchesAndBehaviors] = Arbitrary {
    arbitrary[List[List[(JobBatch, Behavior)]]].map(BatchOfBatchesAndBehaviors.apply)
  }

  implicit val batchOfBatchesAndBehaviorsShrink: Shrink[BatchOfBatchesAndBehaviors] =
    Shrink.xmap[List[List[(JobBatch, Behavior)]], BatchOfBatchesAndBehaviors](
      BatchOfBatchesAndBehaviors.apply,
      batches => batches.values)

  implicit val securityRankerBehavior2Gen: Arbitrary[bug2.Behavior] = Arbitrary {
    import ncreep.bug2.Behavior._

    Gen.oneOf(Fail, EmptyList, AssignRandomRanking)
  }

  @nowarn("msg=deprecated")
  implicit val securityRankerBehavior2Shrink: Shrink[bug2.Behavior] =
    Shrink { behavior =>
      import bug2.Behavior._

      val shrinks = behavior match {
        case Fail => LazyList.empty
        case EmptyList => LazyList(Fail)
        case AssignRandomRanking => LazyList(EmptyList)
      }

      shrinks.toStream
    }

  implicit val batchOfBatchesAndBehaviors2Gen: Arbitrary[bug2.BatchOfBatchesAndBehaviors] = Arbitrary {
    arbitrary[List[List[(JobBatch, bug2.Behavior)]]].map(bug2.BatchOfBatchesAndBehaviors.apply)
  }

  implicit val batchOfBatchesAndBehaviors2Shrink: Shrink[bug2.BatchOfBatchesAndBehaviors] =
    Shrink.xmap[List[List[(JobBatch, bug2.Behavior)]], bug2.BatchOfBatchesAndBehaviors](
      bug2.BatchOfBatchesAndBehaviors.apply,
      batches => batches.values)
}
