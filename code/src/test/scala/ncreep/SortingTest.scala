package ncreep

import ncreep.Generators._
import RankedJob.ordering.compare
import math.signum
import scala.annotation.nowarn
import Ordering.Implicits._

@nowarn("msg=match may not be exhaustive")
class SortingTest extends ZIOSpec {

  "Sorting" should {
    implicit val generatorDrivenConfig: PropertyCheckConfiguration =
      PropertyCheckConfiguration(minSuccessful = 20000)

    "not throw exceptions" in {
      forAll { jobs: RankedJobs =>
        noException should be thrownBy {
          jobs.values.sorted
        }
      }
    }
  }

  "The ranked job ordering" should {
    implicit val generatorDrivenConfig: PropertyCheckConfiguration =
      PropertyCheckConfiguration(minSuccessful = 1000, maxDiscardedFactor = 35)

    "respect inversion" in {
      forAll { (x: RankedJob, y: RankedJob) =>
        assert {
          signum(compare(x, y)) == -signum(compare(y, x))
        }
      }
    }

    "be transitive" in {
      forAll { (x: RankedJob, y: RankedJob, z: RankedJob) =>
        List(x, y, z).permutations.foreach {
          case List(x, y, z) =>
            if (x > y && y > z) assert(x > z)
        }
      }
    }

    "preserve sign on equality" in {
      forAll { (x: RankedJob, y: RankedJob, z: RankedJob) =>
        whenever(compare(x, y) == 0) {
          assert {
            signum(compare(x, z)) == signum(compare(y, z))
          }
        }
      }
    }
  }
}
