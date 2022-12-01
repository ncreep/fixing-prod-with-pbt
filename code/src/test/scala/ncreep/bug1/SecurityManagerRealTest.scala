package ncreep.bug1

import ncreep.Generators._
import ncreep._

class SecurityManagerRealTest extends WithRealSystem with ZIOSpec {
  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    PropertyCheckConfiguration(minSuccessful = 2000)

  "The security manager" should {
    "not corrupt the sandbox" in {
      forAll { batches: BatchOfBatches =>
        val isSandboxCorrupt = submitBatches(batches.values)

        assert(!isSandboxCorrupt)
      }
    }
  }
}
