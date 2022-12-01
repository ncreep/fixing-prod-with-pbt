package ncreep.bug1

import ncreep.Job._
import ncreep._

class SecurityManagerUnitTest extends WithRealSystem with ZIOSpec {
  "Examples from production" should {
    "reproduce the bug 1" in {
      val batches =
        List(
          JobBatch(
            JobProvider("p1"),
            jobs = List(
              MineCrypto(CPU(625), RAM(666)),
              FoldProteins(CPU(511), RAM(591)))),
          JobBatch(
            JobProvider("p2"),
            jobs = List(
              MineCrypto(CPU(686), RAM(390)),
              TrainDeepLearning(CPU(359), RAM(37)))))

      val isSandboxCorrupt = submitBatches(batches)

      assert(isSandboxCorrupt)
    }

    "reproduce the bug 2" in {
      val batches =
        List(
          List(
            JobBatch(
              JobProvider("p1"),
              List(
                FoldProteins(CPU(753), RAM(903)), TrainDeepLearning(CPU(651), RAM(332)), MineCrypto(CPU(772), RAM(346))
              )
            ),
            JobBatch(
              JobProvider("p1"),
              List(
                FoldProteins(CPU(218), RAM(575)), MineCrypto(CPU(368), RAM(869)), FoldProteins(CPU(634), RAM(592)),
                FoldProteins(CPU(145), RAM(642)), MineCrypto(CPU(437), RAM(144))
              )
            ),
            JobBatch(JobProvider("p3"),
              List(FoldProteins(CPU(774), RAM(387))))),
          List(
            JobBatch(
              JobProvider("p2"),
              List(
                MineCrypto(CPU(686), RAM(390)), TrainDeepLearning(CPU(359), RAM(37)), FoldProteins(CPU(298), RAM(880))
              )
            ),
            JobBatch(
              JobProvider("p2"),
              List(
                TrainDeepLearning(CPU(253), RAM(351)), TrainDeepLearning(CPU(244), RAM(669)), TrainDeepLearning(CPU(113), RAM(74)),
                FoldProteins(CPU(973), RAM(464)),
              )
            ),
            JobBatch(JobProvider("p3"), List(MineCrypto(CPU(894), RAM(127)))),
            JobBatch(
              JobProvider("p2"),
              List(
                FoldProteins(CPU(688), RAM(339)), MineCrypto(CPU(445), RAM(840)), FoldProteins(CPU(799), RAM(481)),
                MineCrypto(CPU(49), RAM(301)), FoldProteins(CPU(899), RAM(124)), MineCrypto(CPU(882), RAM(541)),
                TrainDeepLearning(CPU(579), RAM(721))
              )
            ),
            JobBatch(
              JobProvider("p2"),
              List(
                MineCrypto(CPU(845), RAM(367)), TrainDeepLearning(CPU(232), RAM(275)), MineCrypto(CPU(639), RAM(919)),
                TrainDeepLearning(CPU(135), RAM(337))))))

      val isSandboxCorrupt = submitBatches(batches)

      assert(isSandboxCorrupt)
    }
  }
}
