package ncreep

sealed trait Job extends Product with Serializable {
  def cpu: CPU

  def ram: RAM
}

object Job {
  case class MineCrypto(cpu: CPU, ram: RAM) extends Job
  case class TrainDeepLearning(cpu: CPU, ram: RAM) extends Job
  case class FoldProteins(cpu: CPU, ram: RAM) extends Job
}

case class CPU(value: Int)
case class RAM(value: Int)

case class JobProvider(id: String)

case class JobBatch(provider: JobProvider,
                    jobs: List[Job])

sealed trait SecurityRating extends Product with Serializable {

  import SecurityRating._

  def ordinal: Int = this match {
    case Safe => 0
    case Questionable => 1
    case Shady => 2
    case Unsafe(value) => 3 + value // a bug waiting to happen...
  }
}

object SecurityRating {
  case object Safe extends SecurityRating
  case object Questionable extends SecurityRating
  case object Shady extends SecurityRating
  // grade should always be positive
  // good luck avoiding bugs...
  case class Unsafe(grade: Int) extends SecurityRating

  implicit val ordering: Ordering[SecurityRating] = Ordering.by(_.ordinal)

  def fromOrdinal(ordinal: Int): SecurityRating =
    ordinal match {
      case 0 => Safe
      case 1 => Questionable
      case 2 => Shady
      case _ => Unsafe(ordinal - 3)
    }
}

case class RankedJob(value: Job,
                     securityRating: SecurityRating)

object RankedJob {

  import Job._

  /** Order by security rating.
    * On equality, prefer protein folding too all other job types.
    * We want to give back!
    */
  implicit val ordering: Ordering[RankedJob] =
    Ordering
      .by((_: RankedJob).securityRating)
      .orElse { (job1, job2) =>
        (job1.value, job2.value) match {
          // Uncomment to fix the ordering bug
          // case (_: FoldProteins, _: FoldProteins) => 0
          case (_: FoldProteins, _) => 1
          case (_, _: FoldProteins) => -1
          case _ => 0
        }
      }
}

case class RankedBatch(provider: JobProvider,
                       jobs: Option[List[RankedJob]])

case object SecuritySandboxIsCorrupt extends Exception("Security sandbox is corrupt")

case class SandboxIsNotOpen(provider: JobProvider)
  extends Exception(s"Sandbox is not open for [${provider.id}]")

