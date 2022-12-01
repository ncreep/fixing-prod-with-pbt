package ncreep

import ncreep.bug1.Behavior

// This mostly exists for the nicer `toString`

case class BatchOfBatchesAndBehaviors(values: List[List[(JobBatch, Behavior)]]) {
  override def toString: String =
    pprint.apply(this, height = Int.MaxValue, showFieldNames = false).plainText
}

case class BatchOfBatches(values: List[List[JobBatch]]) {
  override def toString: String =
    pprint.apply(this, height = Int.MaxValue, showFieldNames = false).plainText
}

case class RankedJobs(values: List[RankedJob]) {
  override def toString: String =
    pprint.apply(this, height = Int.MaxValue, showFieldNames = false).plainText
}
