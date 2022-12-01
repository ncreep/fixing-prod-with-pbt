package ncreep.bug2

import ncreep.JobBatch

// This mostly exists for the nicer `toString`

case class BatchOfBatchesAndBehaviors(values: List[List[(JobBatch, Behavior)]]) {
  override def toString: String =
    pprint.apply(this, height = Int.MaxValue, showFieldNames = false).plainText
}
