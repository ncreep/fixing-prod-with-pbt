package ncreep

import ncreep.Generators._
import ncreep.bug1.Behavior
import org.scalacheck.{Prop, Test}
import scala.collection.mutable
import scala.language.implicitConversions

// Doing gymnastics here because scalacheck's 'Prop.collect'
// aggregates data per input, and since we have multiple values per input
// we want to count, this doesn't work for us
object DataCollection extends App {
  private def count[A](map: mutable.Map[A, Int])(value: A): Unit = {
    val curr = map.getOrElse(value, 0)
    map.put(value, curr + 1)
  }

  private def printSummary[A](name: String,
                              map: mutable.Map[A, Int]): Unit = {
    val total = map.values.sum

    println("----------------")
    println(s"Summary of $name")
    map.foreach { case (k, v) =>
      val p = 100 * (v.toDouble / total)
      println(f"$p%.3f%% $k%s")
    }
    println("----------------")
  }

  private def behaviors(actions: BatchOfBatchesAndBehaviors): List[Behavior] =
    actions.values.flatMap(_.map(_._2))

  private def providers(actions: BatchOfBatchesAndBehaviors): List[String] =
    actions.values.flatMap(_.map(_._1.provider.id))

  private def jobTypes(actions: BatchOfBatchesAndBehaviors): List[String] =
    actions.values.flatMap(_.flatMap(_._1.jobs).map(_.productPrefix))

  // a poor substitute for a frequency map, not thread-safe...
  val bs = mutable.Map.empty[Behavior, Int]
  val ps = mutable.Map.empty[String, Int]
  val js = mutable.Map.empty[String, Int]

  val prop = Prop.forAll { actions: BatchOfBatchesAndBehaviors =>
    behaviors(actions).foreach(count(bs))
    providers(actions).foreach(count(ps))
    jobTypes(actions).foreach(count(js))

    Prop.passed
  }

  // manually running for the side-effect
  locally {
    val _ = Test.check(
      Test.Parameters.default.withMinSuccessfulTests(10), prop)
  }

  printSummary("behaviors", bs)
  printSummary("job types", js)
  printSummary("job provides", ps)
}
