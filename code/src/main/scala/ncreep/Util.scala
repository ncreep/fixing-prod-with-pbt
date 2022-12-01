package ncreep

import zio.{UIO, ZIO}

object Util {
  def log(value: Any): Unit =
    pprint.pprintln(
      value,
      height = Int.MaxValue,
      width = 40,
      showFieldNames = false)

  def logZIO(value: Any): UIO[Unit] = ZIO.succeed(log(value))
}
