package info.cemu.download.util

import Types._

case class ProgressBar(max: Long) {

  var done = false
  var current: Long = 0
  var lastRatingTimestamp: Long = 0
  var lastRatingValue: Long = 0
  var lastRating: Long = 0

  var startTimestamp: Long = 0
  var estimatedFinish: Long = 0

  def set(value: Long): Unit = {

    current = Math.min(value, max)
    val currentTimestamp = System.currentTimeMillis()

    if (startTimestamp == 0)
      startTimestamp = currentTimestamp

    val progress = ((current * 100.0) / max)

    val passed = currentTimestamp - startTimestamp

    if ((current < max) && ((lastRatingTimestamp + 2000) < currentTimestamp)) {

      if (passed > 0)
        estimatedFinish = ((max.toDouble * passed.toDouble) / current.toDouble).toLong

      val timeDifference = currentTimestamp - lastRatingTimestamp
      val valueDifference = current - lastRatingValue
      lastRating = (valueDifference.toDouble * 1000.0 / timeDifference.toDouble).toLong
      lastRatingTimestamp = currentTimestamp
      lastRatingValue = current

    }

    set(progress, current, max, lastRating,
      if (passed > estimatedFinish) estimatedFinish else passed, estimatedFinish)
  }

  def add(chunk: Long): Unit = {
    current += chunk
    set(current)
  }

  protected def set(value: Double, current: Long, max: Long, rate: Long, passed: Long, estimatedFinish: Long, size : Int = 10): Unit = {
    if (!done) {
      val scale = size / 10.0
      val progress = (value * scale / 10.0).toInt
      val status = value.toInt
      print("% 4d%% [%s%s] %s of %s at %s/s [%s<%s]       ".format(status, "#" * progress,
        " " * ((scale * 10.0).toInt - progress), current.toDisplaySize(), max.toDisplaySize(),
        rate.toDisplaySize(), passed.toDisplayTime(), estimatedFinish.toDisplayTime()))
      if (status == 100) {
        done = true
        println()
      } else
        print("\r")
    }
  }

}
