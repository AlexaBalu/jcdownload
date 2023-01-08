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

  var chunksCount: Long = 0
  var currentChunk: Long = 0
  var filesCount: Long = 0
  var currentFile: Long = 0

  var resetEstimation = false
  var estimationStart: Long = 0
  var currentAlreadyDownloaded: Long = 0
  var failures : Int = 0

  def get(): Long = current

  def set(value: Long): Unit = {

    current = Math.min(value, max)
    val currentTimestamp = System.currentTimeMillis()

    if (startTimestamp == 0)
      startTimestamp = currentTimestamp

    if (estimationStart == 0)
      estimationStart = currentTimestamp

    val progress = ((current * 100.0) / max)

    val passed = currentTimestamp - startTimestamp

    if ((current < max) && ((lastRatingTimestamp + 2000) < currentTimestamp)) {

      val estimationPassed = currentTimestamp - estimationStart

      if (estimationPassed > 0)
        estimatedFinish = (estimationStart - startTimestamp) + (((max - currentAlreadyDownloaded).toDouble * estimationPassed.toDouble) / (current - currentAlreadyDownloaded).toDouble).toLong

      val timeDifference = currentTimestamp - lastRatingTimestamp
      val valueDifference = current - lastRatingValue
      lastRating = Math.max(0, (valueDifference.toDouble * 1000.0 / timeDifference.toDouble).toLong)
      lastRatingTimestamp = currentTimestamp
      lastRatingValue = current

    }

    set(progress, current, max, lastRating,
      passed, if (passed > estimatedFinish) passed else estimatedFinish)
  }

  def add(chunk: Long, alreadyDownloaded: Boolean = false): Unit = {
    current += chunk
    if (!alreadyDownloaded) {
      if (resetEstimation) {
        currentAlreadyDownloaded = current
        estimationStart = 0
        resetEstimation = false
      }
    } else {
      resetEstimation = true
    }
    set(current)
  }

  def setChunksCount(count: Long): Unit = {
    chunksCount = count
    set(current)
  }

  def setCurrentChunk(chunk: Long): Unit = {
    currentChunk = chunk
    set(current)
  }

  def resetFilesCount() : Unit = {
    filesCount = 0
    currentFile = 0
    set(current)
  }

  def setFilesCount(count: Long): Unit = {
    filesCount = count
    set(current)
  }

  def setCurrentFile(file: Long): Unit = {
    currentFile = file
    set(current)
  }

  def markFailure() : Unit = {
    failures += 1
    if (failures > 5)
      throw new RuntimeException("Too many failures, giving up! Try another time")
  }

  protected def set(value: Double, current: Long, max: Long, rate: Long, passed: Long, estimatedFinish: Long, size: Int = 8): Unit = {
    if (!done) {
      val scale = size / 10.0
      val progress = (value * scale / 10.0).toInt
      val status = value.toInt
      print(" %d/%d% 4d%% %s%s %s/%s at %s/s [%s<%s] %s%s\r".format(currentChunk, chunksCount, status, "#" * progress,
        " " * ((scale * 10.0).toInt - progress), current.toDisplaySize(), max.toDisplaySize(),
        rate.toDisplaySize(), passed.toDisplayTime(), estimatedFinish.toDisplayTime(), if (filesCount > 0) "%d/%d ".format(currentFile, filesCount) else " " * 9, if (failures != 0) "*" else ""))
      if (((filesCount == 0 || currentFile == filesCount) && (currentChunk == chunksCount) && (chunksCount > 0))) {
        done = true
        print("\n")
      }
    }
  }

}
