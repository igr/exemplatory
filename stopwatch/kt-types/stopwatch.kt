class StartedStopwatch() {
    internal val startTime = System.currentTimeMillis()
}

data class StoppedStopwatch(
    val startedStopwatch: StartedStopwatch,
) {
    private val stopTime = System.currentTimeMillis()
    val elapsed: Long = stopTime - startedStopwatch.startTime
}

fun main() {
    val startedStopwatch = StartedStopwatch()
    Thread.sleep(1000)
    val stoppedStopwatch = StoppedStopwatch(startedStopwatch)
    println(stoppedStopwatch.elapsed)
}