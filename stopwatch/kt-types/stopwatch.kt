sealed class Stopwatch {

    class Started() : Stopwatch() {
        internal val startTime = System.currentTimeMillis()
    }
    data class Stopped(
        val startedStopwatch: Started,
    ) : Stopwatch() {
        private val stopTime = System.currentTimeMillis()
        val elapsed: Long = stopTime - startedStopwatch.startTime
    }
}

fun main() {
    val startedStopwatch = Stopwatch.Started()
    Thread.sleep(1000)
    val stoppedStopwatch = Stopwatch.Stopped(startedStopwatch)
    println(stoppedStopwatch.elapsed)
}
