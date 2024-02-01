public class Run {
    public static void main(String[] args) throws Exception {
        final var stopwatch = StopWatch.start();

        Thread.sleep(1000);
        
        final var stopped = stopwatch.stop();

        System.out.println(stopped.elapsed());
    }
}