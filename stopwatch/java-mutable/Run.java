public class Run {
    public static void main(String[] args) throws Exception {
        final var stopWatch = new StopWatch();

        stopWatch.start();

        Thread.sleep(1000);
        
        stopWatch.stop();

        System.out.println(stopWatch.elapsed());
    }
}