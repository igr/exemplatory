public class StopWatch {

    private long start;
    private long end;
    
    public void start() {
        start = System.currentTimeMillis();
    }
    
    public void stop() {
        end = System.currentTimeMillis();
    }
    
    public long elapsed() {
        return end - start;
    }
}