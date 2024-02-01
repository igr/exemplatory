public class StopWatch {
    public static Started start() {
        return new Started();
    }
    public static class Started {
        private final long value = System.currentTimeMillis();
        public Stopped stop() {
            return new Stopped(this);
        }
    }
    public static class Stopped {
        private final long elapsed;
        public Stopped(StopWatch.Started started) {
            this.elapsed = System.currentTimeMillis() - started.value;
        }
        public long elapsed() {
            return elapsed;
        }
    }
}