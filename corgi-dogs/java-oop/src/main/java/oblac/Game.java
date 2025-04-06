package oblac;

import java.util.List;

public class Game implements GameChannel {

    public static void main(final String[] args) {
        new Game().run();
    }

    // state
    private boolean running = false;

    public void run() {
        final CordiDogMoveListener printer =
            (corgiName, position) -> System.out.println(corgiName.value() + ": [" + position.x() + "," + position.y() + "]");

        final var garden = new Garden(this);

        final List<CorgiDogChannel> corgis = List.of(
            new CorgiDog(garden, new CorgiName("Coco"), printer),
            new CorgiDog(garden, new CorgiName("Boopie"), printer)
        );

        this.running = true;
        while (running) {
            corgis.forEach(CorgiDogChannel::move);
        }

        System.out.println("Only one corgi dog in garden left");
    }

    @Override
    public void stopGame() {
        running = false;
    }
}
