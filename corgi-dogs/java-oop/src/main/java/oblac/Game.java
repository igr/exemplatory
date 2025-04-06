package oblac;

import java.util.List;

public class Game implements Game4Garden {

    public static void main(final String[] args) {
        final var game = new Game();
        game.placeCorgis();
        game.run();
    }

    record CorgiDogAndRef(CorgiDogRef ref, CorgiDog corgiDog) {}

    // state
    private final Garden garden;
    private final List<CorgiDogAndRef> corgis;
    private boolean running = false;

    Game() {
        garden = new Garden(this);

        corgis = List.of(
            createCorgi("Mile"),
            createCorgi("Žile")
        );
    }

    public void placeCorgis() {
        corgis.forEach(it -> garden.placeCorgiDog(it.ref));
    }

    private CorgiDogAndRef createCorgi(final String name) {
        final var ref = CorgiDogRef.create();
        final var dog = new CorgiDog(ref, new CorgiName(name), garden);
        return new CorgiDogAndRef(ref, dog);
    }

    public void run() {
        this.running = true;
        while (running) {
            corgis.stream().map(it -> it.corgiDog).forEach(CorgiDog4Game::move);
        }

        System.out.println("Only one corgi dog left in the garden.");
    }

    @Override
    public void stopGame() {
        running = false;
    }
}
