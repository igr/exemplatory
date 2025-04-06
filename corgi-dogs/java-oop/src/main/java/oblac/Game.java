package oblac;

import java.util.List;

public class Game implements Game4Garden {

    public static void main(final String[] args) {
        final var game = new Game();
        game.placeCorgis();
        game.run();
    }

    /**
     * This is required for the local state of the Game object.
     * Note that the name is NOT stored in the CorgiDog, as it
     * is unnecessary for the abstraction!
     */
    record CorgiData(CorgiDogRef ref, CorgiName name, CorgiDog corgiDog) {}

    // state
    private final Garden garden;
    private final List<CorgiData> corgis;
    private boolean running = false;

    // dependencies
    private final GameCorgiDogMoveListener gameCorgiDogMoveListener;

    Game() {
        garden = new Garden(this);
        corgis = List.of(
            createCorgi("Mile"),
            createCorgi("Žile")
        );
        gameCorgiDogMoveListener = (corgiName, position) -> System.out.println(corgiName.value() + " moved to " + position.x() + ":" + position.y());
    }

    private CorgiData createCorgi(final String name) {
        final var ref = CorgiDogRef.create();
        final var corgiName = new CorgiName(name);
        final var dog = new CorgiDog(ref, garden);
        return new CorgiData(ref, corgiName, dog);
    }

    public void placeCorgis() {
        corgis.forEach(it -> garden.placeCorgiDog(it.ref));
    }

    public void run() {
        this.running = true;
        while (running) {
            corgis.stream()
                .map(it -> it.corgiDog)
                .forEach(CorgiDog4Game::move);
        }
        System.out.println("Only one corgi dog left in the garden.");
    }

    @Override
    public void stopGame() {
        running = false;
    }

    /**
     * Event listener for Corgi moves.
     */
    @Override
    public void corgiDogMovedToNewPosition(final CorgiDogRef corgiDog, final Position newPosition) {
        final var data = locateExisting(corgiDog);
        gameCorgiDogMoveListener.accept(data.name(), newPosition);
    }

    private CorgiData locateExisting(final CorgiDogRef corgiDog) {
        return corgis.stream()
            .filter(it -> it.ref == corgiDog)
            .findFirst()
            .orElseThrow(IllegalArgumentException::new);
    }
}
