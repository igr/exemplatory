package oblac;

import java.util.HashSet;
import java.util.Random;
import java.util.Set;

public class Garden implements GardenChannel {

    private record DogPosition(CorgiDogRef corgiDog, Position position) {}

    // dependencies
    private final GameChannel gameChannel;

    // state
    private final int width = 3;
    private final int height = 3;
    private final Set<DogPosition> dogs = new HashSet<>();

    // internals
    private final Random random = new Random();

    public Garden(final GameChannel gameChannel) {
        this.gameChannel = gameChannel;
    }

    /**
     * Places a Corgi dog in the garden.
     */
    @Override
    public Position placeCorgiDog(final CorgiDogRef corgiDog) {
        final var randomPosition = new Position(random.nextInt(width), random.nextInt(height));
        dogs.add(new DogPosition(corgiDog, randomPosition));
        return randomPosition;
    }

    @Override
    public MoveResult corgiWantsToMove(final CorgiDogRef corgiDog, final Position newPosition) {
        // check borders
        if (newPosition.x() < 0 || newPosition.y() < 0 || newPosition.x() == width || newPosition.y() == height) {
            return new MoveResult.StopOnBorder();
        }

        // move
        return dogs
            .stream()
            .filter(dp -> !dp.corgiDog.equals(corgiDog))
            .filter(dp -> dp.position.equals(newPosition))
            .findAny()
            .map(dp -> (MoveResult) new MoveResult.MovedToOccupiedPlace(newPosition))
            .orElseGet(() -> new MoveResult.MovedToEmptyPlace(newPosition));
    }

    @Override
    public void corgiFellAsleep(final CorgiDogRef corgiDog) {
        dogs.removeIf(t -> t.corgiDog == corgiDog);
        if (dogs.size() == 1) {
            gameChannel.stopGame();
        }
    }
}
