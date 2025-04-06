package oblac;

import java.util.HashSet;
import java.util.Random;
import java.util.Set;

public class Garden implements Garden4CorgiDog, Garden4Game {

    private record DogPosition(CorgiDogRef corgiDog, Position position) {}

    // dependencies
    private final Game4Garden gameChannel;

    // state
    private final int width = 3;
    private final int height = 3;
    private final Set<DogPosition> dogs = new HashSet<>();

    // internals
    private final Random random = new Random();

    public Garden(final Game4Garden gameChannel) {
        this.gameChannel = gameChannel;
    }

    @Override
    public void placeCorgiDog(final CorgiDogRef corgiDog) {
        final var randomPosition = new Position(random.nextInt(width), random.nextInt(height));
        dogs.add(new DogPosition(corgiDog, randomPosition));
    }

    @Override
    public MoveResult corgiWantsToMove(final CorgiDogRef corgiDog, final Direction direction) {
        final var position = locateExistingDog(corgiDog);
        final var newPosition = switch (direction) {
            case UP -> new Position(position.x(), position.y() + 1);
            case DOWN -> new Position(position.x(), position.y() - 1);
            case LEFT -> new Position(position.x() - 1, position.y());
            case RIGHT -> new Position(position.x() + 1, position.y());
        };

        // check borders
        if (newPosition.x() < 0 || newPosition.y() < 0 || newPosition.x() == width || newPosition.y() == height) {
            return new MoveResult.StopOnBorder();
        }

        // detect other dogs
        return dogs
            .stream()
            .filter(dp -> !dp.corgiDog.equals(corgiDog))
            .filter(dp -> dp.position.equals(newPosition))
            .findAny()
            .map(dp -> (MoveResult) new MoveResult.MovedToOccupiedPlace())
            .orElseGet(MoveResult.MovedToEmptyPlace::new);
    }

    @Override
    public void corgiFellAsleep(final CorgiDogRef corgiDog) {
        dogs.removeIf(t -> t.corgiDog == corgiDog);
        if (dogs.size() == 1) {
            gameChannel.stopGame();
        }
    }

    private Position locateExistingDog(final CorgiDogRef corgiDog) {
        return dogs.stream()
            .filter(it -> it.corgiDog == corgiDog)
            .findFirst()
            .map(DogPosition::position)
            .orElseThrow(IllegalArgumentException::new);
    }
}
