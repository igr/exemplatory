package oblac;

import java.util.Random;

import static oblac.Garden4CorgiDog.Direction.DOWN;
import static oblac.Garden4CorgiDog.Direction.LEFT;
import static oblac.Garden4CorgiDog.Direction.RIGHT;
import static oblac.Garden4CorgiDog.Direction.UP;

public class CorgiDog implements CorgiDog4Game {

    // state
    private final CorgiDogRef ref;   // reference
    private final CorgiName name;
    private int energy;

    // internals
    private final Random random = new Random();

    // dependencies (channels)
    private final Garden4CorgiDog garden;

    public CorgiDog(final CorgiDogRef ref, final CorgiName name, final Garden4CorgiDog garden) {
        this.ref = ref;
        this.name = name;
        this.energy = 10;

        this.garden = garden;
    }

    @Override
    public void move() {
        if (energy == 0) {
            return;
        }

        final var direction = switch (random.nextInt(4)) {
            case 0 -> UP;
            case 1 -> DOWN;
            case 2 -> RIGHT;
            case 3 -> LEFT;
            default -> throw new IllegalStateException("Invalid direction: " + random.nextInt());
        };
        final var moveResult = garden.corgiWantsToMove(ref, direction);

        switch (moveResult) {
            case final Garden4CorgiDog.MoveResult.StopOnBorder nan ->{}
            case final Garden4CorgiDog.MoveResult.MovedToEmptyPlace nan -> movedSuccessfully();
            case final Garden4CorgiDog.MoveResult.MovedToOccupiedPlace nan -> {
                movedSuccessfully();
                bump();
            }
        }
    }

    private void movedSuccessfully() {
    }

    private void bump() {
        energy--;
        if (energy == 0) {
            garden.corgiFellAsleep(ref);
        }
    }
}
