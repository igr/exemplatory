package oblac;

import java.util.Random;

public class CorgiDog implements CorgiDogChannel {

    // reference
    private final CorgiDogRef ref = CorgiDogRef.create();

    // state
    private Position position;
    private int energy;
    private final CorgiName name;

    // internals
    private final Random random = new Random();

    // dependencies (channels)
    private final GardenChannel garden;

    // event handler
    private final CordiDogMoveListener listener;

    public CorgiDog(final GardenChannel garden, final CorgiName name, final CordiDogMoveListener listener) {
        this.energy = 10;
        this.name = name;
        this.listener = listener;

        this.garden = garden;
        this.position = garden.placeCorgiDog(ref);

        listener.accept(name, position);
    }

    @Override
    public void move() {
        if (energy == 0) {
            return;
        }

        final int direction = random.nextInt(4);
        var x = position.x();
        var y = position.y();
        switch (direction) {
            case 0 -> x++;
            case 1 -> x--;
            case 2 -> y++;
            case 3 -> y--;
        }
        final var newPosition = new Position(x, y);

        final var moveResult = garden.corgiWantsToMove(ref, newPosition);

        switch (moveResult) {
            case final GardenChannel.MoveResult.StopOnBorder ignored ->{}
            case final GardenChannel.MoveResult.MovedToEmptyPlace result -> movedSuccessfully(result.position());
            case final GardenChannel.MoveResult.MovedToOccupiedPlace result -> {
                movedSuccessfully(result.position());
                bump();
            }
        }
    }

    private void movedSuccessfully(final Position newPosition) {
        this.position = newPosition;
        listener.accept(this.name, newPosition);
    }

    private void bump() {
        energy--;
        if (energy == 0) {
            garden.corgiFellAsleep(ref);
        }
    }
}
