package oblac;

/**
 * Channel: CorgiDog -> Garden
 */
public interface Garden4CorgiDog {

    enum Direction {
        UP, DOWN, LEFT, RIGHT
    }
    sealed interface MoveResult {
        record StopOnBorder() implements MoveResult {}
        record MovedToEmptyPlace() implements MoveResult {}
        record MovedToOccupiedPlace() implements MoveResult {}
    }

    /**
     * Command: there is an intention to move.
     */
    MoveResult corgiWantsToMove(final CorgiDogRef corgiDog, final Direction Direction);

    /**
     * Event: Corgi fell asleep.
     */
    void corgiFellAsleep(final CorgiDogRef corgiDog);
}
