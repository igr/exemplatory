package oblac;

public interface Garden4CorgiDog {

    enum Direction {
        UP, DOWN, LEFT, RIGHT
    }
    sealed interface MoveResult {
        record StopOnBorder() implements MoveResult {}
        record MovedToEmptyPlace() implements MoveResult {}
        record MovedToOccupiedPlace() implements MoveResult {}
    }
    MoveResult corgiWantsToMove(final CorgiDogRef corgiDog, final Direction Direction);

    void corgiFellAsleep(final CorgiDogRef corgiDog);
}
