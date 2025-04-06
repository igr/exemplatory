package oblac;

public interface GardenChannel {

    Position placeCorgiDog(final CorgiDogRef corgiDog);

    sealed interface MoveResult {
        record StopOnBorder() implements MoveResult {}
        record MovedToEmptyPlace(Position position) implements MoveResult {}
        record MovedToOccupiedPlace(Position position) implements MoveResult {}
    }
    MoveResult corgiWantsToMove(final CorgiDogRef corgiDog, final Position newPosition);

    void corgiFellAsleep(final CorgiDogRef corgiDog);
}
