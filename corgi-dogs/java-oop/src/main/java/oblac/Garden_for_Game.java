package oblac;

/**
 * Channel: Game -> Garden.
 */
public interface Garden_for_Game {
    /**
     * Command: Place a Corgi dog in the garden.
     */
    void placeCorgiDog(final CorgiDogRef corgiDog);
}
