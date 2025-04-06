package oblac;

/**
 * Channel: Garden -> Game.
 */
public interface Game_for_Garden {
    /**
     * Command: stops the game.
     */
    void stopGame();

    /**
     * Event: a dog moved.
     */
    void corgiDogMovedToNewPosition(CorgiDogRef corgiDog, Position newPosition);
}
