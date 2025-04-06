package oblac;

import java.util.UUID;

public record CorgiDogRef(UUID uuid) {
    public static CorgiDogRef create() {
        return new CorgiDogRef(UUID.randomUUID());
    }
}
