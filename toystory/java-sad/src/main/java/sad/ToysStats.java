package sad;

import java.util.Map;

public record ToysStats(
    long total,
    long blue,
    long expensiveAndBroken,
    Map<Toy.Color, Integer> colors
) {
}
