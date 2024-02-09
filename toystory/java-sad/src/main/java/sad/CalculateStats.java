package sad;

import java.util.EnumMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.function.Consumer;
import java.util.function.Supplier;
import java.util.stream.Collectors;

public class CalculateStats implements Consumer<Toy>, Supplier<ToysStats> {

    @Override
    public void accept(Toy toy) {
        counters.forEach(counter -> counter.accept(toy));
    }

    // builds a final stats object
    // we need to be careful to use the correct mapping
    @Override
    public ToysStats get() {
        return new ToysStats(
            total.count(),
            totalBlue.count(),
            totalExpensiveAndBroken.count(),
            colorDistribution.distributionMap()
        );
    }

    // simple utility
    private static Map<Toy.Color, Integer> mapOf(Map<Toy.Color, AtomicInteger> map) {
        return map.entrySet()
            .stream()
            .collect(Collectors.toMap(Map.Entry::getKey, e -> e.getValue().get()));
    }

    // the list of predicate counters
    // they can be defined in other classes if needed

    private final PredicateCounter<Toy> total =
        PredicateCounter.of(toy -> true);
    private final PredicateCounter<Toy> totalBlue =
        PredicateCounter.of(toy -> toy.color() == Toy.Color.BLUE);
    private final PredicateCounter<Toy> totalExpensiveAndBroken =
        PredicateCounter.of(toy -> toy.broken() && toy.price() > 10);
    private final ColorDistribution colorDistribution = new ColorDistribution();

    // manually managed list of counters

    private final List<Consumer<Toy>> counters = List.of(
        total,
        totalBlue,
        totalExpensiveAndBroken,
        colorDistribution
    );

}
