package sad;

import java.util.EnumMap;
import java.util.Map;
import java.util.function.Consumer;

public class ColorDistribution implements Consumer<Toy> {
    private final Map<Toy.Color, Integer> map = new EnumMap<>(Toy.Color.class);
    @Override
    public void accept(Toy toy) {
        final var color = toy.color();
        map.putIfAbsent(color, 0);
        map.computeIfPresent(color, (c, i) -> i + 1);
    }

    public Map<Toy.Color, Integer> distributionMap() {
        return map;
    }
}
