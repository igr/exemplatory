package sad;

public record Toy(
		String name,
		Color color,
		Boolean broken,
		Double price
) {
	public enum Color {
		RED, BLUE, GREEN, YELLOW, PINK, PURPLE, ORANGE, BLACK, WHITE
	}
}
