package sad;

public class RunMe {
	public static void main(String[] args) {
		final var calculateStats = new CalculateStats();
		Data.toys().forEach(calculateStats);
        System.out.println(calculateStats.get());
	}
}
