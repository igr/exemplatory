package sad;

import java.util.function.Predicate;

public class PredicateCounter<T> {
	private final Predicate<T> predicate;
	private int count = 0;

	public PredicateCounter(Predicate<T> predicate) {
		this.predicate = predicate;
	}

	public void process(T t) {
		if (predicate.test(t)) {
			count++;
		}
	}

	public int count() {
		return count;
	}

	public static <T> PredicateCounter<T> of(Predicate<T> predicate) {
		return new PredicateCounter<>(predicate);
	}

}
