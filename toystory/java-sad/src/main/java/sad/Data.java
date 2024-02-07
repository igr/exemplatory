package sad;

import java.util.List;

import static java.lang.Boolean.FALSE;
import static java.lang.Boolean.TRUE;

public class Data {

	public static List<Toy> toys() {
		return List.of(
			new Toy("bear", Toy.Color.BLACK, FALSE, 11.99),
			new Toy("doll", Toy.Color.RED, FALSE, 4.0),
			new Toy("car", Toy.Color.BLUE, TRUE, 9.99),
			new Toy("truck", Toy.Color.BLUE, TRUE, 13.00),
			new Toy("spaceship", Toy.Color.GREEN, FALSE, 25.00)
		);
	}

}
