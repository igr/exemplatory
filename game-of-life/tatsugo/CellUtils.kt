package example.gol

import tatsugo.ParticleAddress

/**
 * Calculate the position from the address.
 */
fun addressToPosition(address: ParticleAddress): Pair<Int, Int> {
	val parts = address.value.split(":")
	return Pair(parts[0].toInt(), parts[1].toInt())
}

/**
 * Constructs the address from the position.
 */
fun Pair<Int, Int>.address(): ParticleAddress {
	return ParticleAddress("${first}:${second}")
}

/**
 * Calculate the maximum number of neighbours for a given address.
 */
fun calcMaxForAddress(address: ParticleAddress, size: Int): Int {
	val (x, y) = addressToPosition(address)

	var max = 8

	// detect edges
	if (x == 0 || x == size - 1) {
		max -= 3
	}
	if (y == 0 || y == size - 1) {
		max -= 3
	}

	// detect corners
	if ((x == 0 || x == size - 1) && (y == 0 || y == size - 1)) {
		max += 1
	}

	return max
}