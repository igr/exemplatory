package example.gol

import kotlinx.coroutines.coroutineScope
import tatsugo.FleetRef
import tatsugo.spawnFleet

private const val SIZE = 11
private const val MAX_GENERATIONS = 6

suspend fun main(): Unit = coroutineScope {
	val parallel = true
	val fleetRef = if (parallel) spawnFleet(
		"cells",
		this,
		coroutineContext
	) { ref, particleAddress ->
		when (particleAddress) {
			GRID_ADDR -> Grid.new(SIZE, MAX_GENERATIONS)
			else -> Cell.new(ref, particleAddress, calcMaxForAddress(particleAddress, SIZE))
		}
	}
	else {
		spawnFleet(
			"cells",
			this,
		) { ref, particleAddress ->
			when (particleAddress) {
				GRID_ADDR -> Grid.new(SIZE, MAX_GENERATIONS)
				else -> Cell.new(ref, particleAddress, calcMaxForAddress(particleAddress, SIZE))
			}
		}
	}

	println("Parallel: $parallel")
	initializeGrid(fleetRef, SIZE)
	println("Initial state sent")
}

private suspend fun initializeGrid(fleetRef: FleetRef, size: Int) {
	for (i in 0..<size) {
		for (j in 0..<size) {
			val addr = (i to j).address()
			val msg = if ((i + j) % 2 == 0) {
				Cell.InitialState(CellStatus.Alive, size)
			} else {
				Cell.InitialState(CellStatus.Dead, size)
			}
			fleetRef.send(addr, msg)
		}
	}
}


