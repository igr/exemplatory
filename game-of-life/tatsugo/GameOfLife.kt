package example.gol

import kotlinx.coroutines.coroutineScope
import tatsugo.FleetRef
import tatsugo.spawnFleet

private const val SIZE = 21
private const val MAX_GENERATIONS = 6

suspend fun main(): Unit = coroutineScope {
	val fleetRef = spawnFleet(
		"cells",
		this,
	) { ref, particleAddress ->
		when (particleAddress) {
			Grid.address -> Grid.new(SIZE, MAX_GENERATIONS)
			else -> Cell.new(ref, particleAddress, calcMaxForAddress(particleAddress, SIZE))
		}
	}
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


