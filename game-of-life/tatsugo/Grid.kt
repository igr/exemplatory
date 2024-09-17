package example.gol

import example.gol.Grid.Message
import example.gol.Grid.Tick
import tatsugo.FleetRef
import tatsugo.Particle
import tatsugo.ParticleAddress

class Grid(
	override val address: ParticleAddress,
	config: GridConfig,
	val behavior: suspend (Grid, Message) -> Grid
) : Particle<Grid, Message> {

	override suspend fun behavior(msg: Message): Grid = behavior(this, msg)

	sealed interface Message
	data class Tick(
		val fleetRef: FleetRef,
		val fromX: Int,
		val fromY: Int,
		val generation: Int,
		val newStatus: CellStatus
	) : Message

	val state = GridState(config = config)

	companion object {
		fun new(size: Int, maxGeneration: Int): Grid {
			return Grid(address, GridConfig(size, maxGeneration), ::gridBehaviour)
		}

		var address = ParticleAddress("GRID")
	}
}

suspend fun gridBehaviour(grid: Grid, msg: Message): Grid {
	val config = grid.state.config

	return when (msg) {
		// cell is updated
		is Tick -> {
			if (grid.state.gameFinished(msg.generation)) {
				return grid
			}

			grid.state.updateStatus(
				msg.generation,
				msg.fromX,
				msg.fromY,
				msg.newStatus
			)
			if (grid.state.generationFinished(msg.generation)) {
				grid.state.printMatrix(msg.generation)
			}
			announceStateToNeighbours(msg, config.gridSize)
			grid
		}
	}
}
private suspend fun announceStateToNeighbours(tick: Tick, gridSize: Int) {
	val x = tick.fromX
	val y = tick.fromY
	listOf(
		(x - 1 to y - 1),
		(x - 1 to y),
		(x - 1 to y + 1),
		(x to y - 1),
		(x to y + 1),
		(x + 1 to y - 1),
		(x + 1 to y),
		(x + 1 to y + 1),
	).filter {
		if (it.first < 0 || it.first >= gridSize) false
		else if (it.second < 0 || it.second >= gridSize) false
		else true
	}.forEach {
		tick.fleetRef.send(it.address(), Cell.AcceptNeighbourStatus(tick.generation, tick.newStatus))
	}
}
