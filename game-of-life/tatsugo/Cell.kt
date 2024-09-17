package example.gol

import example.gol.Cell.InitialState
import example.gol.Cell.Msg
import tatsugo.FleetRef
import tatsugo.Particle
import tatsugo.ParticleAddress

class Cell(
	override val address: ParticleAddress,
	val fleetRef: FleetRef,
	val state: CellState,
	val behavior: suspend (Cell, Msg) -> Cell
) : Particle<Cell, Msg> {

	override suspend fun behavior(msg: Msg): Cell = behavior(this, msg)

	/**
	 * Common utility to change the state and behavior of the cell.
	 */
	fun to(newState: CellState, newBehavior: suspend (Cell, Msg) -> Cell): Cell =
		Cell(address, fleetRef, newState, newBehavior)

	// messages

	sealed interface Msg
	data class InitialState(
		val cellStatus: CellStatus,
		val gridSize: Int,
	) : Msg
	data class AcceptNeighbourStatus(
		val generation: Int,
		val status: CellStatus
	): Msg

	companion object {
		fun new(fleetRef: FleetRef, address: ParticleAddress, max: Int): Cell {
			return Cell(
				address,
				fleetRef,
				CellState(max = max),
				::initialBehaviour
			)
		}
	}
}


/**
 * Initial behavior of the cell.
 */
suspend fun initialBehaviour(cell: Cell, msg: Msg): Cell {
	val state = cell.state
	val pos = addressToPosition(cell.address)
	return when (msg) {
		is InitialState -> {
			// generation 0, announce status to neighbours
			val tickMsg = Grid.Tick(cell.fleetRef, pos.first, pos.second, 0, msg.cellStatus)
			cell.fleetRef.send(Grid.address, tickMsg)

			// update the status of the cell
			val newState = state.update(0) { it.copy(status = msg.cellStatus) }
			cell.to(newState, ::livingBehaviour)
		}
		else -> cell
	}
}

/**
 * The living behaviour of the cell.
 * Example when behaviour is implemented outside the cell.
 */
private suspend fun livingBehaviour(cell: Cell, msg: Msg): Cell {
	return when (msg) {
		is Cell.AcceptNeighbourStatus -> {
			val currentCell = cell.state[msg.generation]
			val updatedCell = currentCell.addNeighbourStatus(msg.status)

			if (updatedCell.readyForProgress()) {
				// all neighbours have reported their status, ready to progress the cell
				val newCell = updatedCell.progress()
				val nextGeneration = msg.generation + 1
				val position = addressToPosition(cell.address)

				val tickMsg = Grid.Tick(cell.fleetRef, position.first, position.second, nextGeneration, newCell.status)
				cell.fleetRef.send(Grid.address, tickMsg)

				val newState = cell.state.update(nextGeneration) { newCell }
				cell.to(newState, ::livingBehaviour)
			} else {
				val newState = cell.state.update(msg.generation) { updatedCell }
				cell.to(newState, ::livingBehaviour)
			}
		}
		else -> cell
	}
}


