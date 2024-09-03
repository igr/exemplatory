package example.gol

/**
 * The living status of a cell.
 */
enum class CellStatus { Alive, Dead }

/**
 * The various live data of a cell that belongs to a single generation.
 * It does not contain the address on purpose!
 *
 * @param status The status of the cell in the current generation.
 * @param deadCount The number of dead neighbours around the cell.
 * @param aliveCount The number of living neighbours around the cell.
 * @param max The maximum number of neighbours around the cell.
 */
data class CellData(
	val status: CellStatus,
	val deadCount: Int,
	val aliveCount: Int,
	val max: Int
) {
	fun addNeighbourStatus(status: CellStatus): CellData {
		return when (status) {
			CellStatus.Alive -> copy(aliveCount = aliveCount + 1)
			CellStatus.Dead -> copy(deadCount = deadCount + 1)
		}
	}

	/**
	 * Checks if the cell is ready to progress to the next generation.
	 */
	fun readyForProgress(): Boolean {
		return deadCount + aliveCount == max
	}

	/**
	 * Progress the cell to the next generation.
	 */
	fun progress(): CellData {
		if (readyForProgress().not()) {
			throw IllegalStateException("Cell is not ready to progress to the next generation")
		}
		val newStatus = when (status) {
			CellStatus.Alive -> {
				when (aliveCount) {
					in 2..3 -> CellStatus.Alive
					else -> CellStatus.Dead
				}
			}
			CellStatus.Dead -> {
				when (aliveCount) {
					3 -> CellStatus.Alive
					else -> CellStatus.Dead
				}
			}
		}
		return CellData(newStatus, 0, 0, max)
	}
}

/**
 * Finally, the state of a cell. It is a list of [CellData]
 * that represents the state of the cell in each generation up to the latest.
 * We didn't have to create a separate class for this, but it makes data state
 * separated.
 */
data class CellState(
	private val history: List<CellData>
) {

	fun update(iteration: Int, cellDataUpdater: (CellData) -> CellData): CellState {
		if (iteration == history.size) {
			// append
			val new = cellDataUpdater(history.last())
			return this.copy(history = history + new)
		}
		// replace
		val new = cellDataUpdater(history[iteration])
		return this.copy(history = history.mapIndexed { index, cellData ->
			if (index == iteration) new else cellData
		})
	}

	operator fun get(index: Int): CellData = history[index]
}
