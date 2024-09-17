package example.gol

data class GridConfig(
	val gridSize: Int,
	val maxGenerations: Int,
	val totalCells: Int = gridSize * gridSize,
)

data class GenerationGrid(
	val generation: Int,
	val matrix: Array<Array<CellStatus>>,
	val count: Int = 0,
)

data class GridState(
	val db: MutableMap<Int, GenerationGrid> = mutableMapOf(),
	val config: GridConfig
) {
	private val startTime = System.currentTimeMillis()

	private fun createGrid() = Array(config.gridSize) { Array(config.gridSize) { CellStatus.Dead } }

	/**
	 * Updates the status of a cell in the grid.
	 * Returns true if the generation is complete.
	 */
	fun updateStatus(generation: Int, x: Int, y: Int, newStatus: CellStatus) {
		val g = db.computeIfAbsent(generation) { GenerationGrid(generation, createGrid()) }
		g.matrix[x][y] = newStatus

		val counts = g.count + 1
		db[generation] = g.copy(count = counts)
	}

	/**
	 * Detects when generation is finished.
	 */
	fun generationFinished(generation: Int): Boolean =
		db[generation]?.count!! >= config.totalCells

	/**
	 * Print the grid for the given generation.
	 */
	fun printMatrix(generation: Int) {
		println("\n---GRID for generation $generation after ${System.currentTimeMillis() - startTime}ms---")

		val matrix = db[generation]?.matrix ?: return
		val gridSize = config.gridSize
		for (i in 0..<gridSize) {
			for (j in 0..<gridSize) {
				print(if (matrix[i][j] == CellStatus.Alive) "*" else "·")
			}
			println()
		}
	}

	/**
	 * Detects when the game is finished.
	 */
	fun gameFinished(generation: Int) = generation >= config.maxGenerations
}