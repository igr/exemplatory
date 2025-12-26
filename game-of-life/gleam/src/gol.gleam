import gleam/int
import gleam/io
import gleam/list
import gleam/string

// Cell type
pub type Cell {
  Live
  Dead
}

// Position type
pub type Position {
  Position(x: Int, y: Int)
}

// Grid is a list of rows, each row is a list of cells
pub type Grid =
  List(List(Cell))

// Grid dimensions
const width = 10

const height = 10

// Convert cell to string representation
fn cell_to_string(cell: Cell) -> String {
  case cell {
    Live -> "*"
    Dead -> "."
  }
}

// Seed function: returns Live for positions where x + y is even
fn seed_state(pos: Position) -> Cell {
  case int.is_even(pos.x + pos.y) {
    True -> Live
    False -> Dead
  }
}

// Initialize the grid with a seed function
fn initialize_grid(get_state: fn(Position) -> Cell) -> Grid {
  list.range(0, height - 1)
  |> list.map(fn(y) {
    list.range(0, width - 1)
    |> list.map(fn(x) { get_state(Position(x, y)) })
  })
}

// Get element at index from list
fn list_at(lst: List(a), index: Int) -> Result(a, Nil) {
  case index < 0 {
    True -> Error(Nil)
    False ->
      case lst {
        [] -> Error(Nil)
        [head, ..tail] ->
          case index == 0 {
            True -> Ok(head)
            False -> list_at(tail, index - 1)
          }
      }
  }
}

// Get cell at position from grid
fn get_cell(grid: Grid, pos: Position) -> Cell {
  case list_at(grid, pos.y) {
    Ok(row) ->
      case list_at(row, pos.x) {
        Ok(cell) -> cell
        Error(_) -> Dead
      }
    Error(_) -> Dead
  }
}

// Collect neighbor positions for a given position
fn collect_neighbors(pos: Position) -> List(Position) {
  let deltas = [-1, 0, 1]
  list.flat_map(deltas, fn(dx) {
    list.filter_map(deltas, fn(dy) {
      let nx = pos.x + dx
      let ny = pos.y + dy
      case dx == 0 && dy == 0 {
        True -> Error(Nil)
        False ->
          case nx >= 0 && nx < width && ny >= 0 && ny < height {
            True -> Ok(Position(nx, ny))
            False -> Error(Nil)
          }
      }
    })
  })
}

// Count live neighbors of a given cell
fn count_live_neighbors(grid: Grid, pos: Position) -> Int {
  collect_neighbors(pos)
  |> list.map(fn(p) { get_cell(grid, p) })
  |> list.filter(fn(cell) { cell == Live })
  |> list.length
}

// Determine the next state of a cell based on Game of Life rules
fn next_state(cell: Cell, live_neighbors: Int) -> Cell {
  case cell {
    Live ->
      case live_neighbors {
        2 | 3 -> Live
        _ -> Dead
      }
    Dead ->
      case live_neighbors {
        3 -> Live
        _ -> Dead
      }
  }
}

// Generate the next generation of the grid
fn next_generation(grid: Grid) -> Grid {
  list.index_map(grid, fn(row, y) {
    list.index_map(row, fn(cell, x) {
      let pos = Position(x, y)
      let neighbors = count_live_neighbors(grid, pos)
      next_state(cell, neighbors)
    })
  })
}

// Build initial grid with seed
fn build_grid() -> Grid {
  initialize_grid(seed_state)
}

// Convert a row to string
fn row_to_string(row: List(Cell)) -> String {
  row
  |> list.map(cell_to_string)
  |> string.concat
}

// Print the grid
fn print_grid(grid: Grid) -> Nil {
  grid
  |> list.each(fn(row) { io.println(row_to_string(row)) })
}

// Run the Game of Life for n generations
fn go_life(max_gen: Int, n: Int, grid: Grid) -> Nil {
  case n {
    0 -> Nil
    _ -> {
      let next_grid = next_generation(grid)
      let gen_num = max_gen - n + 1
      io.println("\nGeneration " <> int.to_string(gen_num) <> ":")
      print_grid(next_grid)
      go_life(max_gen, n - 1, next_grid)
    }
  }
}

pub fn main() -> Nil {
  let grid = build_grid()
  io.println("\nSeed Grid:")
  print_grid(grid)
  let num_generations = 6
  go_life(num_generations, num_generations, grid)
}
