import gleam/int
import gleam/io
import gleam/list
import gleam/result
import gleam/string

pub type Cell {
  Live
  Dead
}

pub type Position {
  Position(x: Int, y: Int)
}

pub type Grid =
  List(List(Cell))

const width = 10

const height = 10

fn cell_to_string(cell: Cell) -> String {
  case cell {
    Live -> "*"
    Dead -> "."
  }
}

fn seed_state(pos: Position) -> Cell {
  case int.is_even(pos.x + pos.y) {
    True -> Live
    False -> Dead
  }
}

fn initialize_grid(get_state: fn(Position) -> Cell) -> Grid {
  use y <- list.map(list.range(0, height - 1))
  use x <- list.map(list.range(0, width - 1))
  get_state(Position(x, y))
}

fn get_cell(grid: Grid, pos: Position) -> Cell {
  grid
  |> list.drop(pos.y)
  |> list.first
  |> result.try(fn(row) { row |> list.drop(pos.x) |> list.first })
  |> result.unwrap(Dead)
}

fn collect_neighbors(pos: Position) -> List(Position) {
  let deltas = [-1, 0, 1]
  use dx <- list.flat_map(deltas)
  use dy <- list.filter_map(deltas)
  case dx, dy {
    0, 0 -> Error(Nil)
    _, _ -> {
      let nx = pos.x + dx
      let ny = pos.y + dy
      case nx >= 0 && nx < width && ny >= 0 && ny < height {
        True -> Ok(Position(nx, ny))
        False -> Error(Nil)
      }
    }
  }
}

fn count_live_neighbors(grid: Grid, pos: Position) -> Int {
  collect_neighbors(pos)
  |> list.count(fn(p) { get_cell(grid, p) == Live })
}

fn next_state(cell: Cell, live_neighbors: Int) -> Cell {
  case cell, live_neighbors {
    Live, 2 | Live, 3 -> Live
    Dead, 3 -> Live
    _, _ -> Dead
  }
}

fn next_generation(grid: Grid) -> Grid {
  use row, y <- list.index_map(grid)
  use cell, x <- list.index_map(row)
  next_state(cell, count_live_neighbors(grid, Position(x, y)))
}

fn row_to_string(row: List(Cell)) -> String {
  list.map(row, cell_to_string) |> string.concat
}

fn print_grid(grid: Grid) -> Nil {
  list.each(grid, fn(row) { io.println(row_to_string(row)) })
}

fn run_generations(grid: Grid, generations: Int) -> Nil {
  let _ =
    list.fold(list.range(1, generations), grid, fn(grid, gen) {
      let next = next_generation(grid)
      io.println("\nGeneration " <> int.to_string(gen) <> ":")
      print_grid(next)
      next
    })
  Nil
}

pub fn main() -> Nil {
  let grid = initialize_grid(seed_state)
  io.println("\nSeed Grid:")
  print_grid(grid)
  run_generations(grid, 6)
}
