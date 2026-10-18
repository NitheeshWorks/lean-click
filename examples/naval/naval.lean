import LeanClick

/-!
Port of click's `examples/naval/naval.py`: the docopt Naval Fate example
adopted to click.
-/

open LeanClick LeanClick.Terminal

/-- Creates a new ship. -/
click_command ship_new [name := "new"]
    (name : String)
    : IO Unit := echo s!"Created ship {name}"

/-- Moves SHIP to the new location X,Y. -/
click_command ship_move [name := "move"]
    (ship : String)
    (x : Dec) (y : Dec)
    (speed : Int := 10) "Speed in knots." [metavar := some "KN"]
    : IO Unit := echo s!"Moving ship {ship} to {x},{y} with speed {speed}"

/-- Makes SHIP fire to X,Y. -/
click_command ship_shoot [name := "shoot"]
    (ship : String)
    (x : Dec) (y : Dec)
    : IO Unit := echo s!"Ship {ship} fires to {x},{y}"

/-- Manages ships. -/
click_group ship [commands := [ship_new, ship_move, ship_shoot]] : IO Unit := pure ()

/-- Sets a mine at a specific coordinate. -/
click_command mine_set [name := "set"]
    (x : Dec) (y : Dec)
    (ty : String := "moored") [flagValues := [("--moored", "moored", "Moored (anchored) mine. Default."),
                                              ("--drifting", "drifting", "Drifting mine.")]]
    : IO Unit := echo s!"Set {ty} mine at {x},{y}"

/-- Removes a mine at a specific coordinate. -/
click_command mine_remove [name := "remove"]
    (x : Dec) (y : Dec)
    : IO Unit := echo s!"Removed mine at {x},{y}"

/-- Manages mines. -/
click_group mine [commands := [mine_set, mine_remove]] : IO Unit := pure ()

/-- Naval Fate.

This is the docopt example adopted to Click but with some actual
commands implemented and not just the empty parsing which really
is not all that interesting. -/
click_group cli [commands := [ship, mine], params := [versionOption "1.0.0"]] : IO Unit := pure ()

def main (args : List String) : IO UInt32 := cli.run () args (prog := "naval")
