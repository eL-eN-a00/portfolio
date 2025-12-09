# Connect 4 - Python Implementation

This project is a fully coded **Connect 4 Game** developed entirely by myself from scratch in Python.
It features a simple text-based interface, customizable display options, and multiple gameplay modes including a computer-controlled opponent with multiple rule-based difficulty levels.

---

## Features

### Core Game Mechanics
- Classic **Connect 4** rules
   - Players take turns dropping tokens into a 7x7 grid.
   - The first player to align **four consecutive tokens** horizontally, vertically, or diagonally wins.
   - The game ends when a player wins or when the grid is full.
- Full-turn based system with accurate detection of win conditions and draw situations.

### Text-Based Display
The game uses a simple, clean, console-based visualization made with characters such as:
- '|' for vertical separators
- '_' for horizontal separators

The player can choose between **different board display styles** at the start of the game.

### Game Modes
- **Player vs Player**
- **Player vs Computer**
- **Computer vs Computer**

### Computer Opponent (Rule-Based logic)
The computer-controlled opponent does **not** use machine learning or modern AI techniques.
Instead, all its behavior is implemented through **explicit algorithmic rules** that I programmed manually.

There are **three difficulty levels**:

1. **Easy - Random Moves**
    The computer selects any valid column at random.

2. **Medium - Best Immediate Moves**
    The computer analyzes the board to:
    - Create potential winning moves
    - Block the opponent's winning opportunities

3. **Hard - Strategic Move Without Allowing Player Win**
    An enhanced version of the medium mode, where the computer:
    - Chooses the best move available
    - Avoids moves that would allow the opponent to win next turn
    - Evaluates consequences of its choices

These decision rules give the impression of a smarter opponent, but everything is entirely **deterministic** and coded from scratch.