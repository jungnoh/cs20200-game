# CLI Yacht Game

A command-line port of the dice game **Yacht**, implemented in F# on .NET 10
using [Terminal.Gui](https://github.com/gui-cs/Terminal.Gui). Supports
two-player hot-seat play and single-player play against a bot with three
difficulty levels.

This project is a term project for [KAIST CS20200](https://softsec.kaist.ac.kr/courses/2026s-cs20200/) Programming Principles of Spring 2026.

## Requirements

- [.NET SDK 10.0](https://dotnet.microsoft.com/download/dotnet/10.0)
- A terminal that supports interactive TUI input (most modern terminals work)
  - Terminal screen size must be large enough to fit the game UI.

## Running the game

After cloning the repository, restore the dotnet project:

```sh
dotnet restore
```

Then the game can be run.

```sh
dotnet run --project Yacht
```

On the main menu, select **1 Player**, **2 Players**, or **Exit** with the
arrow keys and Enter. In-game, follow the on-screen prompts:

- Press **Left** / **Right** to shake the dice, then **Enter** to roll.
- Toggle which dice to keep, then roll again (up to 3 rolls per turn) or
  record the current dice into one of the 12 unfilled scorecard categories.

### Key bindings

| Key | Action |
| --- | --- |
| **↑** / **↓** | Move selection within the dice or category list |
| **Tab** / **Shift+Tab** | Move focus between the dice list and category list |
| **Enter** | Confirm: start shaking, throw the dice, or record the highlighted category |
| **← / →** | Shake the cup left or right while the dice are in hand |
| **Space** | Toggle the keep flag on the currently selected die |
| **Esc** | Cancel dice throwing |

Please refer to external links for rules ([English](https://www.thesprucecrafts.com/yacht-dice-game-rules-412797), [Korean](https://namu.wiki/w/%EC%9A%94%ED%8A%B8(%EC%A3%BC%EC%82%AC%EC%9C%84)#s-2))

## Notes
### Unit tests
Scoring rules, category selection, and bot difficulty checks are covered by unit tests. Tests can be verified by running `dotnet test`.

Bot difficulty verification is done by running games between the easy and hard bot 100 times, and recording the win rate. The hard bot must win against the easy bot for at least 60%.

### LLM usage disclosure

Claude Opus 4.7 and GPT-5.5 was used in the process of developing this process. 
Below are the details of LLM use as required the course instructors:

- LLM Usage
  - Test case generation and verification based on the spec document
  - Drawing ASCII art such as dice (`DiceArt.fs`)
  - Repetitive TUI generation and scene transitition management
  - Adverserial code review: LLMs reviewed my code, and different LLM models reviewed each other's work
- Changes or reprompts done to the LLM
  - Although not included in the git tree, addtional rules such as using `|>` extensively were instructed in `CLAUDE.md` to write idiomatic F# code.
  - More manual verification for UI was needed than other tasks since LLMs were not able to observe TUI interaction in detail.
- Main points the LLM was not able to do correctly
  - Bot AI: The LLM was unsuccessful to choose appropriate algorithms bots with adjusted difficulty.
  - Common TUI structure: Architecting UI code (choosing a TUI library, building scenes) by myself produced better results.
