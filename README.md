# ProcessingGameCore

A console-based process scheduling simulation game written in F# using .NET 6. This project implements a text-based game where players manage processes (called "Ados") and processors to efficiently complete programs within the fewest time ticks.

## Overview

ProcessingGameCore is an educational and entertaining simulation that demonstrates concepts of process scheduling, resource management, and parallel processing. Players must strategically assign processes to processors and advance time to complete programs, with the goal of maximizing efficiency.

### Game Mechanics

- **Programs**: Collections of processes (Ados) that must all be completed before the program finishes
- **Ados (Processes)**: Individual units of work with specific processing requirements
- **Processors**: Computing resources that can handle multiple Ados simultaneously
- **Ticks**: Units of time that advance processing
- **Power**: Processor efficiency (currently fixed at 1 unit per tick)

### Key Rules

1. Ados can only be moved to processors when they are in "Ready" status
2. Once an Ado is placed in a processor, it cannot be moved
3. Processors process Ados in parallel, but an Ado can only exit when ALL Ados from its program are complete
4. The score is based on the number of programs completed within a given number of ticks

## Project Structure

```
ProcessingGameCore/
├── ProcessingGameCore.sln          # Visual Studio solution file
├── ProcessingGame/                 # Core game library
│   ├── ProcessingGame.fsproj       # F# library project
│   ├── ProcessingGameLibrary.fs    # Core types and game logic
│   ├── ProcessingGameInterface.fs  # User interface and input handling
│   ├── EnvironmentBuilder.fs       # Sample game environment setup
│   └── GamePlay.fs                 # Main game loop and user interaction
├── ProcessingGameCore/             # Console application
│   ├── ProcessingGameConsole.fsproj # Console project
│   └── Program.fs                  # Application entry point
└── UnitTests/                      # Unit tests
    ├── UnitTests.fsproj            # Test project
    ├── UnitTest1.fs                # Test cases
    └── Program.fs                  # Test runner
```

## Prerequisites

- [.NET 6.0 SDK](https://dotnet.microsoft.com/download/dotnet/6.0) or later
- A terminal/console for running the game

## Building the Project

1. Clone the repository:
   ```bash
   git clone https://github.com/edwardjcw/ProcessingGameCore.git
   cd ProcessingGameCore
   ```

2. Build the solution:
   ```bash
   dotnet build
   ```

3. Run the tests (optional):
   ```bash
   dotnet test
   ```

## Running the Game

Execute the console application:

```bash
dotnet run --project ProcessingGameCore/ProcessingGameConsole.fsproj
```

Or from the solution root:

```bash
cd ProcessingGameCore
dotnet run
```

## How to Play

The game starts with an introductory message explaining the controls:

### Commands

- **Move Ado to Processor**: `ado_id processor_id`
  - Example: `dfd4 b02c`
  - Type the first few characters of each ID (case-insensitive)
  - If multiple IDs match the prefix, one is selected randomly

- **Advance Time**: Enter a positive integer (e.g., `5`)
  - Advances the simulation by that many ticks
  - Processors work on their assigned Ados during each tick

- **Status**: `s`
  - Displays current state of all programs, ready Ados, and processors

- **Help**: `i`
  - Shows the introductory message again

- **Exit**: `e`
  - Ends the game

### Sample Environment

The default game starts with:
- 3 Programs, each containing 2 Ados (sizes 2 and 3)
- 5 Processors, each with capacity 10 and power 1

### Winning Strategy

The challenge is to assign Ados to processors in a way that maximizes parallel processing while respecting the constraint that all Ados from a program must complete before any can exit their processors.

## Technical Details

### Architecture

- **Functional Programming**: Written in F# with emphasis on immutability and functional composition
- **Asynchronous Processing**: Uses F#'s `MailboxProcessor` for handling user input and game state updates
- **Type Safety**: Strong typing prevents many runtime errors
- **Modular Design**: Separated concerns between game logic, user interface, and environment setup

### Key Components

- `ProcessingGameLibrary.fs`: Defines core types (`Ado`, `Processor`, `Program`, `Environment`) and game transformation logic
- `ProcessingGameInterface.fs`: Handles user input parsing and message passing
- `GamePlay.fs`: Implements the main game loop and user interaction
- `EnvironmentBuilder.fs`: Provides sample game configurations

### Testing

Unit tests cover basic game mechanics:
- Time advancement (ticks)
- Ado movement between processors
- State transitions and validation

Run tests with:
```bash
dotnet test
```

## Contributing

1. Fork the repository
2. Create a feature branch
3. Make your changes
4. Add tests for new functionality
5. Ensure all tests pass
6. Submit a pull request

## Future Enhancements

Potential improvements could include:
- Variable processor power levels
- More complex program structures
- Scoring system with leaderboards
- GUI interface
- Network multiplayer
- Custom environment builder
- Save/load game state

## License

This project is open source. Please check the repository for licensing information.

## Acknowledgments

Built as a demonstration of F# capabilities in game development and process scheduling simulation.

## Web UI (New!)

A new web-based user interface has been added to provide a graphical way to interact with the game. You can drag and drop "Ados" from programs to processors.

### Prerequisites for Web UI

- [Node.js and npm](https://nodejs.org/en/)

### Running the Web Application

To run the web application, you need to start both the backend API server and the frontend development server.

1.  **Start the Backend API Server:**
    Open a terminal and run the following command from the root of the project:

    ```bash
    dotnet run --project ProcessingGame.WebAPI/ProcessingGame.WebAPI.fsproj
    ```

    The API server will start and listen on `http://localhost:5250` and `https://localhost:7265`.

2.  **Start the Frontend Development Server:**
    Open a *new* terminal and navigate to the `processing-game-ui` directory:

    ```bash
    cd processing-game-ui
    ```

    Install the dependencies (only needs to be done once):

    ```bash
    npm install
    ```

    Then, start the development server:

    ```bash
    npm run dev
    ```

    The frontend server will start and be accessible at `http://localhost:5173`.

3.  **Open the Application:**
    Open your web browser and navigate to `http://localhost:5173`. You should see the game interface and be able to interact with it.