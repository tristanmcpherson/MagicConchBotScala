# Self-Registering Command Pattern

## Overview

The command system now uses a self-registering pattern where each command defines its own metadata, argument specifications, and parsing logic. This eliminates the need to modify multiple files when adding a new command.

## How to Add a New Command

### Step 1: Create Your Command File

Create a new file in `src/main/scala/dev/raegous/magicconch/commands/YourCommand.scala`:

```scala
package dev.raegous.magicconch.commands

import cats.effect.*
import cats.implicits.*
import org.typelevel.log4cats.Logger
import dev.raegous.magicconch.VoiceManager

class YourCommand[F[_]: Async](voiceManager: VoiceManager[F])(using Logger[F]) extends Command[F] {
  // Command name (used in !commandname and /commandname)
  val name = "yourcommand"

  // Human-readable description
  val description = "Does something cool"

  // Define your arguments (can be empty list if no args needed)
  val arguments = List(
    CommandArgument("input", "The input parameter", required = true),
    CommandArgument("option", "An optional parameter", required = false)
  )

  // Implement your command logic
  def execute(context: CommandContext[F]): F[CommandResult] = {
    // Access parsed arguments via context.args
    val input = context.args.get("input")
    val option = context.args.get("option")

    // Your command logic here
    Async[F].pure(CommandResult(s"✅ Command executed with input: $input"))
  }
}
```

### Step 2: Register in CommandRegistry

Open `src/main/scala/dev/raegous/magicconch/commands/CommandRegistry.scala` and add ONE LINE:

```scala
private val commands: Map[String, Command[F]] = Map(
  "play" -> new PlayCommand[F](voiceManager),
  "stop" -> new StopCommand[F](voiceManager),
  // ... other commands ...
  "yourcommand" -> new YourCommand[F](voiceManager)  // <-- Add this line
)
```

### That's it! 🎉

Your command is now:
- ✅ Available via text commands (`!yourcommand input`)
- ✅ Available via slash commands (`/yourcommand input`)
- ✅ Automatically registered with Discord
- ✅ Arguments automatically parsed from both text and slash commands

## What Changed?

### Before (Old Pattern)
To add a command, you had to modify:
1. Create command file
2. **MessageHandler.scala** - Add argument parsing logic (lines 44-48)
3. **SlashCommandManager.scala** - Add SlashCommand definition (lines 22-100)
4. **CommandRegistry.scala** - Add command instance (line 13-23)

### After (New Pattern)
To add a command, you only need to:
1. Create command file with metadata
2. **CommandRegistry.scala** - Add ONE line to register it

## Architecture

### Command Trait
```scala
trait Command[F[_]] {
  def name: String
  def description: String
  def arguments: List[CommandArgument]
  def execute(context: CommandContext[F]): F[CommandResult]

  // Automatically generated from metadata
  def toSlashCommand: SlashCommand

  // Default parsing (can be overridden for complex cases)
  def parseTextArgs(argsString: String): Map[String, String]
}
```

### CommandArgument
```scala
case class CommandArgument(
  name: String,
  description: String,
  required: Boolean = false
)
```

### How It Works

1. **Commands declare their metadata** - Each command specifies its name, description, and arguments
2. **Automatic slash command generation** - The `toSlashCommand` method converts metadata to Discord's format
3. **Automatic argument parsing** - The `parseTextArgs` method handles text command parsing
4. **Single source of truth** - Command definitions live in one place

### Examples from Existing Commands

#### Simple command (no arguments)
```scala
class StopCommand[F[_]: Async](voiceManager: VoiceManager[F])(using Logger[F]) extends Command[F] {
  val name = "stop"
  val description = "Stop the current music and clear queue"
  val arguments = List.empty

  def execute(context: CommandContext[F]): F[CommandResult] = {
    voiceManager.stopMusic(context.guildId) >>
    voiceManager.clearQueue(context.guildId) >>
    Async[F].pure(CommandResult("🛑 Music stopped and queue cleared!"))
  }
}
```

#### Command with required argument
```scala
class PlayCommand[F[_]: Async](voiceManager: VoiceManager[F])(using Logger[F]) extends Command[F] {
  val name = "play"
  val description = "Play music from a YouTube URL"
  val arguments = List(
    CommandArgument("url", "YouTube URL to play", required = true)
  )

  def execute(context: CommandContext[F]): F[CommandResult] = {
    context.args.get("url") match {
      case Some(url) => // handle URL
      case None => // handle missing URL
    }
  }
}
```

#### Command with optional argument
```scala
class MagicConchCommand[F[_]: Async](using Logger[F]) extends Command[F] {
  val name = "magicconch"
  val description = "Ask the Magic Conch Shell a question"
  val arguments = List(
    CommandArgument("question", "Your question for the Magic Conch", required = false)
  )

  def execute(context: CommandContext[F]): F[CommandResult] = {
    val response = responses(scala.util.Random.nextInt(responses.length))
    Async[F].pure(CommandResult(s"🐚 The Magic Conch says: **$response**"))
  }
}
```

## Benefits

1. **Less boilerplate** - No need to update multiple files
2. **Type safety** - Argument specs are typed and checked at compile time
3. **Single source of truth** - Command metadata lives with the command
4. **Easier maintenance** - Changes to a command only touch one file
5. **Automatic Discord integration** - Slash commands automatically generated
6. **Extensible** - Commands can override `parseTextArgs` for custom parsing

## Advanced: Custom Argument Parsing

If you need custom argument parsing for text commands, override `parseTextArgs`:

```scala
class ComplexCommand[F[_]: Async]() extends Command[F] {
  val name = "complex"
  val description = "Command with complex argument parsing"
  val arguments = List(
    CommandArgument("key1", "First parameter", required = true),
    CommandArgument("key2", "Second parameter", required = true)
  )

  // Custom parsing logic
  override def parseTextArgs(argsString: String): Map[String, String] = {
    val parts = argsString.split(",").map(_.trim)
    Map(
      "key1" -> parts.headOption.getOrElse(""),
      "key2" -> parts.drop(1).headOption.getOrElse("")
    )
  }

  def execute(context: CommandContext[F]): F[CommandResult] = ???
}
```
