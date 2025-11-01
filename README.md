# Magic Conch Bot

A Discord music bot written in Scala 3 using functional programming with Cats Effect. Inspired by the Magic Conch Shell from SpongeBob SquarePants.

## Features

- **Music Playback**: Play music from YouTube in Discord voice channels
- **Queue Management**: Add, skip, and view the music queue
- **Slash Commands**: Modern Discord slash command interface
- **Magic Conch**: Ask the Magic Conch Shell for wisdom
- **Audio Message Detection**: Responds to voice messages in channels
- **Functional Architecture**: Built with Cats Effect for type-safe, composable effects

## Commands

### Slash Commands

- `/play <youtube-url>` - Add a YouTube video to the queue and play it
- `/stop` - Stop the current music and clear the queue
- `/skip` - Skip to the next track in the queue
- `/queue` - Display the current music queue
- `/join` - Join your current voice channel
- `/leave` - Leave the voice channel
- `/magicconch [question]` - Ask the Magic Conch Shell for guidance

### Legacy Text Commands

- `!magicconch` - Ask the Magic Conch Shell
- `!play <youtube-url>` - Play music from YouTube
- `!stop` - Stop music playback
- `!skip` - Skip current track
- `!queue` - Show the queue
- `!join` - Join voice channel
- `!leave` - Leave voice channel

## Prerequisites

### System Dependencies

1. **Java 11 or higher**
   ```bash
   java -version
   ```

2. **sbt (Scala Build Tool)**
   ```bash
   # macOS
   brew install sbt

   # Linux
   # See https://www.scala-sbt.org/download.html

   # Windows
   # Download from https://www.scala-sbt.org/download.html
   ```

3. **yt-dlp** - For downloading YouTube audio
   ```bash
   # macOS
   brew install yt-dlp

   # Linux
   pip install yt-dlp

   # Windows
   # Download from https://github.com/yt-dlp/yt-dlp/releases
   ```

4. **FFmpeg** - For audio format conversion
   ```bash
   # macOS
   brew install ffmpeg

   # Linux
   sudo apt-get install ffmpeg

   # Windows
   # Download from https://ffmpeg.org/download.html
   ```

### Discord Application Setup

1. Go to the [Discord Developer Portal](https://discord.com/developers/applications)
2. Click "New Application" and give it a name
3. Go to the "Bot" section and click "Add Bot"
4. Enable these **Privileged Gateway Intents**:
   - Message Content Intent
   - Server Members Intent (optional, for voice state tracking)
5. Copy your bot token (keep this secret!)
6. Go to "OAuth2" > "URL Generator"
7. Select scopes: `bot`, `applications.commands`
8. Select bot permissions:
   - Send Messages
   - Connect
   - Speak
   - Use Voice Activity
9. Copy the generated URL and use it to invite the bot to your server

## Setup

1. **Clone the repository**
   ```bash
   cd C:\Git\magic-conch-scala
   ```

2. **Set environment variables**

   Create a `.env` file or set these environment variables:
   ```bash
   export DISCORD_TOKEN="your-bot-token-here"
   export DISCORD_APP_ID="your-application-id-here"
   ```

   On Windows (PowerShell):
   ```powershell
   $env:DISCORD_TOKEN="your-bot-token-here"
   $env:DISCORD_APP_ID="your-application-id-here"
   ```

3. **Build the project**
   ```bash
   sbt compile
   ```

4. **Run the bot**
   ```bash
   sbt run
   ```

   Or create an executable JAR:
   ```bash
   sbt assembly
   java -jar target/scala-3.3.0/magicconch-assembly-0.1.0-SNAPSHOT.jar
   ```

## Architecture

The bot is built using functional programming principles with the following components:

### Core Components

- **MagicConchBot**: Main entry point and application setup
- **DiscordClient**: Manages the Discord Gateway WebSocket connection
- **GatewayEventHandler**: Handles incoming Discord events
- **MessageHandler**: Processes text messages and legacy commands
- **SlashCommandManager**: Manages slash command registration and handling
- **VoiceManager**: Manages voice channel connections and music queue
- **VoiceGateway**: Handles Discord voice WebSocket protocol
- **AudioStreamer**: Streams audio to Discord voice channels
- **YouTubeExtractor**: Extracts audio information from YouTube using yt-dlp

### Technology Stack

- **Scala 3.3.0**: Modern Scala with improved syntax
- **Cats Effect 3.6.2**: Functional effects and concurrency
- **FS2**: Functional streaming
- **http4s 0.23.30**: HTTP client for Discord REST API
- **sttp client4**: WebSocket support
- **Circe 0.14.14**: JSON encoding/decoding
- **log4cats**: Functional logging
- **Concentus 1.10**: Pure Java Opus encoder for audio streaming

## Current Limitations

### Known Issues

1. **UDP IP Discovery Incomplete**
   - Voice connection handshake is partially implemented
   - May not work on all network configurations
   - Audio streaming implementation is complete but needs testing

2. **No Playlist Support Yet**
   - Only single YouTube videos are supported
   - Playlist extraction code exists but needs integration

3. **Limited Error Handling**
   - Network failures may cause the bot to disconnect
   - No automatic reconnection logic

4. **Encoder Performance**
   - Opus encoder is created per audio frame (could be cached for better performance)

### To-Do List

- [x] Implement Opus encoding for audio streaming ✓
- [ ] Complete UDP IP discovery protocol
- [ ] Test and debug audio streaming end-to-end
- [ ] Optimize encoder performance (cache encoder instance)
- [ ] Add playlist support
- [ ] Implement automatic reconnection
- [ ] Add volume control
- [ ] Add seek/pause/resume functionality
- [ ] Add support for other audio sources (SoundCloud, Spotify, etc.)
- [ ] Implement music search (YouTube search by keyword)
- [ ] Add user permissions and DJ role
- [ ] Add database for persistent queue storage

## Development

### Project Structure

```
magic-conch-scala/
├── src/main/scala/dev/raegous/magicconch/
│   ├── MagicConchBot.scala          # Entry point
│   ├── DiscordClient.scala          # Gateway WebSocket
│   ├── DiscordApiClient.scala       # REST API client
│   ├── GatewayEventHandler.scala    # Event dispatcher
│   ├── MessageHandler.scala         # Text message handler
│   ├── SlashCommandManager.scala    # Slash command handler
│   ├── VoiceManager.scala           # Voice & queue management
│   ├── VoiceGateway.scala           # Voice WebSocket
│   ├── AudioStreamer.scala          # Audio streaming
│   ├── YouTubeExtractor.scala       # YouTube integration
│   └── DiscordModels.scala          # Data models
├── build.sbt                        # SBT build configuration
├── Dockerfile                       # Docker configuration
└── README.md                        # This file
```

### Building for Production

```bash
# Create executable JAR
sbt assembly

# Run the JAR
java -jar target/scala-3.3.0/magicconch-assembly-0.1.0-SNAPSHOT.jar
```

### Docker Support

```bash
# Build Docker image
docker build -t magic-conch-bot .

# Run with Docker
docker run -e DISCORD_TOKEN="your-token" -e DISCORD_APP_ID="your-app-id" magic-conch-bot
```

## Contributing

Contributions are welcome! Areas that need help:

1. Implementing Opus encoding for audio playback
2. Completing the UDP IP discovery protocol
3. Adding automated tests
4. Improving error handling and resilience
5. Adding new features (playlists, search, etc.)

## Resources

- [Discord Developer Documentation](https://discord.com/developers/docs)
- [Discord Gateway Documentation](https://discord.com/developers/docs/topics/gateway)
- [Discord Voice Connections](https://discord.com/developers/docs/topics/voice-connections)
- [Cats Effect Documentation](https://typelevel.org/cats-effect/)
- [FS2 Documentation](https://fs2.io/)

## License

This project is open source. Feel free to use and modify as needed.

## Acknowledgments

- Inspired by the Magic Conch Shell from SpongeBob SquarePants
- Built with Scala and functional programming principles
- Thanks to the Typelevel community for amazing libraries

---

**Note**: This bot is currently under development. Opus encoding has been implemented using the Concentus library. Audio streaming should work once UDP IP discovery is completed and tested. Contributions are welcome!
