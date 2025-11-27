 Refactoring Standards \& Practices



  1. No if/else statements - Always use Option.\* for boolean conditionals in functional Scala, or figure out ways to use .filter, etc, instead

  // Bad

  if (condition) valueA else valueB



  2. No isDefined checks - Use proper Option handling with .fold(), .traverse(), or monadic composition

  3. Reduce nesting with monads - Use OptionT and EitherT monad transformers instead of nested pattern matching

  // Instead of nested matches, use OptionT

  val result = for {

    ws <- OptionT.fromOption\[F](context.gatewayWs)

    channelId <- OptionT(voiceManager.getUserVoiceChannel(userId))

    \_ <- OptionT.liftF(someAction)

  } yield ()

  4. Extract methods to reduce nesting - Maximum 2 levels of nesting; extract complex logic into smaller, named

  methods

  5. Use .fold() everywhere - For Options, Eithers, and any sum types instead of pattern matching when possible



  Testing with Smockito



  - Parameter patterns in mocks must match arity: (\_, \_) for 2 params, (\_, \_, \_) for 3 params

  - Use .on(it.methodName) syntax correctly

  - Avoid verify() calls that cause type inference issues



  Project-Specific



  - VoiceManager moved to dev.raegous.magicconch.audio.VoiceManager

  - GuildSettingsManager moved to dev.raegous.magicconch.guilds.GuildSettingsManager

  - Always check imports when refactoring - linters may auto-fix package locations

  - Use MCP tools for compilation/testing, NOT coursier/sbt directly

