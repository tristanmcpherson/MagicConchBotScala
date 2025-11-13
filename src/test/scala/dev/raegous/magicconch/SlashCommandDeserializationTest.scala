package dev.raegous.magicconch

import io.circe.parser.*
import io.circe.syntax.*
import DiscordModels.*
import DiscordModels.given

class SlashCommandDeserializationTest extends munit.FunSuite {

  test("deserialize single SlashCommand with null default_member_permissions") {
    val json = """{
      "id": "1130720855517835365",
      "application_id": "295020167732396032",
      "version": "1130720855639474254",
      "default_member_permissions": null,
      "type": 1,
      "name": "dead",
      "description": "Reports the channel's npc as dead",
      "guild_id": "644004109057261631",
      "options": [{
        "type": 3,
        "name": "offset",
        "description": "offset"
      }],
      "nsfw": false
    }"""

    val result = decode[SlashCommand](json)
    result match {
      case Right(cmd) =>
        assertEquals(cmd.name, "dead")
        assertEquals(cmd.default_member_permissions, None)
        assertEquals(cmd.options.map(_.length), Some(1))
      case Left(error) =>
        fail(s"Failed to decode: $error")
    }
  }

  test("deserialize list of SlashCommands with null and missing fields") {
    val json = """[{
      "id": "1130720855517835365",
      "application_id": "295020167732396032",
      "version": "1130720855639474254",
      "default_member_permissions": null,
      "type": 1,
      "name": "dead",
      "description": "Reports the channel's npc as dead",
      "guild_id": "644004109057261631",
      "options": [{
        "type": 3,
        "name": "offset",
        "description": "offset"
      }],
      "nsfw": false
    }, {
      "id": "1130720855517835366",
      "application_id": "295020167732396032",
      "version": "1130720855639474255",
      "default_member_permissions": null,
      "type": 1,
      "name": "checktimers",
      "description": "Checks the timers",
      "guild_id": "644004109057261631",
      "nsfw": false
    }]"""

    val result = decode[List[SlashCommand]](json)
    result match {
      case Right(commands) =>
        assertEquals(commands.length, 2)
        assertEquals(commands.head.name, "dead")
        assertEquals(commands(1).name, "checktimers")
        assert(commands.forall(_.default_member_permissions == None))
      case Left(error) =>
        fail(s"Failed to decode list: $error")
    }
  }

  test("deserialize real SlashCommand array") {
    val json =
      """
        [{"id":"1130720855517835365","application_id":"295020167732396032","version":"1130720855639474254","default_member_permissions":null,"type":1,"name":"dead","description":"Reports the channel's npc as dead","guild_id":"644004109057261631","options":[{"type":3,"name":"offset","description":"offset"}],"nsfw":false},{"id":"1130720855517835366","application_id":"295020167732396032","version":"1130720855639474255","default_member_permissions":null,"type":1,"name":"checktimers","description":"Checks the timers","guild_id":"644004109057261631","nsfw":false},{"id":"1130720855517835367","application_id":"295020167732396032","version":"1130720855639474256","default_member_permissions":null,"type":1,"name":"eval","description":"Evaluates code","guild_id":"644004109057261631","options":[{"type":3,"name":"code","description":"code","required":true}],"nsfw":false},{"id":"1130720855517835368","application_id":"295020167732396032","version":"1130720855719157760","default_member_permissions":null,"type":1,"name":"resume","description":"Plays a song from YouTube or SoundCloud or search for a song on YouTube","guild_id":"644004109057261631","nsfw":false},{"id":"1130720855517835369","application_id":"295020167732396032","version":"1397958109284012225","default_member_permissions":null,"type":1,"name":"play","description":"Play music from a YouTube URL","guild_id":"644004109057261631","options":[{"type":3,"name":"url","description":"YouTube URL to play","required":true}],"nsfw":false},{"id":"1130720855517835370","application_id":"295020167732396032","version":"1397958114715762709","default_member_permissions":null,"type":1,"name":"stop","description":"Stop the current music and clear queue","guild_id":"644004109057261631","nsfw":false},{"id":"1130720855517835371","application_id":"295020167732396032","version":"1130720855719157763","default_member_permissions":null,"type":1,"name":"pause","description":"Pauses the current song.","guild_id":"644004109057261631","nsfw":false},{"id":"1130720855517835372","application_id":"295020167732396032","version":"1397958119262392391","default_member_permissions":null,"type":1,"name":"skip","description":"Skip the current track","guild_id":"644004109057261631","nsfw":false},{"id":"1130720855517835373","application_id":"295020167732396032","version":"1436945877590868019","default_member_permissions":null,"type":1,"name":"volume","description":"Adjust playback volume (0-200%)","guild_id":"644004109057261631","options":[{"type":3,"name":"level","description":"Volume level (0-200, or use buttons)"}],"nsfw":false},{"id":"1130720855517835374","application_id":"295020167732396032","version":"1130720855719157766","default_member_permissions":null,"type":1,"name":"current","description":"Displays the current song","guild_id":"644004109057261631","nsfw":false},{"id":"1130720855639474247","application_id":"295020167732396032","version":"1130720855719157767","default_member_permissions":null,"type":1,"name":"loop","description":"Loops","guild_id":"644004109057261631","nsfw":false},{"id":"1130720855639474248","application_id":"295020167732396032","version":"1130720855719157768","default_member_permissions":null,"type":1,"name":"mp3","description":"Generates a link to the mp3 of the current song playing or the last song played.","guild_id":"644004109057261631","nsfw":false},{"id":"1130720855639474249","application_id":"295020167732396032","version":"1130720855719157769","default_member_permissions":null,"type":1,"name":"changeintro","description":"...","guild_id":"644004109057261631","options":[{"type":3,"name":"file","description":"file","required":true}],"nsfw":false},{"id":"1130720855639474250","application_id":"295020167732396032","version":"1130720855740133426","default_member_permissions":null,"type":1,"name":"why","description":"Why","guild_id":"644004109057261631","nsfw":false},{"id":"1130720855639474251","application_id":"295020167732396032","version":"1130720855740133427","default_member_permissions":null,"type":1,"name":"save","description":"Save a song to a playlist","guild_id":"644004109057261631","options":[{"type":3,"name":"name","description":"name"}],"nsfw":false},{"id":"1130720855639474252","application_id":"295020167732396032","version":"1130720855740133428","default_member_permissions":null,"type":1,"name":"playlist","description":"Playlist commands","guild_id":"644004109057261631","options":[{"type":1,"name":"play","description":"Play a playlist","options":[{"type":3,"name":"name","description":"name"}]},{"type":1,"name":"savequeue","description":"Save queue as a playlist","options":[{"type":3,"name":"name","description":"name"}]},{"type":1,"name":"add","description":"Add a song to playlist by name or url","options":[{"type":3,"name":"song","description":"song","required":true},{"type":3,"name":"name","description":"name"}]},{"type":1,"name":"songs","description":"List songs in playlist","options":[{"type":3,"name":"name","description":"name"}]},{"type":1,"name":"all","description":"Show all playlists"},{"type":1,"name":"delete","description":"Delete playlist with name","options":[{"type":3,"name":"name","description":"name","required":true}]}],"nsfw":false},{"id":"1130720855639474253","application_id":"295020167732396032","version":"1397958124454940896","default_member_permissions":null,"type":1,"name":"queue","description":"Show the current music queue","guild_id":"644004109057261631","nsfw":false},{"id":"1397958129643163650","application_id":"295020167732396032","version":"1397958129643163651","default_member_permissions":null,"type":1,"name":"join","description":"Join your current voice channel","guild_id":"644004109057261631","nsfw":false},{"id":"1397958134693363712","application_id":"295020167732396032","version":"1397958134693363713","default_member_permissions":null,"type":1,"name":"leave","description":"Leave the voice channel","guild_id":"644004109057261631","nsfw":false},{"id":"1397960991198679040","application_id":"295020167732396032","version":"1397960991198679041","default_member_permissions":null,"type":1,"name":"magicconch","description":"Ask the Magic Conch Shell a question","guild_id":"644004109057261631","options":[{"type":3,"name":"question","description":"Your question for the Magic Conch"}],"nsfw":false},{"id":"1434738294981136586","application_id":"295020167732396032","version":"1434738294981136587","default_member_permissions":null,"type":1,"name":"search","description":"Search YouTube for videos","guild_id":"644004109057261631","options":[{"type":3,"name":"query","description":"Search query","required":true}],"nsfw":false},{"id":"1434738296369713244","application_id":"295020167732396032","version":"1434738296369713245","default_member_permissions":null,"type":1,"name":"seek","description":"Seek to a position in the current track","guild_id":"644004109057261631","options":[{"type":3,"name":"position","description":"Position (e.g., 90, 1:30, or 1:23:45)","required":true}],"nsfw":false}]
        """.stripMargin

    val result = decode[List[SlashCommand]](json)
    result match {
      case Right(commands) =>
        assertEquals(commands.length, 22, "Should deserialize all 22 commands")
        assert(commands.forall(_.application_id == "295020167732396032"))
        assert(commands.forall(_.default_member_permissions == None))
        val playlistCmd = commands.find(_.name == "playlist")
        assert(playlistCmd.isDefined, "Should find playlist command")
        assert(playlistCmd.exists(_.options.exists(_.length == 6)), "Playlist should have 6 subcommands")
      case Left(error) =>
        fail(s"Failed to decode real command array: $error")
    }
  }

  test("deserialize SlashCommand with nested options (subcommands)") {
    val json = """{
      "id": "1130720855639474252",
      "application_id": "295020167732396032",
      "version": "1130720855740133428",
      "default_member_permissions": null,
      "type": 1,
      "name": "playlist",
      "description": "Playlist commands",
      "guild_id": "644004109057261631",
      "options": [{
        "type": 1,
        "name": "play",
        "description": "Play a playlist",
        "options": [{
          "type": 3,
          "name": "name",
          "description": "name"
        }]
      }, {
        "type": 1,
        "name": "delete",
        "description": "Delete playlist with name",
        "options": [{
          "type": 3,
          "name": "name",
          "description": "name",
          "required": true
        }]
      }],
      "nsfw": false
    }"""

    val result = decode[SlashCommand](json)
    result match {
      case Right(cmd) =>
        assertEquals(cmd.name, "playlist")
        assertEquals(cmd.options.map(_.length), Some(2))
        assert(cmd.options.exists(_.exists(_.options.isDefined)))
      case Left(error) =>
        fail(s"Failed to decode nested options: $error")
    }
  }
}
