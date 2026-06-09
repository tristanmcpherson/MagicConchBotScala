package dev.raegous.magicconch.guilds

import cats.effect.*
import cats.implicits.*
import org.typelevel.log4cats.Logger

/** Tracks guilds (Discord servers) that the bot is a member of. Used by the
  * dashboard to display available servers.
  */
object GuildTracker {
  def make[F[_]: Async](using Logger[F]): Resource[F, GuildTracker[F]] = {
    Resource.eval {
      Ref.of[F, Map[String, GuildTracker.TrackedGuild]](Map.empty).map {
        guildsRef =>
          new GuildTracker[F](guildsRef)
      }
    }
  }

  case class TrackedGuild(
      id: String,
      name: String,
      memberCount: Int
  )
}

class GuildTracker[F[_]: Async] private (
    private val guildsRef: Ref[F, Map[String, GuildTracker.TrackedGuild]]
)(using Logger[F]) {

  import GuildTracker.TrackedGuild

  /** Add or update a guild in the tracker.
    */
  def addGuild(id: String, name: String, memberCount: Int): F[Unit] = {
    val guild = TrackedGuild(id, name, memberCount)
    guildsRef.update(_ + (id -> guild)) >>
      Logger[F].debug(s"[GUILD_TRACKER] Added guild: $name ($id)")
  }

  /** Remove a guild from the tracker.
    */
  def removeGuild(id: String): F[Unit] = {
    guildsRef.update(_ - id) >>
      Logger[F].debug(s"[GUILD_TRACKER] Removed guild: $id")
  }

  /** Get all tracked guilds.
    */
  def getAllGuilds: F[List[TrackedGuild]] = {
    guildsRef.get.map(_.values.toList.sortBy(_.name))
  }

  /** Get a specific guild by ID.
    */
  def getGuild(id: String): F[Option[TrackedGuild]] = {
    guildsRef.get.map(_.get(id))
  }

  /** Check if a guild is tracked.
    */
  def hasGuild(id: String): F[Boolean] = {
    guildsRef.get.map(_.contains(id))
  }

  /** Get the number of tracked guilds.
    */
  def getGuildCount: F[Int] = {
    guildsRef.get.map(_.size)
  }
}
