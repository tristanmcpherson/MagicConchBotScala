package dev.raegous.magicconch.audio.internals

import cats.effect.*
import cats.effect.std.Queue
import cats.implicits.*

/** Generic buffer pool for reducing GC pressure in audio streaming.
  *
  * Uses cats-effect Resource pattern for safe buffer acquisition/release.
  * Buffers are pre-allocated and reused across audio frames.
  *
  * @param poolSize
  *   Number of buffers to pre-allocate
  * @param create
  *   Function to create a new buffer
  * @param reset
  *   Optional function to reset buffer state before reuse
  */
class BufferPool[F[_]: Async, A] private (
    queue: Queue[F, A],
    reset: A => F[Unit]
) {

  /** Acquire a buffer from the pool (or create new if pool is empty). Returns a
    * Resource that automatically returns the buffer to the pool.
    */
  def acquire: Resource[F, A] = {
    Resource.make(
      queue.tryTake.flatMap {
        case Some(buffer) =>
          reset(buffer).as(buffer)
        case None =>
          // Pool exhausted - this is OK, buffer will be added back when released
          Async[F].raiseError(
            new RuntimeException("Buffer pool exhausted - should not happen")
          )
      }
    )(buffer => queue.offer(buffer).void)
  }

  /** Acquire a buffer, use it, then return it to the pool. The user function
    * receives the buffer and must complete without storing it.
    */
  def use[B](f: A => F[B]): F[B] = {
    acquire.use(f)
  }
}

object BufferPool {

  /** Create a buffer pool with pre-allocated buffers.
    *
    * @param poolSize
    *   Number of buffers to pre-allocate
    * @param create
    *   Function to create a new buffer
    * @param reset
    *   Function to reset buffer state before reuse
    */
  def make[F[_]: Async, A](
      poolSize: Int,
      create: F[A],
      reset: A => F[Unit]
  ): Resource[F, BufferPool[F, A]] = {
    Resource.eval {
      for {
        queue <- Queue.bounded[F, A](poolSize)
        // Pre-allocate all buffers
        _ <- (1 to poolSize).toList.traverse_ { _ =>
          create.flatMap(queue.offer)
        }
      } yield new BufferPool[F, A](queue, reset)
    }
  }

  /** Create a buffer pool with no reset (for immutable buffers or when reset
    * not needed).
    */
  def makeNoReset[F[_]: Async, A](
      poolSize: Int,
      create: F[A]
  ): Resource[F, BufferPool[F, A]] = {
    make(poolSize, create, (_: A) => Async[F].unit)
  }

  /** Create a pool of byte arrays (most common use case).
    */
  def bytes[F[_]: Async](
      poolSize: Int,
      bufferSize: Int
  ): Resource[F, BufferPool[F, Array[Byte]]] = {
    make(
      poolSize,
      create = Async[F].delay(new Array[Byte](bufferSize)),
      reset = buffer => Async[F].delay(java.util.Arrays.fill(buffer, 0.toByte))
    )
  }

  /** Create a pool of short arrays (for PCM audio data).
    */
  def shorts[F[_]: Async](
      poolSize: Int,
      bufferSize: Int
  ): Resource[F, BufferPool[F, Array[Short]]] = {
    make(
      poolSize,
      create = Async[F].delay(new Array[Short](bufferSize)),
      reset = buffer => Async[F].delay(java.util.Arrays.fill(buffer, 0.toShort))
    )
  }
}
