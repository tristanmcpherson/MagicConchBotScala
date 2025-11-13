import { useState, useEffect } from 'react'
import { dashboardApi } from '../api'
import type { QueueResponse } from '../types'
import NowPlaying from './NowPlaying'
import TrackList from './TrackList'

interface QueueViewProps {
  guildId: string
}

export default function QueueView({ guildId }: QueueViewProps) {
  const [queue, setQueue] = useState<QueueResponse | null>(null)
  const [loading, setLoading] = useState(true)
  const [error, setError] = useState<string>('')

  useEffect(() => {
    loadQueue()
    // Auto-refresh every 5 seconds
    const interval = setInterval(loadQueue, 5000)
    return () => clearInterval(interval)
  }, [guildId])

  const loadQueue = async () => {
    try {
      setError('')
      const response = await dashboardApi.getQueue(guildId)
      setQueue(response.data)
    } catch (err) {
      setError('Failed to load queue')
      console.error(err)
    } finally {
      setLoading(false)
    }
  }

  const handleSkip = async () => {
    try {
      await dashboardApi.skip(guildId)
      await loadQueue()
    } catch (err) {
      alert('Failed to skip track')
      console.error(err)
    }
  }

  const handleStop = async () => {
    if (!confirm('Stop playback and clear queue?')) return

    try {
      await dashboardApi.stop(guildId)
      await loadQueue()
    } catch (err) {
      alert('Failed to stop playback')
      console.error(err)
    }
  }

  const handleRemoveTrack = async (index: number) => {
    try {
      await dashboardApi.removeTrack(guildId, index)
      await loadQueue()
    } catch (err) {
      alert('Failed to remove track')
      console.error(err)
    }
  }

  if (loading) {
    return (
      <div className="card">
        <div className="flex items-center justify-center py-20">
          <div className="animate-spin rounded-full h-12 w-12 border-b-2 border-primary"></div>
        </div>
      </div>
    )
  }

  if (error) {
    return (
      <div className="card">
        <div className="text-center py-20">
          <p className="text-red-500 mb-4">{error}</p>
          <button onClick={loadQueue} className="btn btn-primary">
            Retry
          </button>
        </div>
      </div>
    )
  }

  if (!queue) return null

  return (
    <div className="space-y-6">
      {/* Now Playing */}
      <NowPlaying track={queue.currentTrack} isPlaying={queue.isPlaying} />

      {/* Queue List */}
      <TrackList
        tracks={queue.tracks}
        onRemove={handleRemoveTrack}
      />

      {/* Controls */}
      <div className="card">
        <h3 className="text-lg font-semibold text-gray-800 dark:text-white mb-4">
          Playback Controls
        </h3>
        <div className="flex gap-3">
          <button
            onClick={handleSkip}
            className="btn btn-primary flex items-center gap-2"
            disabled={!queue.currentTrack}
          >
            <span>⏭️</span>
            Skip
          </button>
          <button
            onClick={handleStop}
            className="btn btn-danger flex items-center gap-2"
            disabled={!queue.isPlaying}
          >
            <span>⏹️</span>
            Stop
          </button>
          <button
            onClick={loadQueue}
            className="btn btn-secondary flex items-center gap-2"
          >
            <span>🔄</span>
            Refresh
          </button>
        </div>
      </div>
    </div>
  )
}
