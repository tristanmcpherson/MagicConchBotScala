import type { QueueTrack } from '../types'

interface NowPlayingProps {
  track: QueueTrack | null
  isPlaying: boolean
}

function formatDuration(seconds: number | null): string {
  if (!seconds) return '?:??'
  const mins = Math.floor(seconds / 60)
  const secs = seconds % 60
  return `${mins}:${secs.toString().padStart(2, '0')}`
}

export default function NowPlaying({ track, isPlaying }: NowPlayingProps) {
  if (!track) {
    return (
      <div className="card bg-gradient-to-r from-gray-500 to-gray-600 text-white">
        <h2 className="text-2xl font-bold mb-4 flex items-center gap-2">
          <span>🎵</span>
          Now Playing
        </h2>
        <p className="text-gray-200 opacity-75">Nothing playing</p>
      </div>
    )
  }

  return (
    <div className="card bg-gradient-to-r from-primary to-secondary text-white">
      <div className="flex items-start justify-between mb-4">
        <h2 className="text-2xl font-bold flex items-center gap-2">
          <span>{isPlaying ? '▶️' : '⏸️'}</span>
          Now Playing
        </h2>
        <span className="text-sm opacity-75">
          {formatDuration(track.duration)}
        </span>
      </div>

      <div className="space-y-2">
        <h3 className="text-xl font-semibold">{track.title}</h3>
        <p className="text-sm opacity-90">
          Requested by: <span className="font-medium">{track.requestedBy}</span>
        </p>
        <a
          href={track.url}
          target="_blank"
          rel="noopener noreferrer"
          className="text-sm underline opacity-75 hover:opacity-100 transition-opacity inline-block mt-2"
        >
          View on YouTube →
        </a>
      </div>
    </div>
  )
}
