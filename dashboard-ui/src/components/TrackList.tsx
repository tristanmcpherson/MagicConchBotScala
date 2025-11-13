import type { QueueTrack } from '../types'

interface TrackListProps {
  tracks: QueueTrack[]
  onRemove: (index: number) => void
}

function formatDuration(seconds: number | null): string {
  if (!seconds) return '?:??'
  const mins = Math.floor(seconds / 60)
  const secs = seconds % 60
  return `${mins}:${secs.toString().padStart(2, '0')}`
}

export default function TrackList({ tracks, onRemove }: TrackListProps) {
  if (tracks.length === 0) {
    return (
      <div className="card">
        <h3 className="text-lg font-semibold text-gray-800 dark:text-white mb-4">
          Queue ({tracks.length})
        </h3>
        <p className="text-gray-500 dark:text-gray-400 text-center py-8">
          Queue is empty
        </p>
      </div>
    )
  }

  return (
    <div className="card">
      <h3 className="text-lg font-semibold text-gray-800 dark:text-white mb-4">
        Queue ({tracks.length})
      </h3>

      <div className="space-y-2">
        {tracks.map((track, index) => (
          <div
            key={index}
            className="flex items-center justify-between p-4 bg-gray-50 dark:bg-gray-700 rounded-lg hover:bg-gray-100 dark:hover:bg-gray-600 transition-colors"
          >
            <div className="flex-1 min-w-0">
              <div className="flex items-baseline gap-2">
                <span className="text-sm font-semibold text-gray-500 dark:text-gray-400">
                  {index + 1}.
                </span>
                <h4 className="font-medium text-gray-800 dark:text-white truncate">
                  {track.title}
                </h4>
              </div>
              <div className="flex items-center gap-3 mt-1 text-sm text-gray-600 dark:text-gray-400">
                <span>By: {track.requestedBy}</span>
                <span>•</span>
                <span>{formatDuration(track.duration)}</span>
              </div>
            </div>

            <button
              onClick={() => onRemove(index)}
              className="ml-4 text-red-500 hover:text-red-600 transition-colors p-2"
              title="Remove track"
            >
              <svg
                xmlns="http://www.w3.org/2000/svg"
                className="h-5 w-5"
                viewBox="0 0 20 20"
                fill="currentColor"
              >
                <path
                  fillRule="evenodd"
                  d="M4.293 4.293a1 1 0 011.414 0L10 8.586l4.293-4.293a1 1 0 111.414 1.414L11.414 10l4.293 4.293a1 1 0 01-1.414 1.414L10 11.414l-4.293 4.293a1 1 0 01-1.414-1.414L8.586 10 4.293 5.707a1 1 0 010-1.414z"
                  clipRule="evenodd"
                />
              </svg>
            </button>
          </div>
        ))}
      </div>
    </div>
  )
}
