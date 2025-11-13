import type { GuildInfo } from '../types'

interface GuildListProps {
  guilds: GuildInfo[]
  selectedGuildId: string
  onSelectGuild: (guildId: string) => void
  onRefresh: () => void
}

export default function GuildList({
  guilds,
  selectedGuildId,
  onSelectGuild,
  onRefresh,
}: GuildListProps) {
  return (
    <div className="card">
      <div className="flex items-center justify-between mb-4">
        <h2 className="text-xl font-bold text-gray-800 dark:text-white">Servers</h2>
        <button
          onClick={onRefresh}
          className="btn btn-secondary text-sm"
          title="Refresh server list"
        >
          🔄 Refresh
        </button>
      </div>

      {guilds.length === 0 ? (
        <p className="text-gray-500 dark:text-gray-400 text-center py-8">
          No servers found
        </p>
      ) : (
        <div className="space-y-3">
          {guilds.map((guild) => (
            <button
              key={guild.id}
              onClick={() => onSelectGuild(guild.id)}
              className={`
                w-full text-left p-4 rounded-lg border-2 transition-all
                ${
                  selectedGuildId === guild.id
                    ? 'border-primary bg-primary bg-opacity-10'
                    : 'border-gray-200 dark:border-gray-700 hover:border-primary'
                }
              `}
            >
              <div className="flex items-start justify-between">
                <div className="flex-1">
                  <h3 className="font-semibold text-gray-800 dark:text-white">
                    {guild.name}
                  </h3>
                  <p className="text-sm text-gray-600 dark:text-gray-400 mt-1">
                    {guild.queueSize} {guild.queueSize === 1 ? 'track' : 'tracks'}
                  </p>
                </div>
                {guild.isPlaying && (
                  <span className="text-2xl" title="Playing">
                    ▶️
                  </span>
                )}
              </div>
            </button>
          ))}
        </div>
      )}
    </div>
  )
}
