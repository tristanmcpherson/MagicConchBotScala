import { useState, useEffect } from 'react'
import { dashboardApi } from './api'
import type { GuildInfo } from './types'
import GuildList from './components/GuildList'
import QueueView from './components/QueueView'

function App() {
  const [guilds, setGuilds] = useState<GuildInfo[]>([])
  const [selectedGuildId, setSelectedGuildId] = useState<string>('')
  const [loading, setLoading] = useState(true)
  const [error, setError] = useState<string>('')

  useEffect(() => {
    loadGuilds()
  }, [])

  const loadGuilds = async () => {
    try {
      setLoading(true)
      setError('')
      const response = await dashboardApi.getGuilds()
      setGuilds(response.data)

      // Auto-select first guild if available
      if (response.data.length > 0 && !selectedGuildId) {
        setSelectedGuildId(response.data[0].id)
      }
    } catch (err) {
      setError('Failed to load guilds')
      console.error(err)
    } finally {
      setLoading(false)
    }
  }

  return (
    <div className="min-h-screen bg-gradient-to-br from-primary to-secondary">
      <div className="container mx-auto px-4 py-8">
        {/* Header */}
        <header className="bg-white dark:bg-gray-800 rounded-xl shadow-lg p-6 mb-8">
          <div className="flex items-center justify-between">
            <div>
              <h1 className="text-3xl font-bold text-primary flex items-center gap-3">
                <span className="text-4xl">🐚</span>
                Magic Conch Bot Dashboard
              </h1>
              <p className="text-gray-600 dark:text-gray-400 mt-1">
                Monitor and control your music bot
              </p>
            </div>
            <div className="flex items-center gap-2">
              <div className="w-3 h-3 bg-green-500 rounded-full animate-pulse"></div>
              <span className="text-sm font-medium text-gray-700 dark:text-gray-300">Online</span>
            </div>
          </div>
        </header>

        {error && (
          <div className="bg-red-100 border border-red-400 text-red-700 px-4 py-3 rounded-lg mb-6">
            {error}
          </div>
        )}

        {loading ? (
          <div className="flex items-center justify-center py-20">
            <div className="animate-spin rounded-full h-12 w-12 border-b-2 border-white"></div>
          </div>
        ) : (
          <div className="grid grid-cols-1 lg:grid-cols-3 gap-8">
            {/* Guild List Sidebar */}
            <div className="lg:col-span-1">
              <GuildList
                guilds={guilds}
                selectedGuildId={selectedGuildId}
                onSelectGuild={setSelectedGuildId}
                onRefresh={loadGuilds}
              />
            </div>

            {/* Queue View */}
            <div className="lg:col-span-2">
              {selectedGuildId ? (
                <QueueView guildId={selectedGuildId} />
              ) : (
                <div className="card text-center py-20">
                  <p className="text-gray-500 dark:text-gray-400">
                    {guilds.length === 0
                      ? 'No servers found. Make sure the bot is online and in at least one server.'
                      : 'Select a server to view its queue'}
                  </p>
                </div>
              )}
            </div>
          </div>
        )}
      </div>

      {/* Footer */}
      <footer className="text-center text-white py-8 mt-12">
        <p className="opacity-80">
          Magic Conch Bot • Built with Scala 3, http4s & React
        </p>
      </footer>
    </div>
  )
}

export default App
