# Magic Conch Dashboard

Modern React + TypeScript dashboard for the Magic Conch Discord Music Bot.

## Features

- ✅ View all servers the bot is in
- ✅ Real-time queue display (auto-refresh every 5 seconds)
- ✅ Now playing information
- ✅ Playback controls (skip, stop)
- ✅ Remove tracks from queue
- ✅ Beautiful gradient UI with Tailwind CSS
- ✅ Responsive design
- ✅ TypeScript for type safety

## Prerequisites

- Node.js 18+
- npm or yarn
- Magic Conch backend running on http://localhost:8080

## Installation

```bash
# Install dependencies
npm install
```

## Development

```bash
# Start development server (runs on http://localhost:3000)
npm run dev
```

The Vite dev server includes a proxy that forwards `/api` requests to `http://localhost:8080`, so you don't need to worry about CORS during development.

## Building for Production

```bash
# Build for production
npm run build

# Preview production build
npm run preview
```

The built files will be in the `dist/` directory.

## Project Structure

```
dashboard-ui/
├── src/
│   ├── components/       # React components
│   │   ├── GuildList.tsx
│   │   ├── QueueView.tsx
│   │   ├── NowPlaying.tsx
│   │   └── TrackList.tsx
│   ├── api.ts           # API client
│   ├── types.ts         # TypeScript types
│   ├── App.tsx          # Main app component
│   ├── main.tsx         # Entry point
│   └── index.css        # Global styles
├── index.html           # HTML template
├── package.json
├── tsconfig.json
├── vite.config.ts
└── tailwind.config.js
```

## API Integration

The dashboard communicates with the Scala backend via REST API:

- `GET /api/health` - Health check
- `GET /api/guilds` - List all guilds
- `GET /api/guilds/:id/queue` - Get queue
- `POST /api/guilds/:id/skip` - Skip track
- `POST /api/guilds/:id/stop` - Stop playback
- `DELETE /api/guilds/:id/queue/:index` - Remove track

## Tech Stack

- **React 18** - UI framework
- **TypeScript** - Type safety
- **Vite** - Build tool & dev server
- **Tailwind CSS** - Styling
- **Axios** - HTTP client

## License

MIT
