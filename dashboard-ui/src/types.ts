// API Response Types matching the Scala backend

export interface GuildInfo {
  id: string;
  name: string;
  queueSize: number;
  isPlaying: boolean;
}

export interface QueueTrack {
  title: string;
  url: string;
  duration: number | null;
  requestedBy: string;
}

export interface QueueResponse {
  tracks: QueueTrack[];
  currentTrack: QueueTrack | null;
  isPlaying: boolean;
}

export interface PlaybackResponse {
  currentTrack: QueueTrack | null;
  isPlaying: boolean;
  queueLength: number;
}

export interface HealthResponse {
  status: string;
  uptime: number;
}

export interface ErrorResponse {
  error: string;
}
