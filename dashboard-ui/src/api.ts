import axios from 'axios';
import type { GuildInfo, QueueResponse, PlaybackResponse, HealthResponse } from './types';

const API_BASE = '/api';

const api = axios.create({
  baseURL: API_BASE,
  timeout: 10000,
});

export const dashboardApi = {
  // Health check
  health: () => api.get<HealthResponse>('/health'),

  // Guild operations
  getGuilds: () => api.get<GuildInfo[]>('/guilds'),

  // Queue operations
  getQueue: (guildId: string) =>
    api.get<QueueResponse>(`/guilds/${guildId}/queue`),

  getPlayback: (guildId: string) =>
    api.get<PlaybackResponse>(`/guilds/${guildId}/playback`),

  // Playback controls
  skip: (guildId: string) =>
    api.post(`/guilds/${guildId}/skip`),

  stop: (guildId: string) =>
    api.post(`/guilds/${guildId}/stop`),

  removeTrack: (guildId: string, index: number) =>
    api.delete(`/guilds/${guildId}/queue/${index}`),
};

export default api;
