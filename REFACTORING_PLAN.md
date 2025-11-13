# VoiceManager Refactoring Plan

## Summary
VoiceManager has been refactored to delegate music and search responsibilities to dedicated services.

## New Architecture

```
VoiceManager (voice connections only)
├── MusicService (injected dependency)
│   ├── QueueManager
│   ├── PlaybackController
│   └── TrackExtractor
└── SearchService (injected dependency)
    ├── YouTubeSearchClient
    └── SearchResultCache
```

## Method Migration Map

### ✅ Moved to MusicService
- `addToQueue()` → `musicService.addToQueue()`
- `getQueue()` → `musicService.getQueue()`
- `clearQueue()` → `musicService.clearQueue()`
- `playNext()` → `musicService.queueManager.playNext()`
- **`addPlaylist()`** → **`musicService.addPlaylist()`** ⭐
- `extractAudioFromYoutube()` → `musicService.extractTrackInfo()`
- `getStreamUrl()` → `musicService.trackExtractor.getStreamUrl()`
- `startPlayingCurrent()` → `musicService.startPlayingCurrent()`
- `playNextTrack()` → `musicService.playNextTrack()`
- `pausePlayback()` → `musicService.pausePlayback()`
- `resumePlayback()` → `musicService.resumePlayback()`
- `skipTrack()` → `musicService.skipTrack()`
- `stopPlayback()` → `musicService.stopPlayback()`
- `stopMusic()` → `musicService.stopMusic()`
- `seek()` → `musicService.seek()`
- `isPaused()` → `musicService.isPaused()`
- `getCurrentPosition()` → `musicService.getCurrentPosition()`

### ✅ Moved to SearchService
- `storeSearchResults()` → `searchService.storeResults()`
- `getSearchResults()` → `searchService.getResults()`
- `clearSearchResults()` → `searchService.clearResults()`

### ✅ Kept in VoiceManager (voice-specific)
- `handleVoiceStateUpdate()`
- `handleVoiceServerUpdate()`
- `attemptVoiceConnection()`
- `joinVoiceChannel()`
- `leaveVoiceChannel()`
- `waitForVoiceConnection()`
- `connectToVoiceChannel()`
- `getUserVoiceChannel()`

## State Migration

### ✅ Removed from VoiceManager
- `musicQueueRef` → moved to QueueManager
- `searchResultsRef` → moved to SearchResultCache
- `youtubeExtractor` → moved to TrackExtractor
- `audioSourceManager` → moved to TrackExtractor

### ✅ Kept in VoiceManager
- `voiceStateRef`
- `botUserIdRef`
- `userVoiceStatesRef`
- `activeVoiceConnections`
- `gatewayWebSocketRef`
- `pendingVoiceConnections`
- `pendingConnectionPromises`
- `activePlaybackFibers` (shared with PlaybackController)

## Dependencies

### VoiceManager now depends on:
- `MusicService` (for all music operations)
- `SearchService` (for search operations)
- `VoiceGateway` (unchanged)
- `AudioStreamer` (unchanged)

## Commands Update

Commands should now call:
- **Music operations**: `musicService.xxx()` instead of `voiceManager.xxx()`
- **Search operations**: `searchService.xxx()` instead of `voiceManager.xxx()`
- **Voice operations**: `voiceManager.xxx()` (unchanged)

## Key Achievement

**`addPlaylist()` is now properly located in `MusicService`** where it belongs, orchestrating:
1. Track extraction from playlist URLs
2. Queue management
3. Music coordination

This fixes the original separation of concerns issue!
