#!/usr/bin/env python3
import websocket
import json
import threading
import time

# Timeout máximo para no colgar Waybar
TIMEOUT = 1.0

def get_track():
    track_info = ""

    try:
        ws = websocket.create_connection("ws://127.0.0.1:10767", timeout=TIMEOUT)
        # Escuchar mensajes por TIMEOUT segundos
        start = time.time()
        while time.time() - start < TIMEOUT:
            try:
                msg = ws.recv()
                data = json.loads(msg)
                # Buscar keys comunes para track
                track = data.get("track") or data.get("title")
                artist = data.get("artist")
                if track and artist:
                    track_info = f"🎵 {track} - {artist}"
                    break
            except websocket.WebSocketTimeoutException:
                break
            except json.JSONDecodeError:
                continue
        ws.close()
    except Exception:
        pass

    return track_info

if __name__ == "__main__":
    print(get_track())

