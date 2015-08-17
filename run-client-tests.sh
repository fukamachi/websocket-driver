#!/bin/sh

node t/server.js &
SERVER_PID=$!

while true; do
    nc -z 127.0.0.1 5000 >/dev/null 2>&1 && break
    sleep 1
done

RESULT=$(ros -l websocket-driver-client.asd -l t/client.lisp 2>/dev/null)
echo "$RESULT"

kill -HUP "$SERVER_PID"

if [ "$RESULT" != "ok" ]; then
    exit 1;
fi
