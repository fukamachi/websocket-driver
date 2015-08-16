#!/bin/sh

ros -l websocket-driver-server.asd -l examples/echo-server.lisp &
SERVER_PID=$!

while true; do
    nc -z 127.0.0.1 5000 >/dev/null 2>&1 && break
    sleep 1
done

RESULT=$(node t/client.js)
echo "$RESULT"

kill "$SERVER_PID"

if [ "$RESULT" != "ok" ]; then
    exit 1;
fi
