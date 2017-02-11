#!/bin/sh

ros -l websocket-driver-server.asd -l examples/echo-server.lisp &
SERVER_PID=$!

count=0
while true; do
    nc -z 127.0.0.1 5000 >/dev/null 2>&1 && break
    sleep 1
    count=$(( count + 1 ))
    if [ 60 -lt $count ]; then
        echo "Too long time took to start a server."
        exit 1;
    fi
done

RESULT=$(node t/client.js)
echo "$RESULT"

kill -HUP "$SERVER_PID"

if [ "$RESULT" != "ok" ]; then
    exit 1;
fi
