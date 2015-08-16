var WebSocketServer = require('ws').Server,
    wss = new WebSocketServer({ path: '/echo', port: 5000 });

wss.on('connection', function(ws) {
    console.log('connected');
    ws.on('message', function(message) {
        console.log('received: %s', message);
        ws.send(message);
        console.log('send: %s', message);
    });
});
