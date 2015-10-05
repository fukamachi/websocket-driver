var WebSocket = require('ws'),
    ws = new WebSocket('ws://localhost:5000/echo');

var _output = '';
ws.on('message', function(message) {
    console.error('received: %s', message);
    _output = _output + message;
});
var getOutput = function() {
    var out = _output;
    _output = '';
    return out;
};

ws.on('open', function() {
    ws.send('Hi');
    setTimeout(function() {
        var output = getOutput();
        if (output === 'Hi') {
            console.log('ok');
            process.exit();
        }
        else {
            console.log('ng: "%s"', output);
            process.exit(1);
        }
    }, 300);
});

setTimeout(function() {
    console.error('Timeout.');
    process.exit(1);
}, 3000);
