var WebSocket = require('ws'),
    ws = new WebSocket('ws://localhost:5000/echo');

var _output = '';
ws.on('message', function(message) {
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
        if (getOutput() === 'Hi') {
            console.log('ok');
            process.exit();
        }
        else {
            console.log('ng');
            process.exit(1);
        }
    }, 300);
});

setTimeout(function() {
    console.log('Timeout.');
    process.exit(1);
}, 3000);