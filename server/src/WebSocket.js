const WebSocket = require('ws');
const https = require('https');
const fs = require('fs');

exports.newServer_https =
({ sslInfo, port }) =>
() =>
{
  const server = https.createServer({ cert: sslInfo.cert, key: sslInfo.key });
  const wss = new WebSocket.Server({ server });
  server.listen(port);
  return wss;
};

exports.newServer_http =
({ port }) =>
() =>
{
  return new WebSocket.Server({ port });
};

exports.onConnection =
k => server => () =>
{
  server.on('connection', client => {
    k(client)();
  });
};

exports.onTransmission_f =
decode =>
k => client => () =>
{
  client.on('message', data => {
    const text = data.toString();
    console.log('Transmission received:', text);
    const val = decode(text);
    k(val)();
  });
}

exports.onClose =
k => client => () =>
{
  client.on('close', () => {
    k();
  });
}

exports.transmit_f =
text => client => () =>
{
  console.log('Sending transmission:', text);
  client.send(text);
}
