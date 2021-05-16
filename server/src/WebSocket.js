const WebSocket = require('ws');

exports.newServer =
({ port }) => () =>
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
