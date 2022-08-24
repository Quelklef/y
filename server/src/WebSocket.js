import WebSocket from 'ws';
import https from 'https';
import fs from 'fs';

export const newServer_https =
({ sslInfo, port }) =>
() =>
{
  const server = https.createServer({ cert: sslInfo.cert, key: sslInfo.key });
  const wss = new WebSocket.Server({ server });
  server.listen(port);
  return wss;
};

export const newServer_http =
({ port }) =>
() =>
{
  return new WebSocket.Server({ port });
};

export const onConnection =
k => server => () =>
{
  server.on('connection', client => {
    k(client)();
  });
};

export const onTransmission_f =
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

export const onClose =
k => client => () =>
{
  client.on('close', () => {
    k();
  });
}

export const transmit_f =
text => client => () =>
{
  console.log('Sending transmission:', text);
  client.send(text);
}
