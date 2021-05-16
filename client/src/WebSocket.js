exports.newConnection =
({ url }) => () =>
{
  const client = new WebSocket(url);
  return client;
}

exports.onOpen =
k => client => () =>
{
  client.addEventListener('open', () => {
    k();
  });
}

exports.onTransmission_f =
decode =>
k => client => () =>
{
  client.addEventListener('message', event => {
    const text = event.data.toString();
    console.log('Transmission received:', text);
    const val = decode(text);
    k(val)();
  });
}

exports.transmit_f =
text => client => () =>
{
  console.log('Sending transmission:', text);
  client.send(text);
}
