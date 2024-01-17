export const newConnection =
({ url }) => () =>
{
  const client = new WebSocket(url);
  return client;
}

export const onOpen =
k => client => () =>
{
  client.addEventListener('open', () => {
    k();
  });
}

export const onTransmission_f =
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

export const transmit_f =
text => client => () =>
{
  console.log('Sending transmission:', text);
  client.send(text);
}
