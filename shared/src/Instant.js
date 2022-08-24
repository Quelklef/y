export const getNow_f =
() =>
{
  return +Date.now();
};

export const pgTimestamptzToMilliseconds_f =
nothing => just =>
str =>
{
  const r = Date.parse(str);
  if (Number.isNaN(r))
    return nothing;
  return just(r);
};

export const millisecondsToPgTimestamptz =
ms =>
{
  return new Date(ms).toISOString().replace('T', ' ').replace('Z', '') + '-00';
};
