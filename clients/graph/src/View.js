export const getTimestampPretty =
() =>
{
  return (
    new Date()
      .toISOString()
      .replace(/[T:\.]/g, '-')
      .replace('Z', '-UTC')
  );
};
