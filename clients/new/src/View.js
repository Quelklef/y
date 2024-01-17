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

export const getTargetValue =
event => () => {
  return event.target.value;
};

export const getKeyInfo =
event => () => {
  if (!(event instanceof KeyboardEvent)) throw "No";
  return {
    key: event.key,
    ctrl: event.ctrlKey,
    shift: event.shiftKey,
  };
};

export const preventDefault =
ev => () => {
  ev.preventDefault();
};
