exports.getNow =
() =>
{
  return BigInt(Date.now());
};

exports.getRand =
max =>
() =>
{
  // NOTE: this will mangle high enough values of 'max'
  return BigInt(Math.floor(Math.random() * Number(max)));
};
