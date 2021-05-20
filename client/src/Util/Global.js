const vals = {};

exports.global =
key => mk =>
{
  if (key in vals)
    return vals[key];

  const val = mk();
  vals[key] = val;
  return val;
}
