const vals = {};

export const global_f =
key => mk =>
{
  if (key in vals)
    return vals[key];

  const val = mk();
  vals[key] = val;
  return val;
}
