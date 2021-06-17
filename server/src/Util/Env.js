exports.getEnv_f =
just => nothing =>
varname =>
() =>
{
  const val = process.env[varname];
  if (typeof val === 'undefined')
    return nothing;
  return just(val);
};
