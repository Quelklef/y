export const add_f = b1 => b2 => b1 + b2;
export const zero_f = 0n;
export const mul_f = b1 => b2 => b1 * b2;
export const one_f = 1n;
export const sub_f = b1 => b2 => b1 - b2;
export const degree_f = b => Number(b);
export const div_f = b1 => b2 => b1 / b2;
export const mod_f = b1 => b2 => b1 % b2;

export const eq_f = b1 => b2 => b1 === b2;

export const compare_f =
lt => eq => gt =>
b1 => b2 =>
{
  if (b1 < b2) return lt;
  if (b1 > b2) return gt;
  return eq;
};

export const pow = b1 => b2 => b1 ** b2;

export const toNumber = b => Number(b);
export const fromNumber_f =
nothing => just =>
n =>
{
  if (Math.trunc(n) !== n)
    return nothing;
  return just(BigInt(n));
};
export const fromInt = n => BigInt(n);
export const toInt = b => Number(b);

export const show_f = n => String(n) + 'n';
