exports.add_f = b1 => b2 => b1 + b2;
exports.zero_f = 0n;
exports.mul_f = b1 => b2 => b1 * b2;
exports.one_f = 1n;
exports.sub_f = b1 => b2 => b1 - b2;
exports.degree_f = b => Number(b);
exports.div_f = b1 => b2 => b1 / b2;
exports.mod_f = b1 => b2 => b1 % b2;

exports.eq_f = b1 => b2 => b1 === b2;

exports.compare_f =
lt => eq => gt =>
b1 => b2 =>
{
  if (b1 < b2) return lt;
  if (b1 > b2) return gt;
  return eq;
};

exports.pow = b1 => b2 => eval('b1 ** b2');  // I cry

exports.toNumber = b => Number(b);
exports.fromNumber_f =
nothing => just =>
n =>
{
  if (Math.trunc(n) !== n)
    return nothing;
  return just(BigInt(n));
};
exports.fromInt = n => BigInt(n);
exports.toInt = b => Number(b);
