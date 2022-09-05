export const withProp_f =
k => v => o =>
{
  return { ...o, [k]: v };
};
