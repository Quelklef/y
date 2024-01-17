export const getTheDimsAndCleanUp =
wrapId =>
() =>
{
  const wrap = document.getElementById(wrapId);
  if (!wrap) throw Error('When calculating dims: wrap not found');


  const err = `Expected exactly 1 child, not ${wrap.childElementCount}.`;
  if (wrap.childElementCount === 0) throw Error(err);
  if (wrap.childElementCount !== 1) console.warn(err);
  const target = wrap.firstChild;

  const dims = { width: target.offsetWidth, height: target.offsetHeight };

  wrap.remove();

  return dims;
}
