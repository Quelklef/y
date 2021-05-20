exports.createOffPageElement =
() => {
  const el = document.createElement('div');
  el.classList.add('__super_secret_offpage_element');
  el.style.position = 'absolute';
  el.style.top = '10000px';
  el.style.left = '10000px';
  el.style.display = 'contents';
  return el;
};

exports.deleteElement =
el => () =>
{
  el.remove();
};

exports.getElementDims =
el => () =>
{
  const width = el.offsetWidth;
  const height = el.offsetHeight;
  return { width, height };
};

exports.getOnlyChildOfElement =
el => () =>
{
  console.log(el);
  const err = `Expected exactly 1 child, not ${el.childElementCount}.`;
  if (el.childElementCount === 0) throw Error(err);
  if (el.childElementCount !== 1) console.warn(err);
  return el.firstChild;
};
