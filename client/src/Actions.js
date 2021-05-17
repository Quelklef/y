exports.setTimeout0 =
f =>
() =>
{
  setTimeout(f, 0);
};

exports.focusElementById =
id =>
() =>
{
  const el = document.getElementById(id);
  if (el) el.focus();
};
