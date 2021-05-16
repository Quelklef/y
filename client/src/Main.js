exports.getHostname =
() =>
{
  return window.location.hostname;
};

exports.initialize_f =
newId => mk2tuple =>
() =>
{
  let uid = localStorage.getItem('uid');
  if (!uid) uid = newId('user');
  localStorage.setItem('uid', uid);

  const cid = new URL(window.location.href).searchParams.get('cid');
  if (!cid) window.location.href += '?cid=' + newId('convo');

  return mk2tuple(uid)(cid);
};
