exports.getHostname =
() =>
{
  return window.location.hostname;
};

exports.initialize_f =
mk2tuple =>
freshUid => freshCid => () =>
{
  let uid = localStorage.getItem('uid');
  if (!uid) uid = freshUid
  localStorage.setItem('uid', uid);

  const cid = new URL(window.location.href).searchParams.get('cid');
  if (!cid) window.location.href += '?cid=' + freshCid;

  return mk2tuple(uid)(cid);
};
