exports.getHostname =
() =>
{
  return window.location.hostname;
};

exports.initialize_f =
mk2Tuple =>
freshUid => freshCid =>
() =>
{
  let userId = localStorage.getItem('userId');
  if (!userId) userId = freshUid
  localStorage.setItem('userId', userId);

  const convoId = new URL(window.location.href).searchParams.get('convo');
  if (!convoId) window.location.href += '?convo=' + freshCid;

  return mk2Tuple(userId)(convoId);
};
