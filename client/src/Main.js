exports.getWsTarget =
() =>
{
  const isHttps = [...window.location.protocol].reverse()[1] === 's';
  return `${isHttps ? 'wss' : 'ws'}://${window.location.hostname}:8081`;
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

/* Sometimes, such as when the current document.activeElement is removed
   from the DOM tree, the focus will default to document.body.
   We want to be able to still capture events when the focus has defaulted.
   Unfortunately for us, Elmish doesn't give us the ability to place
   event listeners on the document body.
   The workaround is to redirect the default focus from document.body to
   the actual program root node.
   We assume that the body has only one element and that that element is
   the root node of the program
*/
exports.workaround_redirectFocusFromBodyToRoot =
() =>
{
  function redirect() {
    if (document.activeElement !== document.body) return;

    const err = `In order to redirect focus from body, expected body to have 1 child node, not ${document.body.childNodes.length}`;
    if (document.body.childNodes.length === 0) throw Error(err);
    if (document.body.childNodes.length !== 1) console.warn(err);

    const root = document.body.childNodes[0];
    root.tabIndex = '0';  // fuck
    if (root) root.focus();
  }

  document.addEventListener('DOMContentLoaded', () => {
    redirect();
    document.body.addEventListener('focus', () => redirect());
    document.body.addEventListener('focusin', () => setTimeout(redirect, 0));
    document.body.addEventListener('focusout', () => setTimeout(redirect, 0));
  });
}

exports.screenDimsMorallySub =
send =>
() =>
{
  function notify() {
    send({
      width: window.innerWidth,
      height: window.innerHeight,
    })();
  }

  window.addEventListener('DOMContentLoaded', notify);
  window.addEventListener('resize', notify);

  function cancel() {
    window.removeEventListener('DOMContentLoaded', notify);
    window.removeEventListener('resize', notify);
  }

  return cancel;
};
