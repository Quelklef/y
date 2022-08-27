export const getWsTarget =
() =>
{
  const isHttps = [...window.location.protocol].reverse()[1] === 's';
  return `${isHttps ? 'wss' : 'ws'}://${window.location.hostname}:8081`;
};

export const localStorage_ = {}

localStorage_.has =
key =>
() =>
{
  return localStorage.getItem(key) !== null;
};

localStorage_.get =
key =>
() =>
{
  return localStorage.getItem(key) || '';
};

localStorage_.set =
key => val =>
() =>
{
  localStorage.setItem(key, val);
};

export const getUrlParams_f =
mk2tuple => mapFromList =>
() =>
{
  const params = Array.from(new URL(window.location.href).searchParams);
  return mapFromList(params.map(([k, v]) => mk2tuple(k)(v)));
};

export const appendUrlParam =
key => val =>
() =>
{
  window.location.href += '?' + key + '=' + val;
};

/* Sometimes, such as when the current document.activeElement is removed
   from the DOM tree, the focus will default to document.body.
   We want to be able to still capture events when the focus has defaulted.
   Unfortunately for us, Elmish doesn't give us the ability to place
   event listeners on the document body.
   The workaround is to redirect the default focus from document.body to
   the actual program root node.
   We assume that the body has at least one element child and that the
   first element child is the root node of the Elmish app
*/
export const workaround_redirectFocusFromBodyToRoot =
() =>
{
  function redirect() {
    if (document.activeElement !== document.body) return;
    const root = document.body.firstElementChild;
    if (!root)
      throw Error('In order to redirect focus from body, expected body to have >=1 child element');
    root.tabIndex = '0';  // fuck
    root.focus();
  }

  document.addEventListener('DOMContentLoaded', () => {
    redirect();
    document.body.addEventListener('focus', () => redirect());
    document.body.addEventListener('focusin', () => setTimeout(redirect, 0));
    document.body.addEventListener('focusout', () => setTimeout(redirect, 0));
  });
}

export const screenDimsMorallySub =
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
