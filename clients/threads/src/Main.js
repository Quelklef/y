export const getHostname = () => window.location.hostname;

export const initialize_f = mk2Tuple => freshUid => freshCid => () => {
  let userId = localStorage.getItem('userId');
  if (!userId) userId = freshUid
  localStorage.setItem('userId', userId);

  const convoId = new URL(window.location.href).searchParams.get('convo');
  if (!convoId) window.location.href += '?convo=' + freshCid;

  return mk2Tuple(userId)(convoId);
};

export const dateString = ms => new Date(ms)

export const sendNotification = playSound => soundUrl => person => message => () => {
	if (!document.hasFocus()) {
		if (playSound) new Audio(soundUrl).play();

		new Notification(`⅄`, {body: person + ":\n" + message})
			.onclick = function() {focus(window); this.close()};
	}
};

export const notificationsPermission =
	() => Notification.requestPermission();

export const isSelecting = () => getSelection().type === `Range`;

export const hasFocus = () => document.hasFocus();

export const setItem = key => value => () => localStorage[key] = value;

export const getItemImpl = Nothing => Just => key => () => {
	const x = localStorage[key];
	return x === undefined ? Nothing : Just(x);
};
