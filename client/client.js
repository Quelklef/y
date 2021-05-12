document.addEventListener('DOMContentLoaded', function() {

// Initialization

function mkYid() {
  const t = Date.now() + '';
  const r = (Math.floor(Math.random() * 1e6) + '').padStart(6, '0')
  return t + '-' + r;
}

const uyid = localStorage.getItem('uyid') ?? mkYid();
localStorage.setItem('uyid', uyid);

const cyid = new URL(window.location.href).searchParams.get('cyid');
if (!cyid) window.location.href += '?cyid=' + mkYid();


// State

const sounds = [];

const anchored = [];
let focused = null;


// Communication

const ws = new WebSocket('ws://' + location.hostname + ':8080');

ws.addEventListener('open', function() {
  ws.send(JSON.stringify({ kind: 'init', cyid, uyid }));
});

ws.addEventListener('message', function(event) {
  const newSounds = JSON.parse(event.data);
  for (const newSound of newSounds) {
    console.debug('Sound received:', newSound);
    const alreadyExists = sounds.some(sound => sound.syid === newSound.syid);
    if (!alreadyExists) sounds.push(newSound);
  }
  if (focused === null && newSounds.length > 0) {
    focused = newSounds[0].syid;
  }
  render();
});


// Display

const $song = document.getElementById('song');

function render() {
  for (const sound of sounds)
    displaySound(sound);

  organize();
  drawArrows();
}

function displaySound(sound) {
  let node  = document.getElementById('sound-' + sound.syid);
  if (!node) node = mkSound(sound);

  if (anchored.includes(sound.syid))
    node.classList.add('anchored');
  else
    node.classList.remove('anchored');

  if (focused === sound.syid)
    node.classList.add('focused');
  else
    node.classList.remove('focused');
}

function mkSound(sound) {
  const $sound = document.createElement('div');
  $song.append($sound);
  $sound.id = 'sound-' + sound.syid;
  $sound.classList.add('sound');

  $sound.addEventListener('click', () => {
    focused = sound.syid;
    render();
  });

  const $text = document.createElement('p');
  $sound.append($text);
  $text.classList.add('text');
  $text.innerText = sound.text;

  const $user = document.createElement('p');
  $sound.append($user);
  $user.classList.add('user');
  $user.innerText = sound.uyid;

  return $sound;
}

function organize() {
  const arrows = {};
  for (const sound of sounds) {
    for (const dep of sound.deps) {
      if (!(dep in arrows)) arrows[dep] = [];
      arrows[dep].push(sound.syid);
    }
  }

  const tiers = {};
  let curs = sounds.filter(s => s.deps.length === 0).map(s => s.syid);
  let tier = 0;
  while (curs.length !== 0) {
    for (const cur of curs)
      tiers[cur] = tier;
    curs = curs.flatMap(cur => arrows[cur] ?? []);
    tier++;
  }

  const bytier = {};
  for (const syid in tiers) {
    const tier = tiers[syid];
    if (!(tier in bytier)) bytier[tier] = [];
    bytier[tier].push(syid);
  }

  const padding = 135;
  for (const tier in bytier) {
    bytier[tier].forEach((syid, i) => {
      const $sound = document.getElementById('sound-' + syid);
      const y = padding * (+tier + .5);
      const x = $song.scrollWidth / (bytier[tier].length + 1) * (i + 1) - ($sound.scrollWidth / 2);
      $sound.style.left = x + 'px';
      $sound.style.top = y + 'px';
    });
  }
}

function drawArrows() {
  for (const $arr of document.getElementsByClassName('arrow'))
    $arr.remove();

  for (const sound of sounds) {
    for (const dep of sound.deps) {
      const $arr = mkArrow(dep, sound.syid);
      $song.append($arr);
    }
  }

  function mkArrow(fromSyid, toSyid) {
    const $from = document.getElementById('sound-' + fromSyid);
    const $to = document.getElementById('sound-' + toSyid);

    const fromX = $from.offsetLeft + $from.scrollWidth / 2;
    const fromY = $from.offsetTop + $from.scrollHeight / 2;
    const toX = $to.offsetLeft + $to.scrollWidth / 2;
    const toY = $to.offsetTop + $to.scrollHeight / 2;

    const theta = Math.atan2(toY - fromY, toX - fromX);
    const r = Math.sqrt(Math.pow(toY - fromY, 2) + Math.pow(toX - fromX, 2));

    const $arr = document.createElement('span');
    $arr.classList.add('arrow');
    $arr.style.top = fromY + 'px';
    $arr.style.left = fromX + 'px';
    $arr.style.width = r + 'px';
    $arr.style.transform = 'rotate(' + theta + 'rad)';
    $arr.style.transformOrigin = 'center left';

    return $arr;
  }
}


document.body.addEventListener('keydown', event => {

  console.log(event);

  const $draft = document.getElementById('draft');
  if (event.key === 'Enter' && document.activeElement !== $draft) {
    event.preventDefault();
    $draft.focus();
  }

  else if (event.key === 'Enter' && event.shiftKey && document.activeElement === $draft) {
    event.preventDefault();
    const text = $draft.value;
    if (text.trim() === '') return;
    const syid = mkYid();
    const deps = [].concat(anchored, focused ? [focused] : []);
    const msg = { kind: 'push', syid, deps, text };
    ws.send(JSON.stringify(msg));

    $draft.value = '';
    $draft.blur();

    anchored.length = 0;
    render();
  }

  else if (event.key === 'e' && event.target === document.body) {
    if (focused !== null) {
      if (anchored.includes(focused))
        anchored.splice(anchored.indexOf(focused), 1);
      else
        anchored.push(focused);
      render();
    }
  }

  else if (event.key === 'Escape' && event.target === $draft) {
    $draft.blur();
  }

});

});
