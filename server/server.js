const WebSocket = require('ws');

const wss = new WebSocket.Server({ port: 8080 });

const clients = [];
const songs = {};

wss.on('connection', function(ws) {

  clients.push(ws);

  ws.on('close', function() {
    clients.splice(clients.indexOf(ws), 1);
  });

  ws.on('message', function(data) {
    const msg = JSON.parse(data.toString());

    switch (msg.kind) {

      case 'init': {
        const cyid = ws._cyid = msg.cyid;
        const uyid = ws._uyid = msg.uyid;
        if (!(cyid in songs)) songs[cyid] = [ { syid: mkYid(), uyid: mkYid(), deps: [], text: "begin" } ];
        const sounds = songs[cyid];
        ws.send(JSON.stringify(sounds));
      }
      break;

      case 'push': {
        const sound = { syid: msg.syid, uyid: ws._uyid, deps: msg.deps, text: msg.text };
        const cyid = ws._cyid;
        songs[cyid].push(sound);
        for (const c of clients)
          if (c._cyid === cyid)
            c.send(JSON.stringify([sound]));
      }
      break;

      default: {
      }
      break;

    }
  });

});

function mkYid() {
  const t = Date.now() + '';
  const r = (Math.floor(Math.random() * 1e6) + '').padStart(6, '0')
  return t + '-' + r;
}
