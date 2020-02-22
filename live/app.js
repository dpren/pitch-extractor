console.clear();
const path = require('path');
const express = require('express');
const { createServer } = require('http');
const WebSocket = require('ws');
const ip = require("ip");
const static = str => express.static(path.join(__dirname, str));

const app = express();
app.use(static("public"));
app.use("/", static("public/app"));

// app.get('/', (req, res) => {
//   res.sendFile(path.join(public, 'index.html'));
// });

const server = createServer(app);

const port = 80;
const wss = new WebSocket.Server({ server });

wss.on('connection', ws => {
  console.log('connection');

  ws.on('message', message => {
    console.log('message:', message);

    wss.clients.forEach(client => {
      if (client.readyState === WebSocket.OPEN) {
        client.send(message);
      }
    });
  });
});

server.listen(port, () => {
  console.log(`Listening on:
  http://localhost:${port}
  http://${ip.address()}:${port}
  `);
});