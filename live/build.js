console.clear();
const path = require('path');
const fs = require('fs');
const { promisify } = require('util');
const readdir = promisify(fs.readdir);
const writeFile = promisify(fs.writeFile);
const mkdir = promisify(fs.mkdir);

// console.log('__dirname:', __dirname);
// const isVideo = true;


const _genHtml = async (vdir, isVideo = false) => {
  const filesRaw = await readdir(path.normalize(`../vid-output/${vdir}`));
  const files = filesRaw.filter(file => file.endsWith(".mkv"))
  const muted = isVideo ? 'muted="true"' : '';

  const videoTags = files.map(file => {
    const src = path.normalize(`../../../vid-output/${vdir}/${file}`);

    return `<video id="v-${file.split(".")[0]}" src="${src}" ${muted} style="display: none;" preload></video>`
  }).join('\n')

  const htmlText = htmlTemplate({ videoTags, isVideo });

  await mkdir(
    path.normalize(__dirname + "/routes/" + vdir),
    { recursive: true }
  );
  await writeFile(
    path.normalize(__dirname + "/routes/" + vdir + (isVideo ? "/video.html" : "/audio.html")),
    htmlText
  );
};
const genHtml = (vdir) => {
  _genHtml(vdir, true);
  _genHtml(vdir, false);
};


const htmlTemplate = ({ videoTags, isVideo }) => `
<html>
  <head>
    <meta charset="utf-8">
    <link href="../main.css" rel="stylesheet">
    <script src="https://cdnjs.cloudflare.com/ajax/libs/ramda/0.26.1/ramda.min.js"></script>
  </head>
  <body>
    <div id="container">
      ${videoTags}
    </div>
    <span class="midiSelectContainer">
      <select id="midiSelect">
        <option value="">No MIDI Inputs</option>
      </select>
    </span>
    <samp id="noteLog"></samp>
    <script src="${path.normalize(isVideo ? "../video.js" : "../audio.js")}"></script>
  </body>
</html>
`;

genHtml("marimba");
