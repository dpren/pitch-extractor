Object.assign(this, R);
const container = document.querySelector('#container');
const dropzone = document.querySelector('#dropzone');
const spinner = document.querySelector('#spinner');

let videoEls = [];
let vidsLoaded = 0;

const rejectDotFiles = reject(pathEq(['name', '0'], '.'));
const dropExtension = f => f.split('.')[0];
const midiFromFilename = f => f.split('__')[0];

dropzone.onchange = ev => {
  spinner.style.display = 'inline-block';

  const files = rejectDotFiles(Array.from(ev.target.files));

  videoEls = files.map(file =>
    createVideoEl(file.name, URL.createObjectURL(file), files.length)
  );
};

const createVideoEl = (filename, src, totalVidCount) => {
  const selectorId = 'v-' + dropExtension(filename);
  container.insertAdjacentHTML('beforeend',
    `<video
      id="${selectorId}"
      src="${src}"
      style="display: none;"
    ></video>`
  );
  let vidEl = document.getElementById(selectorId);
  vidEl.midiNote = midiFromFilename(filename);
  vidEl.addEventListener('canplay', onCanPlay(totalVidCount));
  return vidEl;
};

const onCanPlay = totalVidCount => ev => {
  vidsLoaded++;
  ev.target.removeEventListener('canplay', onCanPlay);
  if (vidsLoaded === totalVidCount) {
    onAllVideosLoaded(videoEls);
  }
};

const groupVidsByMidi = groupWith(eqProps('midiNote'));

const indexGroupsByMidi = indexBy(grp => grp[0].midiNote);

const initRRIndex = map(x => { x.rrIndex = 0; return x; });

const videoElsToIndexedGroups = compose(initRRIndex, indexGroupsByMidi, groupVidsByMidi);

const onAllVideosLoaded = (videoEls) => {
  spinner.style.display = 'none';

  const videoMidiGroups = videoElsToIndexedGroups(videoEls)

  const setGetRoundRobin = (midiNote) => {
    const vidMidiGroup = videoMidiGroups[midiNote];
    if (!vidMidiGroup) return;
    const nextIndex = vidMidiGroup.rrIndex + 1;

    if (nextIndex >= vidMidiGroup.length) {
      vidMidiGroup.rrIndex = 0;
    } else {
      vidMidiGroup.rrIndex = nextIndex;
    }
    return vidMidiGroup[vidMidiGroup.rrIndex];
  }

  const getRoundRobin = (midiNote) => {
    const vidMidiGroup = videoMidiGroups[midiNote];
    if (!vidMidiGroup) return;
    return vidMidiGroup[vidMidiGroup.rrIndex];
  }

  const playVideo = (midiNote) => {
    const videoEl = setGetRoundRobin(midiNote);
    if (!videoEl) return;
    videoEl.currentTime = 0;
    videoEl.style.display = 'inline';
    videoEl.play();
  }

  const stopVideo = (midiNote) => {
    const videoEl = getRoundRobin(midiNote);
    if (!videoEl) return;
    videoEl.style.display = 'none';
    videoEl.pause();
  }

  navigator.requestMIDIAccess()
    .then(success, console.warn);

  function success(midi) {
    let inputs = midi.inputs.values();

    for (let input = inputs.next(); input && !input.done; input = inputs.next()) {
      input.value.onmidimessage = onMIDIMessage;
    }
  }

  function onMIDIMessage(message) {
    const midiNote = message.data[1];

    if (message.data[0] === 144 && message.data[2] > 0) {
      playVideo(midiNote);
    }

    if (message.data[0] === 128 || message.data[2] === 0) {
      stopVideo(midiNote);
    }
  }
}
