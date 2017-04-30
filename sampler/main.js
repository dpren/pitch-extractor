const noMidiMsgEl = "<h4 id='midi-err'>🎹 No MIDI device connected.</h4>";
const insertNoMidiMsgEl = () => document.body.insertAdjacentHTML('beforeend', noMidiMsgEl);

const checkMidiRecursively = () =>
  setTimeout(() => {
    navigator.requestMIDIAccess()
      .then(m => m.inputs.size > 0 ?
          document.querySelector('#midi-err').remove()
            : checkMidiRecursively()
      , console.error);
  }, 500);

const initialMidiCheck = () =>
  navigator.requestMIDIAccess()
    .then(m => {
      // midi is not connected
      if (m.inputs.size <= 0) {
        insertNoMidiMsgEl();
        checkMidiRecursively();
      }
    }, console.error);


if (!navigator.requestMIDIAccess) {
  alert("This browser doesn't support Web MIDI :( \n\nTry Chrome or Opera instead.\n\n");
} else {
  initialMidiCheck();
}

Object.assign(this, R);
const containerEl = document.querySelector('#container');
const dropzoneEl = document.querySelector('#dropzone');
const dropzoneContainerEl = document.querySelector('#dropzone-container');
const spinnerEl = document.querySelector('#spinner');
const audioCtx = new (window.AudioContext || window.webkitAudioContext)();

let videoEls = [];
let vidsLoaded = 0;
let totalVidCount = 0;

const rejectDotFiles = reject(pathEq(['name', '0'], '.'));
const dropExtension = f => f.split('.')[0];
const midiFromFilename = f => f.split('__')[0];


dropzoneEl.onchange = ev => {
  spinnerEl.style.display = 'inline-block';

  const files = rejectDotFiles(Array.from(ev.target.files));

  totalVidCount = files.length;

  videoEls = files.map(file =>
    createVideoEl(file.name, URL.createObjectURL(file))
  );
};

const createVideoEl = (filename, src) => {
  const selectorId = 'v-' + dropExtension(filename);
  containerEl.insertAdjacentHTML('beforeend',
    `<video
      id="${selectorId}"
      src="${src}"
      style="display: none;"
    ></video>`
  );
  let vidEl = document.getElementById(selectorId);
  vidEl.midiNote = midiFromFilename(filename);
  vidEl.addEventListener('canplay', onCanPlay);
  attachGainNode(vidEl);
  return vidEl;
};

const attachGainNode = vidEl => {
  vidEl.audioSourceNode = audioCtx.createMediaElementSource(vidEl);
  vidEl.gainNode = audioCtx.createGain();
  vidEl.audioSourceNode.connect(vidEl.gainNode);
  vidEl.gainNode.connect(audioCtx.destination);
}

const onCanPlay = ev => {
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
  spinnerEl.style.display = 'none';
  dropzoneContainerEl.style.display = 'none';

  let videoMidiGroups = videoElsToIndexedGroups(videoEls);

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
    videoEl.gainNode.gain.cancelScheduledValues(audioCtx.currentTime);
    videoEl.gainNode.gain.setValueAtTime(0.0001, audioCtx.currentTime);
    videoEl.gainNode.gain.exponentialRampToValueAtTime(1, audioCtx.currentTime + 0.02);
    videoEl.style.display = 'inline';
    videoEl.play();
  }

  const stopVideo = (midiNote) => {
    const videoEl = getRoundRobin(midiNote);
    if (!videoEl) return;
    videoEl.gainNode.gain.exponentialRampToValueAtTime(0.0001, audioCtx.currentTime + 0.3);
    videoEl.style.display = 'none';
    setTimeout(() => {
      videoEl.pause();
    }, (0.2) * 500);
  }

  navigator.requestMIDIAccess()
    .then(success, console.error);

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
