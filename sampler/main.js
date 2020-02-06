const noMidiMsgEl = "<h4 id='midi-err'>🎹 No MIDI device connected.</h4>";
const insertNoMidiMsgEl = () => document.body.insertAdjacentHTML('beforeend', noMidiMsgEl);

const checkMidiRecursively = () =>
  setTimeout(() => {
    navigator.requestMIDIAccess()
      .then(m => {
        m.inputs.size > 0
          ? document.querySelector('#midi-err').remove()
          : checkMidiRecursively()
      }, console.error);
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
let audioCtx;

let videoEls = [];
window.videoEls = videoEls;
let vidsLoaded = 0;
let totalVidCount = 0;

const rejectDotFiles = reject(pathEq(['name', '0'], '.'));
const dropExtension = f => f.split('.')[0];
const midiFromFilename = f => f.split('__')[0];


dropzoneEl.onchange = ev => {
  spinnerEl.style.display = 'inline-block';

  audioCtx = new (window.AudioContext || window.webkitAudioContext)();

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
      class=""
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



const videoElsToIndexedGroups = videoEls => {
  const sorted = sortBy(prop(`midiNote`), videoEls);

  const groups = groupWith(eqProps("midiNote"), sorted);

  const ixdGrps = indexBy(grp => grp[0].midiNote, groups);

  const initd = map(x => {
    x.rrIndex = 0;
    return x;
  }, ixdGrps);

  return initd;
}


const onAllVideosLoaded = (videoEls) => {
  spinnerEl.style.display = 'none';
  dropzoneContainerEl.style.display = 'none';

  let videoMidiGroups = videoElsToIndexedGroups(videoEls);
  window.videoMidiGroups = videoMidiGroups;

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

  const scaleVel = unit => 1 - Math.log(1 + (10 * (1 - unit))) / Math.log(11);

  const playVideo = (midiNote, velocity) => {
    const videoEl = setGetRoundRobin(midiNote);
    // console.log(midiNote, velocity, scaleVel(velocity))
    if (!videoEl) return;
    videoEl.currentTime = 0;
    videoEl.gainNode.gain.cancelScheduledValues(audioCtx.currentTime);
    videoEl.gainNode.gain.setValueAtTime(0.0001, audioCtx.currentTime);
    videoEl.gainNode.gain.exponentialRampToValueAtTime(scaleVel(velocity), audioCtx.currentTime + 0.03);
    // videoEl.className = "";
    videoEl.style.display = "inline";
    // videoEl.style.opacity = "1";
    videoEl.play();
    setTimeout(() => {
      videoEl.style.display = "none";
    }, 1800);
  }

  const stopVideo = (midiNote) => {
    const videoEl = getRoundRobin(midiNote);
    if (!videoEl) return;
    videoEl.gainNode.gain.exponentialRampToValueAtTime(0.0001, audioCtx.currentTime + 0.8);
    videoEl.className = "fadeOut";
    setTimeout(() => {
      videoEl.className = "";
      videoEl.style.display = "none";
      // videoEl.style.opacity = "0";
      // videoEl.style.display = "none";
      // videoEl.pause();
      // videoEl.stopVideo
      // setTimeout(() => {
      // }, 200);
    }, 500);
  }

  navigator.requestMIDIAccess()
    .then(success, console.error);

  function success(midi) {
    let inputs = midi.inputs.values();

    for (let input = inputs.next(); input && !input.done; input = inputs.next()) {
      input.value.onmidimessage = onMIDIMessage;
    }
  }

  function onMIDIMessage({ data }) {
    // const channel = data[0] & 0xf;
    const command = data[0] >> 4;
    const midiNote = data[1];
    const velocity = data[2] / 127;
    // console.log(command, midiNote, velocity)

    if (command === 9 && velocity > 0) {
      playVideo(midiNote, velocity);
    }

    if (command === 8) { //|| velocity <= 0) {
      stopVideo(midiNote);
    }
  }
}
