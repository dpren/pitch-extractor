const noMidiMsgEl = "<h4 id='midi-err'>🎹 No MIDI device connected.</h4>";
const insertNoMidiMsgEl = () => document.body.insertAdjacentHTML("afterBegin", noMidiMsgEl);

const checkMidiRecursively = () =>
  setTimeout(() => {
    navigator.requestMIDIAccess()
      .then(m => {
        m.inputs.size > 0
          ? document.querySelector("#midi-err").remove()
          : checkMidiRecursively()
      }, console.error);
  }, 500);

const initialMidiCheck = () =>
  navigator.requestMIDIAccess()
    .then(m => {
      // midi is not connected
      if (m.inputs.size <= 0) {
        insertNoMidiMsgEl();
        console.log('insertNoMidiMsgEl')
        checkMidiRecursively();
      }
    }, console.error);


if (!navigator.requestMIDIAccess) {
  alert("This browser doesn't support Web MIDI :( \n\nTry Chrome or Opera instead.\n\n");
} else {
  initialMidiCheck();
}


window.onMIDIMessage = ({ data }) => {
  noteLog.textContent = data[1];
  console.log("onMIDIMessage, no vids loaded");
}
let _onMidiMsg = (ev) => window.onMIDIMessage(ev);
const midiSelect = document.querySelector("#midiSelect");
let midiRefs = [];

midiSelect.addEventListener("change", (ev) => {
  const selection = ev.target.value;

  navigator.requestMIDIAccess()
    .then((midi) => {
      let inputs = midi.inputs.values();

      midiRefs.forEach(inp => {
        inp.value.removeEventListener("midimessage", _onMidiMsg);
      });
      midiRefs = [];

      for (let input = inputs.next(); input && !input.done; input = inputs.next()) {
        if (input.value.name === selection) {
          console.log('> selected:', input.value.name);
          input.value.addEventListener("midimessage", _onMidiMsg);
        }
        midiRefs.push(input);
      }
    }, console.error);
});

const updateMidiSelectOpts = (midi) => {
  const inpsArr = [...midi.inputs.values()];

  midiSelect.innerHTML = inpsArr.length > 0
    ? inpsArr.map(inp => `<option>${inp.name}</option>`).join("")
    : "<option disabled selected>-- No MIDI Inputs --</option>"
};

navigator.requestMIDIAccess()
  .then((midi) => {
    updateMidiSelectOpts(midi);
    midi.onstatechange = (ev) => {
      updateMidiSelectOpts(ev.target);
    }
  }, console.error);



Object.assign(this, R);
const containerEl = document.querySelector('#container');
const dropzoneEl = document.querySelector('#dropzone');
const spinnerEl = document.querySelector('#spinner');
const noteLog = document.querySelector('#noteLog');
let audioCtx;

let videoEls = [];
window.videoEls = videoEls;
let vidsLoaded = 0;
let totalVidCount = 0;

const rejectDotFiles = reject(pathEq(['name', '0'], '.'));
const dropExtension = f => f.split('.')[0];
const midiFromFilename = f => f.split('__')[0];

function sleep(ms) {
  return new Promise(resolve => setTimeout(resolve, ms));
}

dropzoneEl.addEventListener('change', ev => {
  spinnerEl.style.display = 'inline-block';

  audioCtx = new (window.AudioContext || window.webkitAudioContext)();

  const files = rejectDotFiles(Array.from(ev.target.files));
  // console.log('ev.target.files:', ev.target.files)

  totalVidCount = files.length;

  // for (let i = 0; i < totalVidCount; i++) {
  // await sleep(1000);
  // createVideoEl(files[i].name, URL.createObjectURL(files[i]))
  // }
  // console.log('files[0]:', files[0])
  // let src = URL.createObjectURL(files[0]);
  // console.log('src:', src)
  // createVideoEl(files[0].name, src)


  videoEls = files.map(file => createVideoEl(file.name, file));
  // createVideoEl(file.name, URL.createObjectURL(file))
});

const createVideoEl = (filename, file) => {
  // file.type = "video/webm;codecs=vp9,opus"
  // file.type = "video/x-matroska; codecs='h264,pcm'"
  console.log('file:', file)
  const src = URL.createObjectURL(file);
  const selectorId = 'v-' + dropExtension(filename);
  containerEl.insertAdjacentHTML('beforeend',
    `<video
      id="${selectorId}"
      src="${src}"
      style="display: none;"
      preload
      muted="true"
    ></video>`
  );
  // type="video/webm;codecs=vp9,opus"
  // class=""
  // type="video/mp4; codecs='mjpeg'"
  let vidEl = document.getElementById(selectorId);
  // vidEl.addEventListener('onload', () => {
  //   console.log('onload:', filename);
  //   // URL.revokeObjectURL(src);
  // });

  vidEl.midiNote = midiFromFilename(filename);
  vidEl.addEventListener('canplay', onCanPlay);
  // attachGainNode(vidEl);
  return vidEl;
};

const attachGainNode = vidEl => {
  vidEl.audioSourceNode = audioCtx.createMediaElementSource(vidEl);
  vidEl.gainNode = audioCtx.createGain();
  vidEl.audioSourceNode.connect(vidEl.gainNode);
  vidEl.gainNode.connect(audioCtx.destination);
}

const onCanPlay = ev => {
  console.log('onCanPlay')
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
  // spinnerEl.style.display = 'none';
  // dropzoneEl.style.display = 'none';
  spinnerEl.remove();
  dropzoneEl.remove();

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
    // videoEl.currentTime = 0;
    // videoEl.gainNode.gain.cancelScheduledValues(audioCtx.currentTime);
    // videoEl.gainNode.gain.setValueAtTime(0.0001, audioCtx.currentTime);
    // videoEl.gainNode.gain.exponentialRampToValueAtTime(scaleVel(velocity), audioCtx.currentTime + 0.03);
    // videoEl.className = "";
    videoEl.style.display = "inline";
    // videoEl.style.opacity = "1";
    videoEl.play();
    // setTimeout(() => {
    //   videoEl.style.display = "none";
    // }, 1800);
  }

  const stopVideo = (midiNote) => {
    const videoEl = getRoundRobin(midiNote);
    if (!videoEl) return;
    // videoEl.gainNode.gain.exponentialRampToValueAtTime(0.0001, audioCtx.currentTime + 0.8);
    // videoEl.className = "fadeOut";
    // setTimeout(() => {
    // videoEl.className = "";
    videoEl.style.display = "none";
    // videoEl.style.opacity = "0";
    // videoEl.pause();
    // videoEl.stopVideo
    // setTimeout(() => {
    // }, 200);
    // }, 500);
  }

  // navigator.requestMIDIAccess()
  //   .then(success, console.error);

  // function success(midi) {
  //   let inputs = midi.inputs.values();

  //   for (let input = inputs.next(); input && !input.done; input = inputs.next()) {
  //     if (input.value.name === "IAC Driver Bus 1") {
  //       input.value.onmidimessage = onMIDIMessage;
  //     }
  //   }
  // }

  window.onMIDIMessage = ({ data }) => {
    // const channel = data[0] & 0xf;
    const command = data[0] >> 4;
    const midiNote = data[1];
    const velocity = data[2] / 127;
    // console.log(command, midiNote, velocity)

    if (command === 9 && velocity > 0) {
      noteLog.textContent = midiNote;
      playVideo(midiNote, velocity);
    }

    if (command === 8) { //|| velocity <= 0) {
      stopVideo(midiNote);
    }
  }
}
