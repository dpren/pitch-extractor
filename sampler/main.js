const noMidiMsgEl = "<h4 id='midi-err'>🎹 No MIDI device connected.</h4>";
const midiRejectedMsgEl = "<h4 id='midi-err'>🎹 Please enable MIDI permissions in your browser settings.</h4>";
const insertErrorMsg = (el) => document.body.insertAdjacentHTML("afterBegin", el);
const midiSelectEl = document.querySelector("#midiSelect");
let midiInputRef = null;

if (!navigator.requestMIDIAccess) {
  alert("This browser doesn't support Web MIDI :( \n\nTry Chrome or Opera instead.\n\n");
}

// No devices; Poll until device found.
const checkMidiRecursively = () => {

  return setTimeout(() => {
    console.log('checkMidiRecursively');
    navigator.requestMIDIAccess()
      .then(midi => {
        if (midi.inputs.size > 0) {
          document.querySelector("#midi-err").remove();
        } else {
          checkMidiRecursively();
        }
      })
      .catch(err => {
        console.error(err);
        insertErrorMsg(midiRejectedMsgEl);
      });
  }, 500);
};


const initialMidiCheck = () =>
  console.log('initialMidiCheck') ||
  navigator.requestMIDIAccess()
    .then(midi => {
      // console.log('  ~~ requestMIDIAccess');

      // if (midi.inputs.size <= 0) {
      //   console.log("NO MIDI DEVICES");
      //   insertErrorMsg(noMidiMsgEl);
      //   checkMidiRecursively();
      //   return;
      // }

      // use whatever first device initially:
      // const inputs = midi.inputs.values();
      // const input = inputs.next();
      // console.log(' input[0]:', input);
      // if (!input.done) {
      //   console.log('  &&&& selecting input:', input);
      //   input.value.addEventListener("midimessage", _onMidiMsg);
      //   midiInputRef = input;
      // }

    })
    .catch(err => {
      // console.error(err);
      insertErrorMsg(midiRejectedMsgEl);
    });





window.onMIDIMessage = ({ data }) => {
  // no videos loaded yet
  noteLog.textContent = data[1];
};
let _onMidiMsg = (ev) => {
  window.onMIDIMessage(ev);
};


midiSelectEl.addEventListener("change", (ev) => {
  console.log('midiSelectEl change', ev);
  const selName = ev.target.value;

  navigator.requestMIDIAccess()
    .then((midi) => {
      setMidiInputState(midi, selName);
    }, console.error);
});


const setMidiInputState = (midi, selName) => {
  midiInputRef?.value?.removeEventListener("midimessage", _onMidiMsg);
  midiInputRef = null;

  const inputs = midi.inputs.values();
  for (let input = inputs.next(); input && !input.done; input = inputs.next()) {
    if (input.value.name === selName) {
      input.value.addEventListener("midimessage", _onMidiMsg);
      midiInputRef = input;
      console.log('->midiInputRef:', midiInputRef);
    }
  }
};


const updateMidiSelectOpts = (midi) => {
  const inputsArr = [...midi.inputs.values()];

  midiSelectEl.innerHTML = inputsArr.length > 0
    ? inputsArr.map(({ name }) =>
      `<option ${name === midiInputRef?.value?.name ? "selected" : ""}>${name}</option>`).join("")
    : "<option disabled selected>-- No MIDI Inputs --</option>";
};


const handleMidiDeviceChange = (midi) => {
  console.log('^^^^ handleMidiDeviceChange:', midi);
  document.querySelector("#midi-err")?.remove(); // clear errors

  if (midi.inputs.size <= 0) {
    console.log("NO MIDI DEVICES");
    insertErrorMsg(noMidiMsgEl);
    setMidiInputState(midi, null);
    return;
  }

  // use first device if no selection
  if (!midiInputRef) {
    const inputs = midi.inputs.values();
    const firstInput = inputs.next();
    console.log(' &&& selecting firstInput:', firstInput.value.name);
    setMidiInputState(midi, firstInput.value.name);
  }


  // updateMidiSelectOpts(midi);
};

// INIT midi
navigator.requestMIDIAccess()
  .then((midi) => {
    console.log('|> requestMIDIAccess');

    // init
    handleMidiDeviceChange(midi);
    updateMidiSelectOpts(midi);

    // Listen for device connection changes
    midi.onstatechange = (ev) => {
      console.log('|> MIDIAccess statechange', ev);
      // if (ev.port.state === "disconnected") {
      // }
      handleMidiDeviceChange(midi);
      updateMidiSelectOpts(ev.target);
    };

    console.log('   Subscribed to MIDIAccess changes');

  }).catch(err => {
    console.error(err);
    insertErrorMsg(midiRejectedMsgEl);
  });





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


dropzoneEl.addEventListener('change', ev => {
  spinnerEl.style.display = 'inline-block';

  audioCtx = new (window.AudioContext || window.webkitAudioContext)();

  const files = rejectDotFiles(Array.from(ev.target.files));

  totalVidCount = files.length;

  videoEls = files.map(file => createVideoEl(file.name, file));
});

const createVideoEl = (filename, file) => {
  file.type = "video/webm;codecs=vp9,opus";
  // file.type = "video/x-matroska; codecs='h264,pcm'"
  console.log('file:', file);
  const src = URL.createObjectURL(file);
  const selectorId = 'v-' + dropExtension(filename);
  containerEl.insertAdjacentHTML('beforeend',
    `<video
      id="${selectorId}"
      src="${src}"
      style="display: none;"
      type="video/webm;codecs=vp9,opus"
      preload
      // preload="none"
    ></video>`
  );
  // class=""
  // type="video/mp4; codecs='mjpeg'"
  let vidEl = document.getElementById(selectorId);
  // vidEl.addEventListener('onload', () => {
  //   console.log('onload:', filename);
  //   // URL.revokeObjectURL(src);
  // });

  vidEl.midiNote = midiFromFilename(filename);
  vidEl.addEventListener('canplay', onCanPlay);
  /////////////////////////////////////////////
  // attachGainNode(vidEl);
  return vidEl;
};

const attachGainNode = vidEl => {
  vidEl.audioSourceNode = audioCtx.createMediaElementSource(vidEl); //TOO EXPENSIVE
  vidEl.gainNode = audioCtx.createGain();
  vidEl.audioSourceNode.connect(vidEl.gainNode);
  vidEl.gainNode.connect(audioCtx.destination);
};

const onCanPlay = ev => {
  console.log('onCanPlay');
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
};


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
  };

  const getRoundRobin = (midiNote) => {
    const vidMidiGroup = videoMidiGroups[midiNote];
    if (!vidMidiGroup) return;
    return vidMidiGroup[vidMidiGroup.rrIndex];
  };

  const scaleVel = unit => 1 - Math.log(1 + (10 * (1 - unit))) / Math.log(11);

  const playVideo = (midiNote, velocity) => {
    const videoEl = setGetRoundRobin(midiNote);
    // console.log(midiNote, velocity, scaleVel(velocity))
    if (!videoEl) return;
    videoEl.currentTime = 0;

    /////////////////////////////////////////////
    // videoEl.gainNode.gain.cancelScheduledValues(audioCtx.currentTime);
    // videoEl.gainNode.gain.setValueAtTime(0.0001, audioCtx.currentTime);
    // videoEl.gainNode.gain.exponentialRampToValueAtTime(scaleVel(velocity), audioCtx.currentTime + 0.03);
    /////////////////////////////////////////////

    // videoEl.className = "";
    videoEl.style.display = "inline";
    // videoEl.style.opacity = "1";
    videoEl.play();
    // setTimeout(() => {
    //   videoEl.style.display = "none";
    // }, 1800);
  };

  const stopVideo = (midiNote) => {
    const videoEl = getRoundRobin(midiNote);
    if (!videoEl) return;
    /////////////////////////////////////////////
    // videoEl.gainNode.gain.exponentialRampToValueAtTime(0.0001, audioCtx.currentTime + 0.8);
    videoEl.pause();
    // videoEl.fastSeek(0);



    // videoEl.className = "fadeOut";
    // setTimeout(() => {
    // videoEl.className = "";
    videoEl.style.display = "none";
    // videoEl.style.opacity = "0";
    // videoEl.style.display = "none";
    // videoEl.pause();
    // videoEl.stopVideo
    // setTimeout(() => {
    // }, 200);
    // }, 500);
  };


  console.log('==> sampler ready, listening onMIDIMessage...');
  window.onMIDIMessage = ({ data }) => {
    // console.log('onMIDIMessage:', data);
    // const channel = data[0] & 0xf;
    const command = data[0] >> 4;
    const midiNote = data[1];
    const velocity = data[2] / 127;
    // console.log(command, midiNote, velocity)

    // note on:
    if (command === 9 && velocity > 0) {
      noteLog.textContent = midiNote;
      playVideo(midiNote, velocity);
    }

    // note off:
    if (command === 8) { //|| velocity <= 0) {
      stopVideo(midiNote);
    }
  };
};;
