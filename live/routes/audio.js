const socket = new WebSocket(`ws://127.0.0.1:80`);
socket.onopen = ev => {
  console.log('open')
}
socket.onmessage = ({ data }) => {
  console.log("->", data);
  location.href = `file:///Users/dpren/code/pitch-extractor/live/routes/${data}/audio.html`
}

if (!navigator.requestMIDIAccess) {
  alert("This browser doesn't support Web MIDI :( \n\nTry Chrome or Opera instead.\n\n");
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
      selectMidiInput(midi, selection)
    }, console.error);
});

const selectMidiInput = (midi, selection) => {
  let inputs = midi.inputs.values();

  midiRefs.forEach(inp => {
    inp.value.removeEventListener("midimessage", _onMidiMsg);
  });
  midiRefs = [];

  for (let input = inputs.next(); input && !input.done; input = inputs.next()) {
    if (input.value.name === selection) {
      console.log('> selected:', input.value.name);
      input.value.addEventListener("midimessage", _onMidiMsg);
      localStorage.selectedMidiInput = input.value.name;
    }
    midiRefs.push(input);
  }
}

navigator.requestMIDIAccess()
  .then((midi) => {
    updateMidiSelectOpts(midi);
    midi.onstatechange = (ev) => {
      updateMidiSelectOpts(ev.target);
    }
  }, console.error);

const updateMidiSelectOpts = (midi, noset) => {
  const inpsArr = [...midi.inputs.values()];

  if (!inpsArr.length) {
    midiSelect.innerHTML = "<option disabled selected>-- No MIDI Inputs --</option>";
  } else {
    midiSelect.innerHTML = inpsArr.map(inp => `<option>${inp.name}</option>`).join("")

    const { selectedMidiInput } = localStorage;
    if (selectedMidiInput && selectedMidiInput !== midiSelect.value) {
      midiSelect.value = selectedMidiInput;
      selectMidiInput(midi, selectedMidiInput);
    } else {
      midiSelect.value = inpsArr[0].name;
      selectMidiInput(midi, inpsArr[0].name);
    }
  }

};


Object.assign(this, R);
const containerEl = document.querySelector('#container');
// const dropzoneEl = document.querySelector('#dropzone');
const spinnerEl = document.querySelector('#spinner');
const noteLog = document.querySelector('#noteLog');
let audioCtx;

let videoEls = [];
window.videoEls = videoEls;

const rejectDotFiles = reject(pathEq(['name', '0'], '.'));
const dropExtension = f => f.split('.')[0];
const midiFromFilename = f => f.split('__')[0];





const init = () => {
  console.log('init')
  // document.removeEventListener("click", init);
  audioCtx = new (window.AudioContext || window.webkitAudioContext)();

  videoEls = [...containerEl.children];
  videoEls.forEach(vidEl => {
    const filename = vidEl.id.slice(2);
    vidEl.midiNote = midiFromFilename(filename);
    attachGainNode(vidEl);
  });
  console.log('all vids loaoded');
  onAllVideosLoaded(videoEls);
}
window.addEventListener("load", init);

const attachGainNode = vidEl => {
  vidEl.audioSourceNode = audioCtx.createMediaElementSource(vidEl);
  vidEl.gainNode = audioCtx.createGain();
  vidEl.audioSourceNode.connect(vidEl.gainNode);
  vidEl.gainNode.connect(audioCtx.destination);
}



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
  // spinnerEl.remove();
  // dropzoneEl.remove();
  // audioCtx.resume();

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
    // videoEl.style.display = "inline";
    // videoEl.style.opacity = "1";
    videoEl.play();
    // setTimeout(() => {
    //   videoEl.style.display = "none";
    // }, 1800);
  }

  const stopVideo = (midiNote) => {
    const videoEl = getRoundRobin(midiNote);
    if (!videoEl) return;
    videoEl.gainNode.gain.exponentialRampToValueAtTime(0.0001, audioCtx.currentTime + 0.8);
    // videoEl.className = "fadeOut";
    // setTimeout(() => {
    // videoEl.className = "";
    // videoEl.style.display = "none";
    // videoEl.style.opacity = "0";
    // videoEl.style.display = "none";
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
