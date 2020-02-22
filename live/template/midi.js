const midiSelect = document.querySelector("#midiSelect");
if (!navigator.requestMIDIAccess) {
  alert("This browser doesn't support Web MIDI :( \n\nTry Chrome or Opera instead.\n\n");
}

window.onMIDIMessage = ({ data }) => {
  noteLog.textContent = data[1];
  console.log("onMIDIMessage, no vids loaded");
}

let _onMidiMsg = (ev) => window.onMIDIMessage(ev);
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
