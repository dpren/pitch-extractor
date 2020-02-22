"use strict";
var socket;

function start(url) {
  socket = new WebSocket(url);
  socket.onopen = function () {
    document.documentElement.style.background = "#000";
  };
  socket.onclose = function () {
    document.documentElement.style.background = "#333";
    setTimeout(function () { start(url) }, 2000);
  };
  socket.onmessage = function (ev) {
    console.log("->", ev.data);
    Array.from(document.getElementsByClassName("option")).forEach(function (el) {
      el.classList.remove("selected");
    });
    document.getElementById(ev.data).classList.add("selected")
  };
}

function onBtnClick(ev) {
  var target = ev.target;
  if (target.className !== "option") {
    return;
  }
  console.log('send:', target.id);
  socket.send(target.id);
};
window.addEventListener("click", onBtnClick);
window.addEventListener("touchstart", onBtnClick);

start("ws://" + location.host);