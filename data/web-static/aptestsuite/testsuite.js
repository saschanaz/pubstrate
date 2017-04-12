/*
;;; Mudsync --- Live hackable MUD
;;; Copyright © 2017 Christopher Allan Webber <cwebber@dustycloud.org>
;;;
;;; This file is part of Mudsync.
;;;
;;; Mudsync is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or
;;; (at your option) any later version.
;;;
;;; Mudsync is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with Mudsync.  If not, see <http://www.gnu.org/licenses/>.
*/

function scrollDown() {
    var stream_metabox = document.getElementById("stream-metabox");o
    stream_metabox.scrollTop = stream_metabox.scrollHeight;
}

function withMaybeScroll(thunk) {
    var stream_metabox = document.getElementById("stream-metabox");
    var should_scroll = false;
    // if within a reasonable threshold, we scroll
    if((stream_metabox.scrollHeight - stream_metabox.offsetHeight)
       - stream_metabox.scrollTop <= 50) {
        should_scroll = true;
    }
    thunk();
    if (should_scroll) {
        stream_metabox.scrollTop = stream_metabox.scrollHeight;
    }
}


function displayMessage(data, self_sent) {
    var new_entry = document.createElement("div");
    withMaybeScroll(
        function () {
            if (self_sent) {
                new_entry.setAttribute("class", "stream-entry self-sent");
                document.getElementById("main-input").value = "";
            } else {
                new_entry.setAttribute("class", "stream-entry");
            }
            new_entry.innerHTML = data;
            document.getElementById("stream").appendChild(new_entry);
        });
}

function setConnectedText(string, to_class) {
    var stream_metabox = document.getElementById("connection-status");
    stream_metabox.textContent = "[" + string + "]";
    stream_metabox.setAttribute("class", to_class);
}

function installWebsocket() {
    // TODO: Don't hardcode the websocket path; pull it from the DOM
    var address = "ws://".concat(window.location.hostname, ":", window.location.port);
    var ws = new WebSocket(address);
    ws.onmessage = function(evt) {
        displayMessage(evt.data, false);
    };
    ws.onopen = function() {
        setConnectedText("connected", "connected");
        console.log("connected");
    };
    ws.onclose = function () {
        setConnectedText("disconnected", "disconnected");
        // kludge, we shouldn't be using self_sent like this because it
        // wipes the input
        displayMessage(
            "* You have been disconnected.  Refresh to (hopefully) reconnect.",
            true);
        console.log("closed websocket");
    };
    installUIHooks(ws);
}

function installUIHooks(ws) {
    var input = document.getElementById("main-input");
    input.onkeypress = function(e) {
        if (!e) e = window.event;
        var keyCode = e.keyCode || e.which;
        if (keyCode == '13') {
            var input_val = input.value;
            withMaybeScroll(
                function () {
                    displayMessage("> ".concat(input_val), true);
                });
            sendMessageToServer(ws, input_val);
        }
    }
}

function sendMessageToServer(ws, data) {
    ws.send(data);
}

window.onload = function () {
    installWebsocket();
    window.onresize = scrollDown;
}
