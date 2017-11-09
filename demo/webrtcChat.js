//Key-Value pair storage, both keys & values should be strings
var __dataStore = {};
var peerConnections = {};
var dataChannels = {};

function debugMe(a, b) {
    "use strict";
    console.log(a, b);
    execF("harshi", a);
}

var EVENTS = {
    "OFFER_GENERATED": "offer-generated",
    "ANSWER_GENRATED": "answer-generated",
    "ICE_CANDIDATE_GENERATED": "ice-candidate-generated",
    "HANDSHAKE_COMPLETE": "handshake-complete",
    "DISCONNECTED": "disconnect",
    "MESSAGE_RECEIVED": "message-received"
};

var CONSTANTS = {
    "UNDEFINED": "undefined",
    "EMPTY": ""
};

var DS_KEYS = {
    "OFFER": "offer",
    "ANSWER": "answer",
    "ICE_CANDIDATE": "ice-candidate",
    "EVENT": "event",
    "CAN_EXCHANGE_ICE": "canExchangeIce",
    "MESSAGE": "message"
};

var defaultConfig = {
    "iceServers": [{
        "url": "stun:stun.l.google.com:19302"
    }, {
        "url": "stun:stun1.l.google.com:19302"
    }, {
        "url": "stun:stun2.l.google.com:1930"
    }, {
        "url": "stun:stun3.l.google.com:19302"
    }, {
        "url": "stun:stun4.l.google.com:19302"
    }]
};

function initDataStore(targetClientId) {
    "use strict";
    __dataStore[targetClientId] = {};
    __dataStore[targetClientId][DS_KEYS.OFFER] = CONSTANTS.UNDEFINED;
    __dataStore[targetClientId][DS_KEYS.ANSWER] = CONSTANTS.UNDEFINED;
    __dataStore[targetClientId][DS_KEYS.ICE_CANDIDATE] = CONSTANTS.UNDEFINED;
    __dataStore[targetClientId][DS_KEYS.EVENT] = CONSTANTS.UNDEFINED;
    __dataStore[targetClientId][DS_KEYS.CAN_EXCHANGE_ICE] = false;
    __dataStore[targetClientId][DS_KEYS.MESSAGE] = CONSTANTS.EMPTY;
}

function getDatastore(targetClientId, key) {
    "use strict";
    return __dataStore[targetClientId][key];
}

function sendWebRTCMessage(targetClientId, message) {
    "use strict";
    dataChannels[targetClientId].send(message);
}

function enableIceExchange(targetClientId) {
    "use strict";
    __dataStore[targetClientId][DS_KEYS.CAN_EXCHANGE_ICE] = true;
    if (getDatastore(targetClientId, DS_KEYS.ICE_CANDIDATE) !== CONSTANTS.UNDEFINED) {
        __dataStore[targetClientId][DS_KEYS.EVENT] = EVENTS.ICE_CANDIDATE_GENERATED;
    }
}

function getPendingEvent(targetClientId) {
    "use strict";
    return __dataStore[targetClientId][DS_KEYS.EVENT];
}

function clearPendingEvent(targetClientId, eventType) {
    "use strict";
    console.log("Clearing Event ", eventType, " and current latest event is ", __dataStore[targetClientId][DS_KEYS.EVENT]);
    __dataStore[targetClientId][DS_KEYS.EVENT] = CONSTANTS.UNDEFINED;
    switch (eventType) {
        case EVENTS.ANSWER_GENRATED:
            enableIceExchange(targetClientId);
            break;
        case EVENTS.ICE_CANDIDATE_GENERATED:
            __dataStore[targetClientId][DS_KEYS.ICE_CANDIDATE] = CONSTANTS.UNDEFINED;
            break;
        case EVENTS.MESSAGE_RECEIVED:
            __dataStore[targetClientId][DS_KEYS.MESSAGE] = CONSTANTS.EMPTY;
            break;
    }
}

function closeVideoCall(myPeerConnection) {
    "use strict";
    console.log("Closing the call");

    // Close the RTCPeerConnection

    if (myPeerConnection) {
        console.log("--> Closing the peer connection");

        // Disconnect all our event listeners; we don"t want stray events
        // to interfere with the hangup while it"s ongoing.

        myPeerConnection.onaddstream = null; // For older implementations
        myPeerConnection.ontrack = null; // For newer ones
        myPeerConnection.onremovestream = null;
        myPeerConnection.onicecandidate = null;
        myPeerConnection.oniceconnectionstatechange = null;
        myPeerConnection.onsignalingstatechange = null;
        myPeerConnection.onicegatheringstatechange = null;
        myPeerConnection.onnotificationneeded = null;


        myPeerConnection.close();
        myPeerConnection = null;
    }
}


function handleRemoveStreamEvent(event) {
    "use strict";
    console.log("*** Stream removed");
    closeVideoCall();
}

function handleSignalingStateChangeEvent(event) {
    "use strict";
    var myPeerConnection = event.srcElement;
    console.log("*** WebRTC signaling state changed to: " + myPeerConnection.signalingState);
    switch (myPeerConnection.signalingState) {
        case "closed":
            closeVideoCall(myPeerConnection);
            break;
    }
}

function disconnect(targetClientId) {
    "use strict";
    closeVideoCall(peerConnections[targetClientId]);
    delete peerConnections[targetClientId];
}


function _createRTCPeerConnection(targetClientId) {
    "use strict";
    var myPeerConnection = new RTCPeerConnection(defaultConfig, {
        mandatory: {
            OfferToReceiveAudio: true,
            OfferToReceiveVideo: true
        }
    });

    myPeerConnection.onnremovestream = handleRemoveStreamEvent;
    myPeerConnection.oniceconnectionstatechange = function (event) {
        console.log(event);
        var connection = event.srcElement;
        console.log("*** ICE connection state changed to " + connection.iceConnectionState);

        switch (connection.iceConnectionState) {
            case "closed":
            case "failed":
            case "disconnected":
                closeVideoCall(connection);
                delete peerConnections[targetClientId];
                break;
        }
    };

    myPeerConnection.onsignalingstatechange = handleSignalingStateChangeEvent;

    myPeerConnection.onicecandidate = function (event) {
        if (event.candidate) {
            if (__dataStore[targetClientId][DS_KEYS.ICE_CANDIDATE] === CONSTANTS.UNDEFINED) {
                __dataStore[targetClientId][DS_KEYS.ICE_CANDIDATE] = JSON.stringify(event.candidate);
            } else {
                __dataStore[targetClientId][DS_KEYS.ICE_CANDIDATE] = __dataStore[targetClientId][DS_KEYS.ICE_CANDIDATE] + "\n\n\n\n" + JSON.stringify(event.candidate);
            }
            if (__dataStore[targetClientId][DS_KEYS.CAN_EXCHANGE_ICE]) {
                __dataStore[targetClientId][DS_KEYS.EVENT] = EVENTS.ICE_CANDIDATE_GENERATED;
            }
        }
    };

    myPeerConnection.ondatachannel = function (event) {
        var channel = event.channel;
        dataChannels[targetClientId] = channel;
        channel.onopen = function (event) {
            console.log("HERE", targetClientId);
            __dataStore[targetClientId][DS_KEYS.EVENT] = EVENTS.HANDSHAKE_COMPLETE;
        };
        channel.onmessage = function (event) {
            console.log("received: " + event.data);
            __dataStore[targetClientId][DS_KEYS.EVENT] = EVENTS.MESSAGE_RECEIVED;
            __dataStore[targetClientId][DS_KEYS.MESSAGE] = event.data;
        };
        channel.onclose = function (event) {
            __dataStore[targetClientId][DS_KEYS.EVENT] = EVENTS.DISCONNECTED;
            if (peerConnections.hasOwnProperty(targetClientId)) {
                disconnect(targetClientId);
            }
        };
    };
    return myPeerConnection;
}

function consumeIceCandidate(targetClientId, candidateStr) {
    "use strict";
    var candidateArr = candidateStr.split("\n\n\n\n");

    candidateArr.forEach(function (c) {
        if (c.length) {
            var obj = JSON.parse(c);
            var candidate = new RTCIceCandidate(obj);
            var myPeerConnection = peerConnections[targetClientId];
            if (myPeerConnection) {
                myPeerConnection.addIceCandidate(candidate)
                    .catch(function (err) {
                        console.log(err);
                    });
            } else {
                console.log("Peer connection not found to consumeIceCandidate");
            }
        }
    });
}


function createOffer(targetClientId) {
    "use strict";
    console.log(targetClientId);
    var myPeerConnection = peerConnections[targetClientId];
    if (myPeerConnection) {
        console.log(myPeerConnection);
        __dataStore[targetClientId][DS_KEYS.EVENT] = EVENTS.HANDSHAKE_COMPLETE;
        return myPeerConnection;
    }

    myPeerConnection = _createRTCPeerConnection(targetClientId);
    peerConnections[targetClientId] = myPeerConnection;
    initDataStore(targetClientId);

    var dc = myPeerConnection.createDataChannel("sample data channel");
    dataChannels[targetClientId] = dc;

    dc.onmessage = function (event) {
        console.log("received: " + event.data);
        __dataStore[targetClientId][DS_KEYS.EVENT] = EVENTS.MESSAGE_RECEIVED;
        __dataStore[targetClientId][DS_KEYS.MESSAGE] = event.data;
    };

    dc.onopen = function () {
        console.log("HERE", targetClientId);
        __dataStore[targetClientId][DS_KEYS.EVENT] = EVENTS.HANDSHAKE_COMPLETE;
    };

    dc.onclose = function () {
        __dataStore[targetClientId][DS_KEYS.EVENT] = EVENTS.DISCONNECTED;
        if (peerConnections.hasOwnProperty(targetClientId)) {
            disconnect(targetClientId);
        }
    };

    myPeerConnection.createOffer(function (offer) {
        console.log("Created offer.");
        console.log(offer);

        myPeerConnection.setLocalDescription(offer, function () {
            var msg = {
                type: "OFFER",
                payload: {
                    sdp: offer
                },
                dst: targetClientId
            };
            __dataStore[targetClientId][DS_KEYS.OFFER] = JSON.stringify(msg);
            __dataStore[targetClientId][DS_KEYS.EVENT] = EVENTS.OFFER_GENERATED;
        }, function (err) {
            console.log("Failed to setLocalDescription, ", err);
        });
    }, function (err) {
        console.log("Failed to createOffer, ", err);
    });
}


function createAnswer(targetClientId, offerStr) {
    "use strict";
    console.log("Create answer");
    var myPeerConnection = peerConnections[targetClientId];
    if (!myPeerConnection) {
        myPeerConnection = _createRTCPeerConnection(targetClientId);
        peerConnections[targetClientId] = myPeerConnection;
        initDataStore(targetClientId);
    }
    var offer = JSON.parse(offerStr);

    var desc = new RTCSessionDescription(offer.payload.sdp);

    myPeerConnection.setRemoteDescription(desc).then(function () {
        return myPeerConnection.createAnswer();
    }).then(function (answer) {
        myPeerConnection.setLocalDescription(answer);
        __dataStore[targetClientId][DS_KEYS.EVENT] = EVENTS.ANSWER_GENRATED;
        __dataStore[targetClientId][DS_KEYS.ANSWER] = JSON.stringify(answer);
    }).catch(function (err) {
        console.error("Unable to generate answer " + err);
    });
}

function consumeAnswer(targetClientId, answerStr) {
    "use strict";
    console.log("Consume answer");
    var answer = JSON.parse(answerStr);
    var myPeerConnection = peerConnections[targetClientId];
    if (myPeerConnection) {
        myPeerConnection.setRemoteDescription(answer);
        enableIceExchange(targetClientId);
    } else {
        console.error("Peer connection not found to consumeAnswer");
    }
}