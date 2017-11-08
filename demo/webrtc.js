//Key-Value pair storage, both keys & values should be strings
var __dataStore = {};
var peerConnections = {};
var dataChannels = {};

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

function initDataStore(targetClientId) {
    __dataStore[targetClientId] = {};
    __dataStore[targetClientId][DS_KEYS.OFFER] = CONSTANTS.UNDEFINED;
    __dataStore[targetClientId][DS_KEYS.ANSWER] = CONSTANTS.UNDEFINED;
    __dataStore[targetClientId][DS_KEYS.ICE_CANDIDATE] = CONSTANTS.UNDEFINED;
    __dataStore[targetClientId][DS_KEYS.EVENT] = CONSTANTS.UNDEFINED;
    __dataStore[targetClientId][DS_KEYS.CAN_EXCHANGE_ICE] = false;
    __dataStore[targetClientId][DS_KEYS.MESSAGE] = CONSTANTS.EMPTY;
}

function sendWebRTCMessage(targetClientId, message) {
    dataChannels[targetClientId].send(message);
}

function enableIceExchange(targetClientId) {
    __dataStore[targetClientId][DS_KEYS.CAN_EXCHANGE_ICE] = true;
    if (getDatastore(targetClientId, DS_KEYS.ICE_CANDIDATE) != CONSTANTS.UNDEFINED) {
        __dataStore[targetClientId][DS_KEYS.EVENT] = EVENTS.ICE_CANDIDATE_GENERATED;
    }
}

function myFunction(str) {
    "use strict";
    return ('Returning string from myFunction : ' + str);
}

function getDatastore(targetClientId, key) {
    "use strict";
    return __dataStore[targetClientId][key];
}

function getPendingEvent(targetClientId) {
    "use strict";
    return __dataStore[targetClientId][DS_KEYS.EVENT];
}

function clearPendingEvent(targetClientId, eventType) {
    "use strict";
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

var defaultConfig = {
    'iceServers': [{
        'url': 'stun:stun.l.google.com:19302'
    }, {
        'url': 'stun:stun1.l.google.com:19302'
    }, {
        'url': 'stun:stun2.l.google.com:19302'
    }, {
        'url': 'stun:stun3.l.google.com:19302'
    }, {
        'url': 'stun:stun4.l.google.com:19302'
    }]
};


function _createRTCPeerConnection(targetClientId) {
    // var myPeerConnection = new RTCPeerConnection(defaultConfig, {optional: [{RtpDataChannels: true}]});
    var myPeerConnection = new RTCPeerConnection(defaultConfig, {
        mandatory: {
            OfferToReceiveAudio: true,
            OfferToReceiveVideo: true
        }
    });

    myPeerConnection.onnremovestream = handleRemoveStreamEvent;
    myPeerConnection.oniceconnectionstatechange = function(event) {
        console.log(event);
        var myPeerConnection = event.srcElement;
        console.log("*** ICE connection state changed to " + myPeerConnection.iceConnectionState);

        switch (myPeerConnection.iceConnectionState) {
            case "closed":
            case "failed":
            case "disconnected":
                closeVideoCall(myPeerConnection);
                delete peerConnections[targetClientId];
                break;
        }
    }

    myPeerConnection.onsignalingstatechange = handleSignalingStateChangeEvent;

    myPeerConnection.onicecandidate = function(event) {
        if (event.candidate) {
            if (__dataStore[targetClientId][DS_KEYS.ICE_CANDIDATE] == CONSTANTS.UNDEFINED) {
                __dataStore[targetClientId][DS_KEYS.ICE_CANDIDATE] = JSON.stringify(event.candidate);
            } else {
                __dataStore[targetClientId][DS_KEYS.ICE_CANDIDATE] = __dataStore[targetClientId][DS_KEYS.ICE_CANDIDATE] + "\n\n\n\n" + JSON.stringify(event.candidate);
            }
            if (__dataStore[targetClientId][DS_KEYS.CAN_EXCHANGE_ICE]) {
                __dataStore[targetClientId][DS_KEYS.EVENT] = EVENTS.ICE_CANDIDATE_GENERATED;
            }
        }
    };

    myPeerConnection.ondatachannel = function(event) {
        var channel = event.channel;
        dataChannels[targetClientId] = channel;
        channel.onopen = function(event) {
            console.log("HERE", targetClientId);
            __dataStore[targetClientId][DS_KEYS.EVENT] = EVENTS.HANDSHAKE_COMPLETE;
        };
        channel.onmessage = function(event) {
            __dataStore[targetClientId][DS_KEYS.EVENT] = EVENTS.MESSAGE_RECEIVED;
            __dataStore[targetClientId][DS_KEYS.MESSAGE] = event.data;
        }
        channel.onclose = function(event) {
            __dataStore[targetClientId][DS_KEYS.EVENT] = EVENTS.DISCONNECTED;
            if (targetClientId in peerConnections) {
                disconnect(targetClientId);
            }
        }
    };
    return myPeerConnection;
}

function consumeIceCandidate(targetClientId, candidateStr) {
    var candidateArr = candidateStr.split("\n\n\n\n");

    candidateArr.forEach(function(c) {
        if (c.length) {
            var obj = JSON.parse(c);
            var candidate = new RTCIceCandidate(obj);
            var myPeerConnection = peerConnections[targetClientId];
            if (myPeerConnection) {
                myPeerConnection.addIceCandidate(candidate)
                    .catch(function(err) {
                        console.log(err);
                    });
            } else {
                console.log("Peer connection not found to consumeIceCandidate");
            }
        }
    });
}

function handleSignalingStateChangeEvent(event) {
    var myPeerConnection = event.srcElement;
    console.log("*** WebRTC signaling state changed to: " + myPeerConnection.signalingState);
    switch (myPeerConnection.signalingState) {
        case "closed":
            closeVideoCall(myPeerConnection);
            break;
    }
}


function handleRemoveStreamEvent(event) {
    console.log("*** Stream removed");
    closeVideoCall();
}

function disconnect(targetClientId) {
    closeVideoCall(peerConnections[targetClientId]);
    delete peerConnections[targetClientId];
}

function closeVideoCall(myPeerConnection) {
    console.log("Closing the call");

    // Close the RTCPeerConnection

    if (myPeerConnection) {
        console.log("--> Closing the peer connection");

        // Disconnect all our event listeners; we don't want stray events
        // to interfere with the hangup while it's ongoing.

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

function createOffer(targetClientId) {
    console.log(targetClientId);
    var myPeerConnection = peerConnections[targetClientId];
    if (myPeerConnection) {
        console.log(myPeerConnection);
        __dataStore[targetClientId][DS_KEYS.EVENT] = EVENTS.HANDSHAKE_COMPLETE;
        return myPeerConnection;
    } else {
        myPeerConnection = _createRTCPeerConnection(targetClientId);
        peerConnections[targetClientId] = myPeerConnection;
        initDataStore(targetClientId);
    }

    var dc = dataChannels[targetClientId] = myPeerConnection.createDataChannel("sample data channel");
    //, {negotiated: true, id: 0});

    dc.onmessage = function(event) {
        console.log("received: " + event.data);
        __dataStore[targetClientId][DS_KEYS.EVENT] = EVENTS.MESSAGE_RECEIVED;
        __dataStore[targetClientId][DS_KEYS.MESSAGE] = event.data;
    };

    dc.onopen = function() {
        console.log("HERE", targetClientId);
        __dataStore[targetClientId][DS_KEYS.EVENT] = EVENTS.HANDSHAKE_COMPLETE;
    };

    dc.onclose = function() {
        __dataStore[targetClientId][DS_KEYS.EVENT] = EVENTS.DISCONNECTED;
        if (targetClientId in peerConnections) {
            disconnect(targetClientId);
        }
    };

    myPeerConnection.createOffer(function(offer) {
        console.log('Created offer.');
        console.log(offer);

        myPeerConnection.setLocalDescription(offer, function() {
            var msg = {
                type: 'OFFER',
                payload: {
                    sdp: offer
                },
                dst: targetClientId
            };
            __dataStore[targetClientId][DS_KEYS.OFFER] = JSON.stringify(msg);
            __dataStore[targetClientId][DS_KEYS.EVENT] = EVENTS.OFFER_GENERATED;
        }, function(err) {
            console.log('Failed to setLocalDescription, ', err);
        });
    }, function(err) {
        console.log('Failed to createOffer, ', err);
    });
}


function createAnswer(targetClientId, offerStr) {
    console.log('Create answer');
    var myPeerConnection = peerConnections[targetClientId];
    if (!myPeerConnection) {
        myPeerConnection = peerConnections[targetClientId] = _createRTCPeerConnection(targetClientId);
        initDataStore(targetClientId);
    }
    var offer = JSON.parse(offerStr);

    var desc = new RTCSessionDescription(offer.payload.sdp);

    myPeerConnection.setRemoteDescription(desc).then(function() {
        return myPeerConnection.createAnswer();
    }).then(function(answer) {
        myPeerConnection.setLocalDescription(answer);
        __dataStore[targetClientId][DS_KEYS.EVENT] = EVENTS.ANSWER_GENRATED;
        __dataStore[targetClientId][DS_KEYS.ANSWER] = JSON.stringify(answer);
    }).catch(function(err) {
        console.error("Unable to generate answer " + err);
    });
}

function consumeAnswer(targetClientId, answerStr) {
    console.log('Consume answer');
    var answer = JSON.parse(answerStr);
    var myPeerConnection = peerConnections[targetClientId];
    if (myPeerConnection) {
        myPeerConnection.setRemoteDescription(answer);
        enableIceExchange(targetClientId);
    } else {
        console.error("Peer connection not found to consumeAnswer");
    }
}