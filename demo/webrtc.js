//Key-Value pair storage, both keys & values should be strings
var __dataStore = {};

var peerConnections = {};

var dataChannels = {};

function initDataStore(targetClientId) {
    __dataStore[targetClientId] = { 
            "offer" : "undefined",
            "answer" : "undefined",
            "ice-candidate" : "undefined",
            "event" : "undefined",
            "canExchangeIce" : false
    }
}

function enableIceExchange(targetClientId){
    __dataStore[targetClientId]['canExchangeIce'] = true;
    if(getDatastore(targetClientId, "ice-candidate") != "undefined"){
        __dataStore[targetClientId]["event"] = "ice-candidate-generated";
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
    return __dataStore[targetClientId]['event'];
}

function clearPendingEvent(targetClientId, eventType) {
    "use strict";
    __dataStore[targetClientId]['event'] = "undefined";
    switch(eventType){
        case "answer-generated":
            enableIceExchange(targetClientId);
            break;
        case "ice-candidate-generated":
            __dataStore[targetClientId]['ice-candidate'] = "undefined";
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
    myPeerConnection.oniceconnectionstatechange = handleICEConnectionStateChangeEvent;
    myPeerConnection.onsignalingstatechange = handleSignalingStateChangeEvent;

    myPeerConnection.onicecandidate = function (event) {
        console.log("Ice Candiate event called");
        if (event.candidate) {
            console.log("Outgoing ICE candidate: " + event.candidate.candidate);
            console.info("New ICE candidate: " + JSON.stringify(event.candidate));
            if(__dataStore[targetClientId]['ice-candidate'] == "undefined"){
                 __dataStore[targetClientId]['ice-candidate'] = JSON.stringify(event.candidate);
             }else{
                 __dataStore[targetClientId]['ice-candidate'] = __dataStore[targetClientId]['ice-candidate'] + "\n\n\n\n" + JSON.stringify(event.candidate);
             }
             if(__dataStore[targetClientId]["canExchangeIce"]){
                __dataStore[targetClientId]['event'] = 'ice-candidate-generated';
            }     
        }
    };

    myPeerConnection.ondatachannel = function (event) {
        var channel = event.channel;
        dataChannels[targetClientId] = channel;
        channel.onopen = function (event) {
            console.log("Channel Open");
            channel.send('Hi back!');
        };
        channel.onmessage = function (event) {
            console.log(event.data);
        }
    };
    return myPeerConnection;
}

function consumeIceCandidate(targetClientId, candidateStr) {
    console.log('Consume Ice candidate');
    var candidateArr = candidateStr.split("\n\n\n\n");

    candidateArr.forEach(function (c) {
            if (c.length) {
                var obj = JSON.parse(c);
                var candidate = new RTCIceCandidate(obj);
                var myPeerConnection = peerConnections[targetClientId];
                if (myPeerConnection) {
                    console.log("Adding received ICE candidate: " + JSON.stringify(candidate));
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

// function handleNegotiationNeededEvent() {
//     console.log("*** Negotiation needed");
//     console.log("---> Creating offer");
//
//     myPeerConnection.createOffer().then(function (offer) {
//         log("---> Creating new description object to send to remote peer");
//         return myPeerConnection.setLocalDescription(offer);
//     })
//         .then(function () {
//             log("---> Sending offer to remote peer");
//             // sendToServer({
//             //     name: myUsername,
//             //     target: targetUsername,
//             //     type: "video-offer",
//             //     sdp: myPeerConnection.localDescription
//             // });
//         })
//         .catch(reportError);
// }

function handleSignalingStateChangeEvent(event) {
    var myPeerConnection = event.srcElement;
    console.log("*** WebRTC signaling state changed to: " + myPeerConnection.signalingState);
    switch (myPeerConnection.signalingState) {
        case "closed":
            closeVideoCall(myPeerConnection);
            break;
    }
}

function handleICEConnectionStateChangeEvent(event) {
    console.log(event);
    var myPeerConnection = event.srcElement;
    console.log("*** ICE connection state changed to " + myPeerConnection.iceConnectionState);

    switch (myPeerConnection.iceConnectionState) {
        case "closed":
        case "failed":
        case "disconnected":
            closeVideoCall(myPeerConnection);
            break;
    }
}


function handleRemoveStreamEvent(event) {
    console.log("*** Stream removed");
    closeVideoCall();
}

function closeVideoCall(myPeerConnection) {
    log("Closing the call");

    // Close the RTCPeerConnection

    if (myPeerConnection) {
        log("--> Closing the peer connection");

        // Disconnect all our event listeners; we don't want stray events
        // to interfere with the hangup while it's ongoing.

        myPeerConnection.onaddstream = null;  // For older implementations
        myPeerConnection.ontrack = null;      // For newer ones
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
        return myPeerConnection;
    } else {
        myPeerConnection = _createRTCPeerConnection(targetClientId);
        peerConnections[targetClientId] = myPeerConnection;
        initDataStore(targetClientId);
    }

    var dc = dataChannels[targetClientId] = myPeerConnection.createDataChannel("sample data channel");
    //, {negotiated: true, id: 0});

    dc.onmessage = function (event) {
        console.log("received: " + event.data);
    };

    dc.onopen = function () {
        dc.send("sending message");
        console.log("datachannel open");
    };

    dc.onclose = function () {
        console.log("datachannel close");
    };

    myPeerConnection.createOffer(function (offer) {
        console.log('Created offer.');
        console.log(offer);

        myPeerConnection.setLocalDescription(offer, function () {
            var msg = {
                type: 'OFFER',
                payload: {
                    sdp: offer
                },
                dst: targetClientId
            };
            __dataStore[targetClientId]['offer'] = JSON.stringify(msg);
            __dataStore[targetClientId]['event'] = "offer-generated";
        }, function (err) {
            console.log('Failed to setLocalDescription, ', err);
        });
    }, function (err) {
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

    myPeerConnection.setRemoteDescription(desc).then(function () {
        return myPeerConnection.createAnswer();
    }).then(function (answer) {
        myPeerConnection.setLocalDescription(answer);
        __dataStore[targetClientId]['event'] = 'answer-generated';
        __dataStore[targetClientId]['answer'] = JSON.stringify(answer);
    }).catch(function (err) {
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