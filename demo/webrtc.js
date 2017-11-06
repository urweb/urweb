//Key-Value pair storage, both keys & values should be strings
var __dataStore = {
    offer: "undefined",
    answer: "undefined",
    ice: "undefined"
};

var peerConnections = {};

function myFunction(str) {
    "use strict";
    setTimeout(() => {
        __dataStore["key"] = 10;
    }, 5000);

    setInterval(() => {
        __dataStore['event'] = "Event : " + new Date();
    }, 2000);
    return ('Returning string from myFunction : ' + str);
}

function getDatastore(key) {
    "use strict";
    return __dataStore[key];
}

function getPendingEvent() {
    "use strict";
    if (__dataStore['event']) {
        return __dataStore['event']
    } else {
        return "undefined";
    }
}

function clearPendingEvent() {
    "use strict";
    delete __dataStore['event'];
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


function _createRTCPeerConnection() {
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
            if(__dataStore['ice'] == "undefined"){
                 __dataStore['ice'] = JSON.stringify(event.candidate);
             }else{
                 __dataStore['ice'] = __dataStore['ice'] + "\n\n\n\n" + JSON.stringify(event.candidate);
             }
            
            
        }
    };

    myPeerConnection.ondatachannel = function (event) {
        var channel = event.channel;
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

function consumeIceCandidate(str) {
    console.log('Consume Ice candidate');
    console.log(str);
    var targetClientId = str.split(":::")[0];
    var candidateStr = str.split(":::")[1];
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
        myPeerConnection = _createRTCPeerConnection();
        peerConnections[targetClientId] = myPeerConnection;
    }

    var dc = window.dc = myPeerConnection.createDataChannel("sample data channel");
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
                    sdp: offer,
                    // type: connection.type,
                    // label: connection.label,
                    // connectionId: connection.id,
                    // reliable: connection.reliable,
                    // serialization: connection.serialization,
                    // metadata: connection.metadata,
                    browser: window.navigator.userAgent
                },
                dst: targetClientId
            };
            __dataStore['offer'] = JSON.stringify(msg);
            __dataStore['event'] = "offer-generated";
        }, function (err) {
            console.log('Failed to setLocalDescription, ', err);
        });
    }, function (err) {
        console.log('Failed to createOffer, ', err);
    });
}


function createAnswer(str) {
    console.log('Create answer');
    console.log(str);
    var targetClientId = str.split(":::")[0];
    var myPeerConnection = peerConnections[targetClientId];
    if (!myPeerConnection) {
        myPeerConnection = peerConnections[targetClientId] = _createRTCPeerConnection();
    }
    var offer = JSON.parse(str.split(":::")[1]);

    var desc = new RTCSessionDescription(offer.payload.sdp);

    myPeerConnection.setRemoteDescription(desc).then(function () {
        return myPeerConnection.createAnswer();
    }).then(function (answer) {
        myPeerConnection.setLocalDescription(answer);
        __dataStore['event'] = 'answer-generated';
        __dataStore['answer'] = JSON.stringify(answer);
    }).catch(function (err) {
        console.error("Unable to generate answer " + err);
    });
    setTimeout(function(){ 
                    __dataStore['event'] = 'ice-candidate';
            }, 1000);
}

function consumeAnswer(str) {
    console.log('Consume answer');
    console.log(str);
    var targetClientId = str.split(":::")[0];
    var answer = JSON.parse(str.split(":::")[1]);
    var myPeerConnection = peerConnections[targetClientId];
    if (myPeerConnection) {
        myPeerConnection.setRemoteDescription(answer);
    } else {
        console.error("Peer connection not found to consumeAnswer");
    }
    setTimeout(function(){ 
                    __dataStore['event'] = 'ice-candidate';
            }, 1000);
}