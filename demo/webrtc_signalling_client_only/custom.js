'use strict'

// var defaultConfig = {"iceServers": [{"url": "stun:stun.l.google.com:19302"}]};
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

function createRTCPeerConnection() {
    // var myPeerConnection = new RTCPeerConnection(defaultConfig, {optional: [{RtpDataChannels: true}]});
    var myPeerConnection = new RTCPeerConnection(defaultConfig, {
        mandatory: {
            OfferToReceiveAudio: true,
            OfferToReceiveVideo: true
        }
    });

    myPeerConnection.onnremovestream = handleRemoveStreamEvent;
    myPeerConnection.oniceconnectionstatechange = handleICEConnectionStateChangeEvent;
    // myPeerConnection.onicegatheringstatechange = handleICEGatheringStateChangeEvent;
    myPeerConnection.onsignalingstatechange = handleSignalingStateChangeEvent;
    // myPeerConnection.onnegotiationneeded = handleNegotiationNeededEvent;

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

function _consumeNewICECandidateMsg(myPeerConnection, msg) {
    var candidate = new RTCIceCandidate(msg);

    console.log("Adding received ICE candidate: " + JSON.stringify(candidate));
    myPeerConnection.addIceCandidate(candidate)
        .catch(function (err) {
            console.log(err);
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
            closeVideoCall();
            break;
    }
}

function handleICEConnectionStateChangeEvent(event) {
    var myPeerConnection = event.srcElement;
    console.log("*** ICE connection state changed to " + myPeerConnection.iceConnectionState);

    switch (myPeerConnection.iceConnectionState) {
        case "closed":
        case "failed":
        case "disconnected":
            closeVideoCall();
            break;
    }
}


function handleRemoveStreamEvent(event) {
    console.log("*** Stream removed");
    closeVideoCall();
}

function closeVideoCall(myPeerConnection) {
    console.log("Closing the call");

    // Close the RTCPeerConnection

    if (myPeerConnection) {
        console.log("--> Closing the peer connection");

        // Disconnect all our event listeners; we don't want stray events
        // to interfere with the hangup while it's ongoing.

        myPeerConnection.onaddstream = null;  // For older implementations
        myPeerConnection.ontrack = null;      // For newer ones
        myPeerConnection.onremovestream = null;
        myPeerConnection.onnicecandidate = null;
        myPeerConnection.oniceconnectionstatechange = null;
        myPeerConnection.onsignalingstatechange = null;
        myPeerConnection.onicegatheringstatechange = null;
        myPeerConnection.onnotificationneeded = null;


        myPeerConnection.close();
        myPeerConnection = null;
    }
}

var _createOffer = function (pc, targetClientId) {
    var defer = $.Deferred();

    var dc = window.dc = pc.createDataChannel("my data channel");//, {negotiated: true, id: 0});

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

    pc.createOffer(function (offer) {
        console.log('Created offer.');

        pc.setLocalDescription(offer, function () {
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
            defer.resolve(msg);
        }, function (err) {
            console.log('Failed to setLocalDescription, ', err);
        });
    }, function (err) {
        console.log('Failed to createOffer, ', err);
    });

    return defer.promise();
};


function _handleOffer(pc, msg) {
    var defer = $.Deferred();

    var desc = new RTCSessionDescription(msg.payload.sdp);

    pc.setRemoteDescription(desc).then(function () {
        // return navigator.mediaDevices.getUserMedia(mediaConstraints);
        // })
        // .then(function (stream) {
        //     document.getElementById("local_video").srcObject = stream;
        //     return myPeerConnection.addStream(stream);
        // })
        //     .then(function () {
        return pc.createAnswer();
    }).then(function (answer) {
        pc.setLocalDescription(answer);
        defer.resolve(answer);
    }).catch(function () {
        console.log("Some error ");
    });
    return defer.promise();
}

function _handleAnswer(pc, answer) {
    console.log("Added answer to remote description");
    pc.setRemoteDescription(answer);
}
