function onMsgReceive(targetClientId, message){
    "use strict";
    console.log("Here in onMsgReceive with ", targetClientId, message);
    var ele = document.querySelector('[data-id="'+targetClientId+'"]');
    if(ele){
        var targetHTML = ele.innerHTML;
        setInnerHTML(ele, targetHTML + "<p>RECEIVE :: "+message+"</p>");
    }

}

function onMsgSend(targetClientId, message){
    "use strict";
    console.log("Here in onMsgSend with ", targetClientId, message);
    var ele = document.querySelector('[data-id="'+targetClientId+'"]');
    if(ele){
        var targetHTML = ele.innerHTML;
        setInnerHTML(ele, targetHTML + "<p>SEND :: "+message+"</p>");
    }

}

function onDisconnect(senderClientId, targetClientId){
    "use strict";
    console.log("Here in onDisconnect with ", senderClientId, targetClientId);
    var connectBtn = document.querySelector('[data-connect="'+targetClientId+'"]');
    var disconnectBtn = document.querySelector('[data-disconnect="'+targetClientId+'"]');
    if(connectBtn){
        connectBtn.style.display = "block";
    }
    if(disconnectBtn){
        disconnectBtn.style.display = "none";
    }
}

function onHandshakeComplete(senderClientId, targetClientId){
    "use strict";
    console.log("Here in onConnect with ", senderClientId, targetClientId);
    var connectBtn = document.querySelector('[data-connect="'+targetClientId+'"]');
    var disconnectBtn = document.querySelector('[data-disconnect="'+targetClientId+'"]');
    if(connectBtn){
        connectBtn.style.display = "none";
    }
    if(disconnectBtn){
        disconnectBtn.style.display = "block";
    }
}
