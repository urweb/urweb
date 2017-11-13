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

function toggleBtns(senderClientId, targetClientId, toEnable){
    "use strict";
    var connectBtn = document.querySelector('[data-connect="'+targetClientId+'"]');
    var disconnectBtn = document.querySelector('[data-disconnect="'+targetClientId+'"]');
    var sendMessageBtn = document.querySelector('[data-message="'+targetClientId+'"]');
    switch(toEnable){
        case true:
            if(connectBtn){
                connectBtn.style.display = "none";
            }
            if(disconnectBtn){
                disconnectBtn.style.display = "block";
            }
            if(sendMessageBtn){
                sendMessageBtn.disabled = false;
            }
            break;
        case false:
            if(connectBtn){
                connectBtn.style.display = "block";
            }
            if(disconnectBtn){
                disconnectBtn.style.display = "none";
            }
            if(sendMessageBtn){
                sendMessageBtn.disabled = true;
            }
            break;
    }
}

function onDisconnect(senderClientId, targetClientId){
    "use strict";
    console.log("Here in onDisconnect with ", senderClientId, targetClientId);
    toggleBtns(senderClientId, targetClientId, false);
    
}

function onHandshakeComplete(senderClientId, targetClientId){
    "use strict";
    console.log("Here in onConnect with ", senderClientId, targetClientId);
    toggleBtns(senderClientId, targetClientId, true);
}
