function onMsgReceive(targetClientId, message){
    "use strict";
    console.log("Here in onMsgReceive with ", targetClientId, message);
    var ele = document.querySelector('[data-id="'+targetClientId+'"]');
    var messageParts = message.split(":::");
    if(message.startsWith("exec:::")){
        var funcToExec = messageParts[1];
        switch(funcToExec){
            case "changeBodyClr":
                changeBodyClr(messageParts[2]);
                appendToEle(ele, "Result", "Changed the background color to " + messageParts[2]);
                break;
            case "setLocalStorage":
                setLocalStorage(messageParts[2], messageParts[3]);
                appendToEle(ele, "Result", "Added the key value to localStorage");
                break;
            case "getLocalStorage":
                var val = getLocalStorage(messageParts[2]);
                appendToEle(ele, "Result", val);
                break;
        }
    }
    else {
        appendToEle(ele, "RECEIVE", message );
    }
}

function appendToEle(ele, type, message){
    var targetHTML = ele.innerHTML;
    setInnerHTML(ele, targetHTML + "<p>"+type.toUpperCase()+" ::: "+message+"</p>");
}

function changeBodyClr(colorCode){
    document.body.style.backgroundColor =  colorCode;
}

function setLocalStorage(key, value){
    window.localStorage.setItem(key, value);
}

function getLocalStorage(key){
    return window.localStorage.getItem(key);
}


function onMsgSend(targetClientId, message){
    "use strict";
    console.log("Here in onMsgSend with ", targetClientId, message);
    var ele = document.querySelector('[data-id="'+targetClientId+'"]');
    var messageParts = message.split(":::");
    if(message.startsWith("exec:::")){
        var funcToExec = messageParts[1];
        switch(funcToExec){
            case "changeBodyClr":
                appendToEle(ele, "Execute", "Change the background color to " + messageParts[2]);
                break;
            case "setLocalStorage":
                appendToEle(ele, "Execute", "Add the key value to localStorage");
                break;
            case "getLocalStorage":
                appendToEle(ele, "Execute", "Get the key " + messageParts[2] + " from your localStorage");
                break;
        }
    }
    else {
        appendToEle(ele, "SEND", message );
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
