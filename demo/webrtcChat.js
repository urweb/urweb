function onMsgReceive(targetClientId, message){
    "use strict";
    console.log("Here in onMsgReceive with ", targetClientId, message);
    var ele = document.querySelector('[data-id="'+targetClientId+'"]');
    if(ele){
        var targetHTML = ele.innerHTML;
        setInnerHTML(ele, targetHTML + "<p>RECEIVE :: "+message+"</p>");
    }

}