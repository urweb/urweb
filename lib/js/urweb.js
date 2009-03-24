function cons(v, ls) {
  return { n : ls, v : v };
}

function callAll(ls) {
  for (; ls; ls = ls.n)
    ls.v();
}

function sc(v) {
  return {v : v, h : null};
}
function sv(s, v) {
  s.v = v;
  callAll(s.h);
}
function sg(s) {
  return s.v;
}

function ss(s) {
  return s;
}
function sr(v) {
  return {v : v, h : null};
}
function sb(x,y) {
  var z = y(x.v);
  var s = {v : z.v, h : null};

  function reZ() {
    z.h = cons(function() { s.v = z.v; callAll(s.h); }, z.h);    
  }

  x.h = cons(function() { z = y(x.v); reZ(); s.v = z.v; callAll(s.h); }, x.h);
  reZ();

  return s;
}

function lastParent() {
  var pos = document;

  while (pos.lastChild && pos.lastChild.nodeType == 1)
    pos = pos.lastChild;

  return pos.parentNode;
}

var thisScript = null;

function addNode(node) {
  if (thisScript) {
    thisScript.parentNode.appendChild(node);
    thisScript.parentNode.removeChild(thisScript);
  } else
    lastParent().appendChild(node);
}

function runScripts(node) {
  var savedScript = thisScript;

  var scripts = node.getElementsByTagName("script"), scriptsCopy = {};
  var len = scripts.length;
  for (var i = 0; i < len; ++i)
    scriptsCopy[i] = scripts[i];
  for (var i = 0; i < len; ++i) {
    thisScript = scriptsCopy[i];
    eval(thisScript.textContent);
  }

  thisScript = savedScript;
}

function populate(node, html) {
  node.innerHTML = html;
  runScripts(node);
}

function dyn(s) {
  var x = document.createElement("span");
  populate(x, s.v);
  addNode(x);
  s.h = cons(function() { populate(x, s.v) }, s.h);
}

function inp(t, s) {
  var x = document.createElement(t);
  x.value = s.v;
  addNode(x);
  s.h = cons(function() { x.value = s.v }, s.h);
  x.onkeyup = function() { sv(s, x.value) };
  return x;
}

function eh(x) {
  return x.split("&").join("&amp;").split("<").join("&lt;").split(">").join("&gt;");
}

function ts(x) { return x.toString() }
function bs(b) { return (b ? "True" : "False") }

function pi(s) {
  var r = parseInt(s);
  if (r.toString() == s)
    return r;
  else
    throw "Can't parse int: " + s;
}

function pfl(s) {
  var r = parseFloat(s);
  if (r.toString() == s)
    return r;
  else
    throw "Can't parse float: " + s;
}

function whine(msg) {
  alert(msg);
  throw msg;
}

function pf() {
  whine("Pattern match failure");
}

var closures = [];

function ca(f) {
  var n = closures.length;
  closures[n] = f;
  return n;
}

function cr(n) {
  return closures[n]();
}


var client_id = 0;
var client_pass = 0;
var url_prefix = "/";
var timeout = 60;

function getXHR(uri)
{
  try {
    return new XMLHttpRequest();
  } catch (e) {
    try {
     return new ActiveXObject("Msxml2.XMLHTTP");
    } catch (e) {
      try {
        return new ActiveXObject("Microsoft.XMLHTTP");
      } catch (e) {
        throw "Your browser doesn't seem to support AJAX.";
      }
    }
  }
}

function requestUri(xhr, uri) {
  xhr.open("GET", uri, true);

  if (client_id != 0) {
    xhr.setRequestHeader("UrWeb-Client", client_id.toString());
    xhr.setRequestHeader("UrWeb-Pass", client_pass.toString());
  }

  xhr.send(null);
}

function rc(uri, parse, k) {
  var xhr = getXHR();

  xhr.onreadystatechange = function() {
    if (xhr.readyState == 4) {
      var isok = false;

      try {
        if (xhr.status == 200)
          isok = true;
      } catch (e) { }

      if (isok)
        k(parse(xhr.responseText));
      else {
        whine("Error querying remote server!");
      }
    }
  };

  requestUri(xhr, uri);
}

function path_join(s1, s2) {
  if (s1.length > 0 && s1[s1.length-1] == '/')
    return s1 + s2;
  else
    return s1 + "/" + s2;
}

var channels = [];

function newQueue() {
  return { front : null, back : null };
}
function enqueue(q, v) {
  if (q.front == null) {
    q.front = cons(v, null);
    q.back = q.front;
  } else {
    var node = cons(v, null);
    q.back.n = node;
    q.back = node;
  }
}
function dequeue(q) {
  if (q.front == null)
    return null;
  else {
    var r = q.front.v;
    q.front = q.front.n;
    if (q.front == null)
      q.back = null;
    return r;
  }
}

function newChannel() {
  return { msgs : newQueue(), listeners : newQueue() };
}

function listener() {
  var uri = path_join(url_prefix, ".msgs");
  var xhr = getXHR();
  var tid, orsc, onTimeout;

  var connect = function () {
    xhr.onreadystatechange = orsc;
    tid = window.setTimeout(onTimeout, timeout * 500);
    requestUri(xhr, uri);
  }

  orsc = function() {
    if (xhr.readyState == 4) {
      window.clearTimeout(tid);

      var isok = false;

      try {
        if (xhr.status == 200)
          isok = true;
      } catch (e) { }

      if (isok) {
        var lines = xhr.responseText.split("\n");
        if (lines.length < 2) 
          throw "Empty message from remote server";

        for (var i = 0; i+1 < lines.length; i += 2) {
          var chn = lines[i];
          var msg = lines[i+1];

          if (chn < 0)
            whine("Out-of-bounds channel in message from remote server");

          var ch;

          if (chn >= channels.length || channels[chn] == null) {
            ch = newChannel();
            channels[chn] = ch;
          } else
            ch = channels[chn];

          var listener = dequeue(ch.listeners);
          if (listener == null) {
            enqueue(ch.msgs, msg);
          } else {
            listener(msg);
          }
        }

        connect();
      }
      else {
        try {
          whine("Error querying remote server for messages! " + xhr.status);
        } catch (e) { }
      }
    }
  };

  onTimeout = function() {
    xhr.abort();
    connect();
  };

  connect();
}

function rv(chn, parse, k) {
  if (chn < 0)
    whine("Out-of-bounds channel receive");

  var ch;

  if (chn >= channels.length || channels[chn] == null) {
    ch = newChannel();
    channels[chn] = ch;
  } else
    ch = channels[chn];

  var msg = dequeue(ch.msgs);
  if (msg == null) {
    enqueue(ch.listeners, function(msg) { k(parse(msg))(null); });
  } else {
    k(parse(msg))(null);
  }
}

function unesc(s) {
  return unescape(s).replace("+", " ");
}
