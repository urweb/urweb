// Lists

function cons(v, ls) {
  return { next : ls, data : v };
}
function concat(ls1, ls2) {
  return (ls1 ? cons(ls1.data, concat(ls1.next, ls2)) : ls2);
}
function member(x, ls) {
  for (; ls; ls = ls.next)
    if (ls.data == x)
      return true;
  return false;
}
function remove(x, ls) {
  return (ls ? (ls.data == x ? ls.next : cons(ls.data, remove(x, ls.next))) : null);
}
function union(ls1, ls2) {
  return (ls1 ? (member(ls1.data, ls2) ? union(ls1.next, ls2) : cons(ls1.data, union(ls1.next, ls2))) : ls2);
}
function length(ls) {
  return (ls ? 1 + length(ls.next) : 0);
}


// Error handling

function whine(msg) {
  alert(msg);
  throw msg;
}

function pf(loc) {
  whine("Pattern match failure (" + loc + ")");
}

function runHandlers(kind, ls, arg) {
  if (ls == null)
    alert(kind + ": " + arg);
  for (; ls; ls = ls.next)
    try {
      ls.data(arg)(null);
    } catch (v) { }
}

var errorHandlers = null;

function onError(f) {
  errorHandlers = cons(f, errorHandlers);
}

function er(s) {
  runHandlers("Error", errorHandlers, s);
  throw {uw_error: s};
}

var failHandlers = null;

function onFail(f) {
  failHandlers = cons(f, failHandlers);
}

function doExn(v) {
  if (v == null || v.uw_error == null) {
    var s = (v == null ? "null" : v.toString());
    runHandlers("Fail", failHandlers, s);
  }
}

var disconnectHandlers = null;

function onDisconnect(f) {
  disconnectHandlers = cons(function (_){return f}, disconnectHandlers);
}

function discon() {
  runHandlers("Disconnect", disconnectHandlers, null);
}

var connectHandlers = null;

function onConnectFail(f) {
  connectHandlers = cons(function (_){return f}, connectHandlers);
}

function conn() {
  runHandlers("Connect", connectHandlers, null);
}

var serverHandlers = null;

function onServerError(f) {
  serverHandlers = cons(f, serverHandlers);
}

function servErr(s) {
  runHandlers("Server", serverHandlers, s);
}


// Embedding closures in XML strings

function cs(f) {
  return {closure: f};
}

function isWeird(v) {
  return v.closure != null || v.cat1 != null;
}

function cat(s1, s2) {
  if (isWeird(s1) || isWeird(s2))
    return {cat1: s1, cat2: s2};
  else
    return s1 + s2;
}

var closures = [];
var freeClosures = null;

function newClosure(f) {
  var n;
  if (freeClosures == null) {
    n = closures.length;
  } else {
    n = freeClosures.data;
    freeClosures = freeClosures.next;
  }
  closures[n] = f;
  return n;
}

function freeClosure(n) {
  closures[n] = null;
  freeClosures = cons(n, freeClosures);
}

function cr(n) {
  return closures[n]();
}

function flatten(cls, tr) {
  if (tr.cat1 != null)
    return flatten(cls, tr.cat1) + flatten(cls, tr.cat2);
  else if (tr.closure != null) {
    var cl = newClosure(tr.closure);
    cls.v = cons(cl, cls.v);
    return "cr(" + cl + ")";
  } else
    return tr;
}

function flattenLocal(s) {
  var cls = {v : null};
  var r = flatten(cls, s);
  for (cl = cls.v; cl != null; cl = cl.next)
    freeClosure(cl.data);
  return r;
}



// Dynamic tree management

function populate(node) {
  var s = node.signal;
  var oldSources = node.sources;
  try {
    var sr = s();
    var newSources = sr.sources;

    for (var sp = oldSources; sp; sp = sp.next)
      if (!member(sp.data, newSources))
        sp.data.dyns = remove(node, sp.data.dyns);

    for (var sp = newSources; sp; sp = sp.next)
      if (!member(sp.data, oldSources))
        sp.data.dyns = cons(node, sp.data.dyns);

    node.sources = newSources;
    node.recreate(sr.data);
  } catch (v) {
    doExn(v);
  }
}

function sc(v) {
  return {data : v, dyns : null};
}
function sv(s, v) {
  s.data = v;
  for (var ls = s.dyns; ls; ls = ls.next)
    if (!ls.dead)
      populate(ls.data);
}
function sg(s) {
  return s.data;
}

function ss(s) {
  return function() { return {sources : cons(s, null), data : s.data } };
}
function sr(v) {
  return function() { return {sources : null, data : v } };
}
function sb(x,y) {
  return function() {
    var xr = x();
    var yr = y(xr.data)();
    return {sources : union(xr.sources, yr.sources), data : yr.data};
  };
}

function lastParent() {
  var pos = document;

  while (pos.lastChild && pos.lastChild.nodeType == 1)
    pos = pos.lastChild;

  return pos.parentNode;
}

function addNode(node) {
  if (thisScript) {
    thisScript.parentNode.appendChild(node);
    thisScript.parentNode.removeChild(thisScript);
  } else
    lastParent().appendChild(node);
}

function setHTML(html) {
  var x = document.createElement("span");
  x.dead = false;
  x.signal = null;
  x.sources = null;
  x.closures = null;
  x.innerHTML = html;
  addNode(x);
  runScripts(x);
  alert("HTML:\n" + html);
}  

var thisScript = null;

function runScripts(node) {
  var savedScript = thisScript;

  var scripts = node.getElementsByTagName("script"), scriptsCopy = [];
  var len = scripts.length;
  for (var i = 0; i < len; ++i)
    scriptsCopy[i] = scripts[i];
  for (var i = 0; i < len; ++i) {
    thisScript = scriptsCopy[i];
    try {
      eval(thisScript.textContent);
    } catch (v) {
      doExn(v);
    }
  }

  thisScript = savedScript;
}


// Dynamic tree entry points

var dynDepth = 0;

function dyn(s) {
  var x = document.createElement("span");
  x.dead = false;
  x.signal = s;
  x.sources = null;
  x.closures = null;
  x.recreate = function(v) {
    for (var ls = x.closures; ls; ls = ls.next)
      freeClosure(ls.data);

    var spans = x.getElementsByTagName("span");
    for (var i = 0; i < spans.length; ++i) {
      var span = spans[i];
      span.dead = true;
      for (var ls = span.sources; ls; ls = ls.next)
        ls.data.dyns = remove(span, ls.data.dyns);
      for (var ls = span.closures; ls; ls = ls.next)
        freeClosure(ls.data);
    }

    var cls = {v : null};
    x.innerHTML = flatten(cls, v);
    x.closures = cls.v;
    runScripts(x);
  };
  addNode(x);
  populate(x);
}

function inp(t, s, content) {
  var x = document.createElement(t);
  x.dead = false;
  x.signal = ss(s);
  x.sources = null;
  x.recreate = function(v) { if (x.value != v) x.value = v; };
  populate(x);
  addNode(x);
  if (t == "select") {
    x.innerHTML = content;
    x.value = s.data;
    x.onchange = function() { sv(s, x.value) };
  } else {
    x.value = s.data;
    x.onkeyup = function() { sv(s, x.value) };
  }

  return x;
}

function addOnChange(x, f) {
  var old = x.onchange;
  x.onchange = function() { old(); f (); };
}


// Basic string operations

function eh(x) {
  if (x == null)
    return "NULL";
  else
    return x.split("&").join("&amp;").split("<").join("&lt;").split(">").join("&gt;");
}

function ts(x) { return x.toString() }
function bs(b) { return (b ? "True" : "False") }

function pi(s) {
  var r = parseInt(s);
  if (r.toString() == s)
    return r;
  else
    er("Can't parse int: " + s);
}

function pfl(s) {
  var r = parseFloat(s);
  if (r.toString() == s)
    return r;
  else
    er("Can't parse float: " + s);
}

function uf(s) {
  return escape(s).replace(new RegExp ("/", "g"), "%2F");
}

function uu(s) {
  return unescape(s).replace(new RegExp ("\\+", "g"), " ");
}



// Remote calls

var client_id = null;
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

var sig = null;

function requestUri(xhr, uri, needsSig) {
  xhr.open("GET", uri, true);

  if (client_id != null) {
    xhr.setRequestHeader("UrWeb-Client", client_id.toString());
    xhr.setRequestHeader("UrWeb-Pass", client_pass.toString());
  }

  if (needsSig) {
    if (sig == null)
      whine("Missing cookie signature!");

    xhr.setRequestHeader("UrWeb-Sig", sig);
  }

  xhr.send(null);
}

function rc(uri, parse, k, needsSig) {
  uri = flattenLocal(uri);
  var xhr = getXHR();

  xhr.onreadystatechange = function() {
    if (xhr.readyState == 4) {
      var isok = false;

      try {
        if (xhr.status == 200)
          isok = true;
      } catch (e) { }

      if (isok) {
        try {
          k(parse(xhr.responseText));
        } catch (v) {
          doExn(v);
        }
      } else {
        conn();
      }
    }
  };

  requestUri(xhr, uri, needsSig);
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
    q.back.next = node;
    q.back = node;
  }
}
function dequeue(q) {
  if (q.front == null)
    return null;
  else {
    var r = q.front.data;
    q.front = q.front.next;
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
    requestUri(xhr, uri, false);
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
        if (lines.length < 2) {
          discon();
          return;
        }

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
            try {
              listener(msg);
            } catch (v) {
              doExn(v);
            }
          }
        }

        connect();
      }
      else {
        try {
          servErr("Error querying remote server for messages: " + xhr.status);
        } catch (e) { servErr("Error querying remote server for messages"); }
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
  if (chn == null)
    return;

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
    try {
      k(parse(msg))(null);
    } catch (v) {
      doExn(v);
    }
  }
}


// App-specific code

