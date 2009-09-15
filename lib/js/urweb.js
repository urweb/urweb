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
  throw ("Pattern match failure (" + loc + ")");
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
  window.setTimeout(function () { runHandlers("Server", serverHandlers, s); }, 0);
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
function scur(s) {
  return s().data;
}

function lastParent() {
  var pos = document;

  while (pos.lastChild && pos.lastChild.nodeType == 1)
    pos = pos.lastChild;

  return pos.parentNode;
}

function parent() {
  return thisScript ? thisScript.parentNode : lastParent();
}

function addNode(node) {
  if (thisScript)
    thisScript.parentNode.replaceChild(node, thisScript);
  else
    lastParent().appendChild(node);
}

var thisScript = null;

function runScripts(node) {
  if (node.getElementsByTagName) {
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
      if (thisScript.parentNode)
        thisScript.parentNode.removeChild(thisScript);
    }

    thisScript = savedScript;
  }
}


// Dynamic tree entry points

function dyn(pnode, s) {
  var x = document.createElement("script");
  x.dead = false;
  x.signal = s;
  x.sources = null;
  x.closures = null;

  var firstChild = null;

  x.recreate = function(v) {
    for (var ls = x.closures; ls; ls = ls.next)
      freeClosure(ls.data);

    var next;
    for (var child = firstChild; child && child != x; child = next) {
      next = child.nextSibling;
      if (child.getElementsByTagName) {
        var arr = child.getElementsByTagName("script");
        for (var i = 0; i < arr.length; ++i) {
          var span = arr[i];
          span.dead = true;
          for (var ls = span.sources; ls; ls = ls.next)
            ls.data.dyns = remove(span, ls.data.dyns);
          for (var ls = span.closures; ls; ls = ls.next)
            freeClosure(ls.data);
        }
      }

      if (child.parentNode)
        child.parentNode.removeChild(child);
    }

    var cls = {v : null};
    var html = flatten(cls, v);
    x.closures = cls.v;

    if (pnode == "table") {
      var dummy = document.createElement("body");
      dummy.innerHTML = "<table>" + html + "</table>";
      runScripts(dummy);
      var table = x.parentNode;

      if (table) {
        var arr = dummy.getElementsByTagName("tbody");
        firstChild = null;
        var tbody;
        if (arr.length > 0 && table != null)
          tbody = arr[0];
        else
          tbody = dummy.firstChild;

        var next;
        firstChild = document.createElement("script");
        table.insertBefore(firstChild, x);
        for (var node = tbody.firstChild; node; node = next) {
          next = node.nextSibling;
          table.insertBefore(node, x);
        }
      }
    } else if (pnode == "tr") {
      var dummy = document.createElement("body");
      dummy.innerHTML = "<table><tr>" + html + "</tr></table>";
      runScripts(dummy);
      var table = x.parentNode;

      if (table) {
        var arr = dummy.getElementsByTagName("tr");
        firstChild = null;
        var tbody;
        if (arr.length > 0 && table != null)
          tbody = arr[0];
        else
          tbody = dummy.firstChild;

        var next;
        firstChild = document.createElement("script");
        table.insertBefore(firstChild, x);
        for (var node = tbody.firstChild; node; node = next) {
          next = node.nextSibling;
          table.insertBefore(node, x);
        }
      }
    } else {
      firstChild = document.createElement("span");
      firstChild.innerHTML = html;
      runScripts(firstChild);
      if (x.parentNode)
        x.parentNode.insertBefore(firstChild, x);
    }
  };

  addNode(x);
  populate(x);
}

function input(t, s, recreate) {
  var x = document.createElement(t);
  x.dead = false;
  x.signal = ss(s);
  x.sources = null;
  x.recreate = recreate(x);
  addNode(x);
  populate(x);

  return x;
}

function inp(s) {
  var x = input("input", s, function(x) { return function(v) { if (x.value != v) x.value = v; }; });
  x.value = s.data;
  x.onkeyup = function() { sv(s, x.value) };

  return x;
}

function sel(s, content) {
  var x = input("select", s, function(x) { return function(v) { if (x.value != v) x.value = v; }; });
  x.innerHTML = content;
  x.value = s.data;
  if (x.value != s.data)
    sv(s, x.value);
  x.onchange = function() { sv(s, x.value) };

  return x;
}

function chk(s) {
  var x = input("input", s, function(x) { return function(v) { if (x.checked != v) x.checked = v; }; });
  x.type = "checkbox";
  x.checked = s.data;
  x.onchange = function() { sv(s, x.checked) };

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

function sub(s, i) { return s[i]; }
function suf(s, i) { return s.substring(i); }
function slen(s) { return s.length; }
function sidx(s, ch) {
  var r = s.indexOf(ch);
  if (r == -1)
    return null;
  else
    return r;
}
function sspn(s, chs) {
  for (var i = 0; i < s.length; ++i)
    if (chs.indexOf(s[i]) != -1)
      return i;

  return null;
}
function schr(s, ch) {
  var r = s.indexOf(ch);
  if (r == -1)
    return null;
  else
    return s.substring(r);
}
function ssub(s, start, len) {
  return s.substring(start, start+len);
}

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

function pio(s) {
  var r = parseInt(s);
  if (r.toString() == s)
    return r;
  else
    return null;
}

function pflo(s) {
  var r = parseFloat(s);
  if (r.toString() == s)
    return r;
  else
    return null;
}

function uf(s) {
  if (s.length == 0)
    return "_";
  return (s[0] == '_' ? "_" : "")
        + escape(s).replace(new RegExp ("/", "g"), "%2F").replace(new RegExp ("\\+", "g"), "%2B");
}

function uu(s) {
  if (s.length > 0 && s[0] == '_')
    s = s.substring(1);
  else if (s.length >= 3 && s[0] == '%' && s[1] == '5' && (s[2] == 'f' || s[2] == 'F'))
    s = s.substring(3);
  return unescape(s.replace(new RegExp ("\\+", "g"), " "));
}

function ub(b) {
  return b ? "1" : "0";
}

function uul(getToken, getData) {
  var tok = getToken();
  if (tok == "Nil") {
    return null;
  } else if (tok == "Cons") {
    var d = getData();
    var l = uul(getToken, getData);
    return {_1:d, _2:l};
  } else
    throw ("Can't unmarshal list (" + tok + ")");
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
        var text = xhr.responseText
        if (text == "")
          return;
        var lines = text.split("\n");

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
          if (xhr.status != 0)
            servErr("Error querying remote server for messages: " + xhr.status);
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


// Key events

function kc(e) {
  return window.event ? e.keyCode : e.which;
}


// App-specific code

