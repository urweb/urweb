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

function pf() { alert("Pattern match failure") }

var closures = [];

function ca(f) {
  var n = closures.length;
  closures[n] = f;
  return n;
}

function cr(n) {
  return closures[n]();
}


function getXHR()
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
      else
        alert("Error querying remote server!");
    }
  };

  xhr.open("GET", uri, true);
  xhr.send(null);
}
