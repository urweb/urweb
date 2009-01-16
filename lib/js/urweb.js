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

function lastParent(pos) {
  while (pos.lastChild && pos.lastChild.nodeType == 1)
    pos = pos.lastChild;

  return pos.parentNode;
}

var parents = null;

function pushParent(node) {
  parents = cons(node, parents);
}

function popParent() {
  if (parents)
    parents = parents.n;
  else
    alert("popParent: stack underflow");
}

function curParent() {
  return lastParent(parents ? parents.v : document);
}

function populate(node, html) {
  node.innerHTML = html;

  var scripts = node.getElementsByTagName("script");
  var len = scripts.length;
  for (var i = 0; i < len; ++i) {
    pushParent(scripts[i].parentNode);
    eval(scripts[i].textContent);
    popParent();
  }
}

function dyn(s) {
  var x = document.createElement("span");
  x.innerHTML = s.v;
  curParent().appendChild(x);
  s.h = cons(function() { populate(x, s.v) }, s.h);
}

function inp(t, s) {
  var x = document.createElement(t);
  x.value = s.v;
  curParent().appendChild(x);
  s.h = cons(function() { x.value = s.v }, s.h);
  x.onkeyup = function() { sv(s, x.value) };
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

