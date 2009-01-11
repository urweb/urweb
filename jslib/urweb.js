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

function dyn(s) {
  var x = document.createElement("span");
  x.innerHTML = s.v;
  document.body.appendChild(x);
  s.h = cons(function() { x.innerHTML = s.v }, s.h);
}

function inp(t, s) {
  var x = document.createElement(t);
  x.value = s.v;
  document.body.appendChild(x);
  s.h = cons(function() { x.value = s.v }, s.h);
  x.onkeyup = function() { sv(s, x.value) };
}

function eh(x) {
  return x.split("&").join("&amp;").split("<").join("&lt;").split(">").join("&gt;");
}

function ts(x) { return x.toString() }
function bs(b) { return (b ? "True" : "False") }

function pf() { alert("Pattern match failure") }

