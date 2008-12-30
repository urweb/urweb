function callAll(ls) {
  for (; ls; ls = ls.next)
    ls.v();
}

function sc(v) { return {v : v, h : null} }
function sv(s, v) { s.v = v; callAll(s.h); }

function ss(s) { return s }
function sr(v) { return {v : v, h : null} }
function sb(x,y) { return {v : y(x.v).v, h : null} }

function dyn(s) {
  var x = document.createElement("span");
  x.innerHTML = s.v;
  document.body.appendChild(x);
  s.h = { n : s.h, v : function() { x.innerHTML = s.v } };
}
