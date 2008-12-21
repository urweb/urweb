function sreturn(v) { return {v : v} }

function dyn(s) {
  var x = document.createElement("span");
  x.innerHTML = s.v;
  document.body.appendChild(x);
}
