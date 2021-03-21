style st1
style st2
style st_3

fun main () = return <xml><body>
  <span title="Whoa" class={classes st1 st2}>Hi!</span>
  <span class="st-3 st2">Bye!</span>
  <span class="st1">Appendix!</span>
  <span class="">Sequel!</span>

  <span style="width: 30%">A</span>
  <span class="st-3" style="color: blue red">B</span>
  <span style="background: url(http://www.google.com/image.png)">C</span>
  <span style="background: url('http://www.google.com/image.png') red 10% 66px">D</span>
  <span style="color: red; width: 90 green; background: url(http://www.google.com/foo.jpg);">C</span>
  <span style="-moz-hyphens: auto">Properties with dashes</span>
</body></xml>
