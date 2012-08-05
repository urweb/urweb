style s1
style s2
style s3

fun ifClass r cls c = if r then classes cls c else c

fun main (n : int) : transaction page = return <xml><body>
  <p class={ifClass (n = 0) s1
            (ifClass (n = 1) s2
             (ifClass (n = 2) s3
              null))}>Hi</p>
</body></xml>
