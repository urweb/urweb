fun main () =
    s <- source "";
    return <xml><body>
      <cselect source={s}>
        <coption>Wilbur</coption>
        <coption>Walbur</coption>
      </cselect>

      Hello, I'm <dyn signal={s <- signal s; return <xml>{[s]}</xml>}/>.
      I'll be your waiter for this evening.
    </body></xml>
