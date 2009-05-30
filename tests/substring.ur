fun main () : transaction page = return <xml>
  {[case String.split "abc{" #"{" of
        None => "!"
      | Some (pre, post) => pre ^ post]}
</xml>
