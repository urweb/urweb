fun main () : transaction page =
    i <- fresh;
    x <- source <xml/>;
    return <xml>
      <body>
        <dyn signal={signal x} />
        <active code={set x <xml><ctextbox/><ctextbox id={i}/><ctextbox/><active code={giveFocus i; return <xml/>}/></xml>; return <xml/>}/>
      </body>
    </xml>
