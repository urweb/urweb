cookie user : {EMail : string}

fun main () =
    ro <- getCookie user;
    case ro of
        Some u => welcome u
      | _ => login ()

and welcome u = return <xml><body>
  Welcome {[u.EMail]}. <a link={logout ()}>Logout</a>
</body></xml>

and logout () =
    clearCookie user;
    main ()

and login () = return <xml><body>
  <form>E-mail:<textbox{#EMail}/><submit action={signin}/></form>
</body></xml>
   
and signin r =
    setCookie user {Value = {EMail = r.EMail},
                    Expires = None, (* Some (readError "2012-11-6
00:00:00"), *)
                    Secure = False
                   };
    main ()
