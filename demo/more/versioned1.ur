open Versioned.Make(struct
                        con key = [Id = int]
                        con data = [Nam = string, ShoeSize = int]

                        val key = {Id = _}
                        val data = {Nam = {Inj = _,
                                           Eq = _},
                                    ShoeSize = {Inj = _,
                                                Eq = _}}
                    end)

fun expandKey k =
    name <- source "";
    shoeSize <- source "";
    return {Key = k, Nam = name, ShoeSize = shoeSize}

fun main () =
    ks0 <- keys;
    ks0 <- List.mapM (fn r => expandKey r.Id) ks0;
    ks <- source ks0;

    id <- source "";
    name <- source "";
    shoeSize <- source "";

    return <xml><body>
      <dyn signal={ks <- signal ks;
                   return (List.mapX (fn kr => <xml><div>
                     {[kr.Key]}:
                     <ctextbox source={kr.Nam}/>
                     <ctextbox size={5} source={kr.ShoeSize}/>
                     <button value="Latest" onclick={ro <- rpc (current {Id = kr.Key});
                                                     case ro of
                                                         None => alert "Can't get it!"
                                                       | Some r =>
                                                         set kr.Nam r.Nam;
                                                         set kr.ShoeSize (show r.ShoeSize)}/>
                     <button value="Update" onclick={name <- get kr.Nam;
                                                     shoeSize <- get kr.ShoeSize;
                                                     rpc (update {Id = kr.Key,
                                                                  Nam = name,
                                                                  ShoeSize = readError shoeSize})
                                                    }/>
                   </div></xml>) ks)}/>

      <h2>Add one:</h2>

      <table>
        <tr><th>Id:</th> <td><ctextbox size={5} source={id}/></td></tr>
        <tr><th>Name:</th> <td><ctextbox source={name}/></td></tr>
        <tr><th>Shoe size:</th> <td><ctextbox size={5} source={shoeSize}/></td></tr>
        <tr><th><button value="Add" onclick={id <- get id;
                                             name <- get name;
                                             shoeSize <- get shoeSize;
                                             rpc (insert {Id = readError id, Nam = name,
                                                          ShoeSize = readError shoeSize});

                                             cur <- get ks;
                                             kr <- expandKey (readError id);
                                             set ks (kr :: cur)}/></th></tr>
      </table>
    </body></xml>
