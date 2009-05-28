table t : {Id : int, Nam : string, Ready : bool}
  PRIMARY KEY Id

open Crud.Make(struct
                   val tab = t
                             
                   val title = "Are you ready?"

                   val cols = {Nam = Crud.string "Name",
                               Ready = {Nam = "Ready",
                                        Show = (fn b => if b then
                                                            <xml>Ready!</xml>
                                                        else
                                                            <xml>Not ready</xml>),
                                        Widget = (fn [nm :: Name] => <xml>
                                          <select{nm}>
                                            <option>Ready</option>
                                            <option>Not ready</option>
                                          </select>
                                        </xml>),
                                        WidgetPopulated = (fn [nm :: Name] b => <xml>
                                          <select{nm}>
                                            <option selected={b}>Ready</option>
                                            <option selected={not b}>Not ready</option>
                                          </select>
                                        </xml>),
                                        Parse = (fn s =>
                                                    case s of
                                                        "Ready" => True
                                                      | "Not ready" => False
                                                      | _ => error <xml>Invalid ready/not ready</xml>),
                                        Inject = _
                                       }
                              }
               end)
