table t : {Id : int, Text : string}
  PRIMARY KEY Id

open Crud.Make(struct
                   val tab = t
                             
                   val title = "Crud3"

                   val cols = {Text = {Nam = "Text",
                                       Show = txt,
                                       Widget = (fn [nm :: Name] => <xml>
                                         <subform{nm}>
                                           <textbox{#A}/>
                                           <textbox{#B}/>
                                         </subform>
                                       </xml>),
                                       WidgetPopulated = (fn [nm :: Name] s => <xml>
                                         <subform{nm}>
                                           <textbox{#A} value={s}/>
                                           <textbox{#B}/>
                                         </subform>
                                       </xml>),
                                       Parse = (fn p : {A : string, B : string} => p.A ^ p.B),
                                       Inject = _
                                      }
                              }
               end)
