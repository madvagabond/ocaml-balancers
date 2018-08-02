
open Lwt.Infix


module SyncVar = struct
  type 'a t = { lock: Lwt_mutex.t; mutable value: 'a}

  
                                     
  let synchronized t f =
    Lwt_mutex.with_lock t.lock f

  let become t v1 =
    Lwt_mutex.with_lock t.lock (
      fun () -> 
      t.value <- v1;
      Lwt.return_unit
    )

  let update t ufn =
    Lwt_mutex.with_lock t.lock (
      fun () ->
      let v1 = ufn t.value in
    
      t.value <- v1;
      Lwt.return v1
   )

  let get_lock t = t.lock

  let value t = t.value

  let make value =
    let lock = Lwt_mutex.create () in 
    {lock; value}
                

  module Infix = struct
    let (>>|) t f =
      synchronized t f

    let (<<) t v1 =
      become t v1

    let (>>) t f =
      update t f
   
    let (!) t =
      t.value          
  end 


                   
               
end





                         
module LoadedNode = struct

  type t = {node: Node.t; mutable load: int64}

  let incr t =
    t.load <- Int64.succ t.load

  let decr t =
    t.load <- Int64.pred t.load

  let compare l r =
    Node.compare l.node r.node

  let make node =
    let load = 0L in {node; load}

  let node t = t.node
                 

end
                      


                      
module NodeSet = Set.Make(Node)
module LoadedSet = Set.Make(LoadedNode)
                           
type loaded_nodes = LoadedSet.t SyncVar.t

type nodes = NodeSet.t SyncVar.t
                       
type rr_queue = (Node.t Queue.t) SyncVar.t



                  
            
