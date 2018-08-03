
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

    let (<<<) t v1 =
      become t v1

    let (>>>) t f =
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

  let make node load =
    {node; load}

  let of_node node =
    let load = 0L in {node; load}
                       
  let node t = t.node
                 

end
                      


                      
module NodeSet = Set.Make(Node) 
module LoadedSet = Set.Make(LoadedNode)


module type S = sig
  type t 

  val update: t -> NodeSet.t -> t
  val resolve: t -> NodeSet.t React.S.t -> t
  val of_nodes: Node.t list -> t
                                 
end

                  
                           
type loaded_nodes = LoadedSet.t SyncVar.t

module LoadedNodes = struct
  open SyncVar.Infix

      
  type t = LoadedSet.t SyncVar.t

  let of_list x = LoadedSet.of_list x |> SyncVar.make
                              
  let of_nodes x =
    List.map (fun n -> LoadedNode.of_node n) x |> LoadedSet.of_list |> SyncVar.make

  let to_nodes ls =
    let elements = LoadedSet.elements ls in
    List.fold_left (fun acc x -> LoadedSet.add x acc) LoadedSet.empty elements 
                       
(**
  let update t nodes =
    let e =
      LoadedSet.elements (! t)
      |> fun (fun y ->  (fun x -> to_nodes) )  in
      
    let rm_set = LoadedSet.diff !t nodes in
   
    let add_set = LoadedSet.diff nodes !t in

    t >>> fun st ->
    let v1 =
      let v = LoadedSet.filter (fun x -> (LoadedSet.mem x rm_set) <> true) st in
      LoadedSet.union nodes add_set
    in
    
   **) 
    
end

type nodes = NodeSet.t SyncVar.t
                       
type rr_queue = (Node.t Queue.t) SyncVar.t

module RRQueue = struct

  let of_nodes nodes =
    let q = Queue.create () in
    List.iter (fun x -> Queue.add x q) nodes;
    SyncVar.make q

                   
end

                  
            
