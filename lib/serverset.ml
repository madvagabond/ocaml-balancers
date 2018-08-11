
open Lwt.Infix
open Util


module LoadedNode = struct
  type t = {node: Node.t; load: Counter64.t}
             
  let node t = t.node
  let load t = Counter64.get t.load

  let of_node node =
    let load = Counter64.zero () in
    {node; load}

  let make node i =
    let load = Counter64.create i in
    {node; load}

  let compare l r = Node.compare l.node r.node

                                 
                             
end



module NodeSet = Set.Make(Node) 
module LoadedSet = Set.Make(LoadedNode)

(** Maintains information to perform balancing decisions and membership *)
module type S = sig
  type elt 
  type t 

  (** A function to maintain server set retrieved from an external source*)
  val update: t -> NodeSet.t -> elt Lwt.t 

  (** Use a reference cell to maintain the ServerSet using update when an event is triggered*)
  val from_src: t -> NodeSet.t React.S.t -> t
  val of_nodes: Node.t list -> t




  val add_node: t -> Node.t -> elt Lwt.t 
  val rm_node: t -> Node.t -> elt Lwt.t

  val nodes: t -> Node.t list Lwt.t
                       
                                 
end

                  
                           


module LoadedNodes  = struct
  open LoadedNode
  type elt = LoadedSet.t 
      
  type t = LoadedSet.t SyncVar.t

  let of_list x = LoadedSet.of_list x |> SyncVar.create
                              
  let of_nodes x =
    List.map (fun n -> LoadedNode.of_node n) x |> LoadedSet.of_list |> SyncVar.create

  let to_nodes ls =
    let elements = LoadedSet.elements ls in
    List.fold_left (fun acc x -> LoadedSet.add x acc) LoadedSet.empty elements 




  let update t nodes =
    let nodes0 =
      NodeSet.elements nodes |> List.map (fun n -> LoadedNode.of_node n)  |> LoadedSet.of_list
    in
    
    let f s =                   
      let e = to_nodes s in
      
      let rm_set = LoadedSet.diff s nodes0 in
      let add_set = LoadedSet.diff nodes0 s in
      

      let v = LoadedSet.filter (fun x -> (LoadedSet.mem x rm_set) <> true) s in
      LoadedSet.union v add_set
    in
    
    SyncVar.update t f 

           
  let from_src t src =
    let e = React.S.changes src in
    React.E.map (fun nodes -> update t nodes ) e;
    t

      
 
  let add_node t node =
    let ln = LoadedNode.of_node node in
    SyncVar.update t (LoadedSet.add ln)

  let rm_node t node =
    let ln = LoadedNode.of_node node in
    SyncVar.update t (LoadedSet.remove ln)

  let nodes t =
    SyncVar.read t >|= fun e -> LoadedSet.elements e |> List.map (fun x -> x.node)

end


                       

module Nodes = struct
  type elt = NodeSet.t
  type t = NodeSet.t SyncVar.t

  let of_nodes l = NodeSet.of_list l |> SyncVar.create 

  let update t x =
    SyncVar.become t x >|= fun () -> x

  let from_src t src =
    let e = React.S.changes src in
    React.E.map (fun nodes -> update t nodes ) e;
    t



  let add_node t node =
    SyncVar.update t (NodeSet.add node)

  let rm_node t node =
    SyncVar.update t (NodeSet.remove node)

  let nodes t =
    SyncVar.read t >|= fun e -> NodeSet.elements e
    
end



module ListExt = struct

  


  let diff eq l r =
    let pred x = List.exists(fun e -> eq e x) r <> true in
    List.filter pred l

  let add_unique eq l r =
    let to_add = diff eq r l in
    l @ r

          
end



                 

module RRQueue = struct

  type elt = {nodes: Node.t list; counter: Counter32.t}
  type t = elt SyncVar.t


  let take t =
    SyncVar.read t >|= fun v ->

    let nodes = v.nodes in 
    let max = List.length nodes - 1 |> Int32.of_int in 
    let pos = Counter32.bounded_incr v.counter max in
    List.nth nodes (Int32.to_int pos) 

            
              
  
  let of_nodes nodes =
    SyncVar.create {nodes; counter= Counter32.zero ()}
                   



  let add_node t node =
    SyncVar.update t (fun v ->
        let nodes = ListExt.add_unique Node.eq v.nodes [node] in
        {v with nodes}
      )

  let rm_node t node =
    SyncVar.update t (fun v ->

        let nodes = ListExt.diff Node.eq v.nodes [node] in
        {v with nodes}
      ) 

  let update t x =
    SyncVar.update t (fun v -> {v with nodes = (NodeSet.elements x) }   )

  let from_src t src =
    let e = React.S.changes src in    
    React.E.map (update t) e;
    t


  let nodes t =
    SyncVar.read t >|= fun v -> v.nodes 
end

                  
            


