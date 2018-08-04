
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


module type S = sig
  type t 

  val update: t -> NodeSet.t -> t
  val resolve: t -> NodeSet.t React.S.t -> t
  val of_nodes: Node.t list -> t
                                 
end

                  
                           
type loaded_nodes = LoadedSet.t SyncVar.t

module LoadedNodes = struct
      
  type t = LoadedSet.t SyncVar.t

  let of_list x = LoadedSet.of_list x |> SyncVar.create
                              
  let of_nodes x =
    List.map (fun n -> LoadedNode.of_node n) x |> LoadedSet.of_list |> SyncVar.create

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
    SyncVar.create q

  let take t =
    SyncVar.sync_effect t (fun q -> Queue.take q |> Lwt.return )
    

  let add t n =
    SyncVar.sync_effect t (fun q -> Queue.add n q |> Lwt.return )
                   
end

                  
            
