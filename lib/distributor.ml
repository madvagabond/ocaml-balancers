open Serverset
open Lwt.Infix
       
open Util
       

module type S = sig
  type param
  type state
  type peer
         

  val pick: state -> param -> peer Lwt.t 
  val use: state -> param -> (Node.t -> 'a Lwt.t) -> 'a Lwt.t
                                                        
end


module type Fanout = sig
  val fanout: int
end



                       
module type Checksum = sig
  val sum64: Cstruct.t -> int64 
end 


                         

module P2C = struct

  type state = loaded_nodes
  type param = unit

  type peer = LoadedNode.t
                
                 



  open LoadedNode              
         
              
                 
(* Think about adding nth *)
  let pick state () =
    


    SyncVar.read state >>= fun set ->
    let elements = LoadedSet.elements set in 
   

    let select () =
      Random.int ( List.length elements )
      |> fun i -> List.nth elements i
    in
    
    let (a, b) =
      select (),  select ()
    in


    if a.load <= b.load then
      Lwt.return a    
    else
      Lwt.return b
      
        

                 

  let use t () f =
    pick t () >>= fun n ->  
    Counter64.incr n.load;     

    Util.ensure (f n.node) ( fun () ->
      Counter64.decr n.load; 
      ()
    )             

                
end
               


module RoundRobin = struct

  type state = rr_queue
  type param = unit
  type peer = Node.t
                

  let pick state () =
    RRQueue.take state 


                               
  let use state () f =
    pick state () >>= fun node ->

    Util.ensure (f node) (fun () ->
      RRQueue.add state node;
      ()
    )
    

    
                  
end 




                      
module CHash (C: Checksum) = struct

  type param = Cstruct.t
  type state = nodes
                 
  type peer = Node.t
                

  let pick state key =
    SyncVar.read state >>= fun s ->
    let nodes = NodeSet.elements s in
    Chash.shard nodes (C.sum64 key) |> Lwt.return 

  let use state key f =
    pick state key >>= fun node ->
    f node

      
    
end 






                               

module CHashLeastLoaded (C: Checksum) (F: Fanout) = struct
         
         

  type param = Cstruct.t
  type state = LoadedNodes.t
  type peer = LoadedNode.t


  open LoadedNode
         
  let min nodes =

    let rec aux hosts m =
      match hosts with

      | hd :: tl when m.load < hd.load ->
         aux tl hd

      | hd :: tl ->
         aux tl m

      | [] -> m

    in
    aux nodes (List.hd nodes)
                
                
    
              

  let pick state key =


    SyncVar.read state >>= fun s -> 

    let nodes =
      let a = LoadedSet.elements s in
      Chash.shards a (C.sum64 key) F.fanout
    in 

    let n = min nodes in
    Lwt.return n



               
  let use state key f =
    pick state key >>= fun n ->
    Counter64.incr n.load ;

    ensure (f n.node) (fun () ->
      Counter64.decr n.load;
      ()                 
    )
    
                
end 


                                                      
