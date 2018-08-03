open Serverset
open Serverset.SyncVar.Infix
open Lwt.Infix
       
       

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
    let elements = (! state) |> LoadedSet.elements in 

    let select () =
      Random.int ( LoadedSet.cardinal (! state) )
      |> fun i -> List.nth elements i
    in
    
    let (a, b) =
      select (),  select ()
    in


    if a.load <= b.load then
      Lwt.return a    
    else
      Lwt.return b
      
        


  let use state () f =
    pick state () >>= fun n ->  
    state >>| fun () ->

    incr n;
    f n.node >|= fun out ->
    decr n;
    
    out
      
              
    
end
               


module RoundRobin = struct

  type state = rr_queue
  type param = unit
  type peer = Node.t
                

  let pick state () =
    state >>| fun () ->
    ! state |> Queue.take |> Lwt.return


                               
  let use state () f =
    pick state () >>= fun node ->
    f node >>= fun out ->

    state >>| fun () ->
    (! state) |> Queue.add node; 
    Lwt.return out
    

    
                  
end 




                      
module CHash (C: Checksum) = struct

  type param = Cstruct.t
  type state = nodes
                 
  type peer = Node.t
                

  let pick state key =
    let nodes = (! state) |> NodeSet.elements in
    Chash.shard nodes (C.sum64 key) |> Lwt.return 

  let use state key f =
    pick state key >>= fun node ->
    f node

      
    
end 






                               

module CHashLeastLoaded (C: Checksum) (F: Fanout) = struct
  open LoadedNode
         

  type param = Cstruct.t
  type state = loaded_nodes
  type peer = LoadedNode.t


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

    let nodes =
      let a = LoadedSet.elements (! state) in
      Chash.shards a (C.sum64 key) F.fanout
    in

    let n = min nodes in
    Lwt.return n



               
  let use state key f =
    pick state key >>= fun ln ->
    


    state >>| fun () ->

    incr ln; 
    f ln.node >>= fun out ->
    decr ln;
    
    Lwt.return out 
    
                
end 


                                                      
