open Lwt.Infix
    
       
module type ConnectionManager = sig
  
  type t
  include Mirage_flow_lwt.S

  val connect: t -> Node.t ->  flow Lwt.t          
end 


module type S = sig

  type t
  type flow
  type param

  val use: t -> param -> (flow -> 'a Lwt.t) -> 'a Lwt.t
                                                  
end 

module Make (C: ConnectionManager) (D: Distributor.S) = struct
  type t = {
      cm: C.t;
      server_set: D.state
    }


  type flow = C.flow
  type param = D.param


  let use t param f =

    let handle x =
      C.connect t.cm x >>= fun flow ->
      f flow
    in
    
        
    D.use t.server_set param handle

          
end 


                                                                                                                            
module Distributor = Distributor
module Node = Node
module Serverset = Serverset
module Chash = Chash
                 
