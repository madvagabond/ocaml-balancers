module type ConnectionManager = sig
  
  type t
  include Mirage_flow_lwt

  val connect: t -> Node.t ->  flow Lwt.t          
end 


module type S = sig

  type t
  type flow
  type param

  val use: t -> param -> (flow -> 'a Lwt.t) -> 'a Lwt.t
                                                  
end 



module Make: functor (C: ConnectionManager) (D: Distributor) -> S
                                                                  
