module type INT = sig
  type t 

  val succ: t -> t
  val pred: t -> t
  val zero: t
              
end 


module type COUNTER = sig
  type elt
  type t = elt ref


  val zero: unit -> t
  val create: elt -> t

                      
  val incr: t -> elt
  val decr: t -> elt

  val get: t -> elt
  val set: t -> elt -> elt 
end
                    
                    
module Counter (I: INT) = struct
  type elt = I.t
  type t = elt ref

  let zero () =
    ref I.zero 

  let create i =
    ref i 

        
  let cas t old n =
    if !t = old then

      let () = t := n in
      true 

    else 
      false
        

  let rec incr t =
    let old = !t in
    let n = I.succ old in

    let b = cas t old n in
    

    if b = true then n
    else incr t


  let rec decr t =

    let old = !t in
    let n = I.pred old in
    
    let b = cas t old n in
    

    if b = true then n
    else incr t


  let rec get t =
    let old = !t in

    if old = !t then old
    else get t
             

         
  let rec set t n =
    let old = !t in
    let p = cas t old n in

    if p = true then n
    else set t n 
      
    
end 


module Counter64 = Counter(Int64)
module Counter32 = Counter(Int32)


                     
module SyncVar = struct
  type 'a t = { lock: Lwt_mutex.t; mutable value: 'a}

  let create value =
    let lock = Lwt_mutex.create () in
    {lock; value}
                
  let rec read t =
    let pred = Lwt_mutex.is_locked t.lock in

    if pred = false then
      Lwt.return t.value

    else
      read t



  let become t v1 =
    Lwt_mutex.with_lock t.lock ( fun x ->
      t.value <- v1;
      Lwt.return_unit                            
    )


  let sync t f =
    Lwt_mutex.with_lock t.lock f

  let sync_effect t f =
    Lwt_mutex.with_lock t.lock (fun () -> f t.value) 

                        
  let update t ufn =
    Lwt_mutex.with_lock t.lock ( fun () ->
      let v1 = ufn t.value in
      t.value <- v1;
      Lwt.return v1 
    )

                        
end


let ensure t f =
  Lwt.on_any t (fun _ -> f () ) (fun _ -> f () );
  t 

    
