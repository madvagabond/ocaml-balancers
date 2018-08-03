open Balancer
open Lwt.Infix
       

open Serverset
open Distributor
       
module CH = Balancer.Chash

let string_m =
  let open Alcotest in list string 


let make_node port =
  let host = "localhost" in 
  Node.make ~host ~port ()

let make_loaded port i =
  make_node port |> (fun n -> LoadedNode.make n i)


let node =
  let pp = Fmt.of_to_string Node.to_string in
  let eq l r = (Node.compare l r) = 0 in 
  Alcotest.testable pp eq


let loaded_node =
  let pp = Fmt.of_to_string (fun ln -> LoadedNode.node ln |> Node.to_string ) in
  let eq l r = LoadedNode.compare l r = 0 in
  Alcotest.testable pp eq


let free () = Lwt.return_unit
                
let use_switch switch =
  Lwt_switch.add_hook (Some switch) free




                               
let random_host_port () =

  let host = "localhost" in
  let (min, max) = 1024, 65535 in 
  let port = min + ( Random.int (max - min) ) in

  host, port 


          
let random_node () =
  let (host, port) = random_host_port () in
  Node.make ~host ~port () 


            
                               
let make_nodes n =
  let rec aux i l =
    if (i > 0) then

      let n = random_node () in
      aux (i - 1) (l @ [n])

    else
      l

  in
  aux n []
        
                

                      
let test_p2c switch () =
  use_switch switch;
  
  let host = "127.0.0.1" in
  
  let (v1, v2) = (make_loaded 4000 0L), (make_loaded 4001 1L) in
  
  let srv =  LoadedNodes.of_list [v1; v2] in 
  Distributor.P2C.pick srv () >|= fun got -> 
  (Alcotest.check loaded_node) "node is uneqal" v1 got 










                               
let test_chash () =
  let srvs = ["a"; "b"; "c"; "d"; "e" ] in
  let got = CH.slice srvs 3 3 in
 
  (Alcotest.check string_m) "test slice" ["d"; "e"; "a"] got


      
let test_rr switch () =
  use_switch switch;

  let peers = make_nodes 5 in
  let expected = List.hd peers in

  let ss = RRQueue.of_nodes peers in 
  RoundRobin.pick ss () >>= fun got -> 
  Alcotest.check node "Nodes didn't match" expected got;

  let expect_n = List.nth peers 1 in

  RoundRobin.pick ss () >|= fun got -> 
  
  Alcotest.check node "Doesn't exibit FIFO properties" expect_n got
             
   
 
let distributors = [
    "Chash", `Quick, test_chash;
    Alcotest_lwt.test_case "P2C" `Quick test_p2c;
    Alcotest_lwt.test_case "Round Robin" `Quick test_rr; 
  ]

let () =
  Alcotest.run "Testing Balancer" [
    "Testing Load Distributors", distributors;
  ]
