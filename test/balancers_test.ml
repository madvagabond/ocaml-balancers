module CH = Balancers.Chash

let string_m =
  let open Alcotest in list string 

let test_lookups () =
  let srvs = ["a"; "b"; "c"; "d"; "e" ] in
  let got = CH.slice srvs 3 3 in
 
  (Alcotest.check string_m) "test slice" ["d"; "e"; "a"] got

  

  
let test_set = ["Lookups", `Quick, test_lookups;]

let () =
  Alcotest.run "My first test" [
    "test_set", test_set;
  ]
