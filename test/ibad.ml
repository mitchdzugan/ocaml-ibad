open Ibad.Lens
open VL

type address = {
  street : string;
  number : int;
  postcode : string;
}

type person = {
  name : string;
  age : int;
  address : address;
}

let _address =
  mk_lens
    ( module struct
      type a = person

      type b = address

      module Mk (F : FUNCTOR) = struct
        let run f x = F.map (fun address -> { x with address }) @@ f x.address
      end
    end
    )

let _age =
  mk_lens
    ( module struct
      type a = person

      type b = int

      module Mk (F : FUNCTOR) = struct
        let run f x = F.map (fun age -> { x with age }) @@ f x.age
      end
    end
    )

let _name =
  mk_lens
    ( module struct
      type a = person

      type b = string

      module Mk (F : FUNCTOR) = struct
        let run f x = F.map (fun name -> { x with name }) @@ f x.name
      end
    end
    )

let _postcode =
  mk_lens
    ( module struct
      type a = address

      type b = string

      module Mk (F : FUNCTOR) = struct
        let run f x =
          F.map (fun postcode -> { x with postcode }) @@ f x.postcode
      end
    end
    )

let _street =
  mk_lens
    ( module struct
      type a = address

      type b = string

      module Mk (F : FUNCTOR) = struct
        let run f x = F.map (fun street -> { x with street }) @@ f x.street
      end
    end
    )

let _number =
  mk_lens
    ( module struct
      type a = address

      type b = int

      module Mk (F : FUNCTOR) = struct
        let run f x = F.map (fun number -> { x with number }) @@ f x.number
      end
    end
    )

let test_lens () =
  let address = { street = "Highstreet"; number = 42; postcode = "e1w" } in
  let person = { name = "Joe"; age = 23; address } in
  let _addPostCode = _address // _postcode in
  let _addStreet = _address // _street in
  let _addNumber = _address // _number in
  Alcotest.(check string)
    "" "HIGHSTREET"
    ((_addStreet @% String.uppercase_ascii) person).address.street;
  Alcotest.(check string)
    "" "E1W" ((_addPostCode @% String.uppercase_ascii) person).address.postcode;
  Alcotest.(check int)
    "" 43 ((_addNumber @% fun x -> x + 1) person).address.number;
  Alcotest.(check int) "" 1 PF.aVal

let () =
  let open Alcotest in
  run "ibad" [ "basic lens", [ test_case "get/modify/set" `Quick test_lens ] ]