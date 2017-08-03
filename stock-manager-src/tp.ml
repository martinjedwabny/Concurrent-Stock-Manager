module Session = Session.Bare

(* Types *)

type item = {code: string; quantity: int; price: float};;
type cart = CartItem of item * cart | EmptyCart;;
type catalog = CatalogItem of item * catalog | EmptyCatalog;;
type client_data = {name:string; card_number:int};;

(* Folds *)

let rec foldCatalog f catalog base_case = match catalog with
    | EmptyCatalog -> base_case
    | CatalogItem(item, tail) -> f item (foldCatalog f tail base_case);;

let rec foldCart f cart base_case = match cart with
    | EmptyCart -> base_case
    | CartItem(item, tail) -> f item (foldCart f tail base_case);;

(* Catalog functions *)

let stock code quantity catalog =
    foldCatalog (fun item (hay_stock, c2, price) -> if item.code = code then (item.quantity>=quantity, item.quantity, item.price) else (hay_stock, c2, price) ) catalog (false, 0, 0.0);;

let reserve code quantity catalog =
    foldCatalog (fun item res -> CatalogItem((if item.code = code then {code=code; quantity=item.quantity - quantity; price=item.price} else item), res) ) catalog EmptyCatalog;;

let cancel_reserve_n code quantity catalog =
    foldCatalog (fun item res -> CatalogItem((if item.code = code then {code=code; quantity=item.quantity + quantity; price=item.price} else item), res) ) catalog EmptyCatalog;;

let return cart catalog = foldCart (fun item res -> cancel_reserve_n item.code item.quantity res) cart catalog;;

let list_to_catalog lista =
    List.fold_right (fun x res -> CatalogItem(x, res)) lista EmptyCatalog;;

(* Funciones cart *)

let string_cart = function
        | EmptyCart -> "Empty Cart"
        | CartItem(item,c) -> let rec aux = function
            | EmptyCart -> ""
            | CartItem(item, c) -> item.code ^ " - " ^ (string_of_int item.quantity) ^ " - " ^ (string_of_float item.price) ^ ";" ^ aux c
            in aux (CartItem(item,c));;

let print_cart c = print_string (string_cart c);;

let total_price cart = foldCart (fun item res -> (float_of_int item.quantity *. item.price) +. res) cart 0.0;;

let quantity_in_cart cart code = foldCart (fun item res -> if (item.code = code) then item.quantity + res else res) cart 0;;

let is_in_cart cart code = quantity_in_cart cart code > 0;;

let add_cart_n cart code quantity =
    foldCart (fun item res -> CartItem((if item.code = code then {code=code; quantity=item.quantity + quantity; price=item.price} else item), res) ) cart EmptyCart;;

let add_cart cart code quantity price hasStock =
    if hasStock then
        if (is_in_cart cart code) then
            add_cart_n cart code quantity
        else
            CartItem({code=code; quantity=quantity; price=price},cart)
    else
        cart;;

let remove_cart cart code =
    foldCart (fun item res ->
        if item.code = code then
            res else
            CartItem(item, res)) cart EmptyCart;;

let remove_cart_n cart code quantity =
    foldCart (fun item res ->
        if item.code = code && item.quantity > quantity then
            CartItem({code=code; quantity=item.quantity - quantity; price=item.price}, res) else
            CartItem(item, res)) cart EmptyCart;;

(* Server *)

let server_cart_add sc catalog =
    let code, sc = Session.receive sc in
    let quantity, sc = Session.receive sc in
    let (hasStock,quantity_catalog, price) = stock code quantity catalog in
    let _ = if hasStock then (reserve code quantity catalog) else catalog in
    let quantity_remaining = if hasStock then quantity_catalog - quantity else quantity_catalog in
    let sc = Session.send ((hasStock,quantity_remaining, price)) sc in
    sc;;


let server_cart_remove sc catalog =
    let code, sc = Session.receive sc in
    let quantity, sc = Session.receive sc in
    let _ = cancel_reserve_n code quantity catalog in
    sc;;

let server_cart_abandon sc catalog=
    let cart, sc = Session.receive sc in
    let catalog = return cart catalog in
    (sc, catalog);;

let validate_client _ _ = true;;

let rec server_cart_purchase sc catalog =
    let info_client, sc = Session.receive sc in
    let cart, sc = Session.receive sc in
    let res = validate_client info_client cart in
    if res then
        let sc = Session.select (fun x -> `PurchaseOk x) sc in Session.close sc else
        let sc = Session.select (fun x -> `PurchaseError x) sc in
        match Session.branch sc with
            | `Purchase sc -> server_cart_purchase sc catalog
            | `Quit sc -> let sc, _ = server_cart_abandon sc catalog in
                Session.close sc;;

(* Server Ops: Add, Remove, Purchase, Quit *)

let rec server_cart sc catalog =
match Session.branch sc with
        | `Add sc -> let sc = server_cart_add sc catalog in
            server_cart sc catalog
        | `Remove sc -> let sc = server_cart_remove sc catalog in
            server_cart sc catalog
        | `Purchase sc -> server_cart_purchase sc catalog
        | `Quit sc -> let sc, _ = server_cart_abandon sc catalog in
            Session.close sc;;

(* Server v2 *)

let rec server_cart_purchase_v2 sc catalog n =
    let info_client, sc = Session.receive sc in
    let cart, sc = Session.receive sc in
    if n = 0 then
        let sc = Session.select (fun x -> `PurchaseFail x) sc in Session.close sc
    else
        let res = validate_client info_client cart in
        if res then
            let sc = Session.select (fun x -> `PurchaseOk x) sc in
                Session.close sc
        else
            let sc = Session.select (fun x -> `PurchaseError x) sc in
                match Session.branch sc with
                    | `PurchaseRetry sc -> server_cart_purchase_v2 sc catalog (n-1)
                    | `PurchaseDesist sc -> let sc, _ = server_cart_abandon sc catalog in
                        Session.close sc;;

let rec server_cart_v2 sc catalog n =
match Session.branch sc with
        | `Add sc -> let sc = server_cart_add sc catalog in
            server_cart_v2 sc catalog n
        | `Remove sc -> let sc = server_cart_remove sc catalog in
            server_cart_v2 sc catalog n
        | `Purchase sc -> server_cart_purchase_v2 sc catalog n
        | `Quit sc -> let sc, _ = server_cart_abandon sc catalog in
            Session.close sc;;

(* Server v3 *)

let service_payments spag =
    let _, spag = Session.receive spag in
    let _, spag = Session.receive spag in
    let spag = Session.send true spag in
    Session.close spag;;

let server_cart_purchase_v3 sc catalog =
    let cart, sc = Session.receive sc in
    let csc, cpag = Session.create () in
    let _ = Thread.create service_payments cpag in
    let total_price_to_pay = total_price cart in
    let csc = Session.send total_price_to_pay csc in
    let sc = Session.send csc sc in
    let csc, sc = Session.receive sc in
    let validation, csc = Session.receive csc in
    Session.close csc;
    if validation then
        let sc = Session.select (fun x -> `PurchaseOk x) sc in Session.close sc else
        let sc = Session.select (fun x -> `PurchaseError x) sc in
        match Session.branch sc with
            | `Purchase sc -> server_cart_purchase sc catalog
            | `Quit sc -> let sc, _ = server_cart_abandon sc catalog in
                Session.close sc;;

let rec server_cart_v3 sc catalog =
match Session.branch sc with
        | `Add sc -> let sc = server_cart_add sc catalog in
            server_cart_v3 sc catalog
        | `Remove sc -> let sc = server_cart_remove sc catalog in
            server_cart_v3 sc catalog
        | `Purchase sc -> server_cart_purchase_v3 sc catalog
        | `Quit sc -> let sc, _ = server_cart_abandon sc catalog in
            Session.close sc;;

(* Client *)

let client_add cc code quantity cart =
let cc = Session.select (fun x -> `Add x) cc in
let cc = Session.send code cc in
let cc = Session.send quantity cc in
let (hasStock,_, price), cc = Session.receive cc in
let res = add_cart cart code quantity price hasStock in
(cc, res);;

let client_remove cc code cart =
let cc = Session.select (fun x -> `Remove x) cc in
let cc = Session.send code cc in
let quantity = quantity_in_cart cart code in
let cc = Session.send quantity cc in
let res = remove_cart cart code in
(cc, res);;

let client_remove_n cc code quantity cart =
let cc = Session.select (fun x -> `Remove x) cc in
let cc = Session.send code cc in
let cc = Session.send quantity cc in
let res = remove_cart_n cart code quantity in
(cc, res);;

let client_abandon cc cart =
let cc = Session.select (fun x -> `Quit x) cc in
let cc = Session.send cart cc in
Session.close cc;
EmptyCatalog;;

let client_purchase cc info_client cart =
let cc = Session.select (fun x -> `Purchase x) cc in
let cc = Session.send info_client cc in
let cc = Session.send cart cc in
match Session.branch cc with
    | `PurchaseOk cc -> Session.close cc; EmptyCart
    | `PurchaseError cc -> let _ = client_abandon cc cart in EmptyCart;;

(* Client v2 *)

let client_purchase_v2 cc info_client cart =
let cc = Session.select (fun x -> `Purchase x) cc in
let rec aux cc =
    let cc = Session.send info_client cc in
    let cc = Session.send cart cc in
    match Session.branch cc with
        | `PurchaseOk cc -> Session.close cc; EmptyCart
        | `PurchaseError cc -> let cc = Session.select (fun x -> `PurchaseRetry x) cc in aux cc
        | `PurchaseFail cc -> Session.close cc; EmptyCart
    in aux cc;;

let client_purchase_v2_alt cc info_client cart =
let cc = Session.select (fun x -> `Purchase x) cc in
let aux cc =
    let cc = Session.send info_client cc in
    let cc = Session.send cart cc in
    match Session.branch cc with
        | `PurchaseOk cc -> Session.close cc; EmptyCart
        | `PurchaseError cc ->
            let cc = Session.select (fun x -> `PurchaseDesist x) cc in
            let cc = Session.send cart cc in
            Session.close cc; EmptyCart
        | `PurchaseFail cc -> Session.close cc; EmptyCart
    in aux cc;;

(* Client v3 *)

let client_purchase_v3 cc info_client cart =
let cc = Session.select (fun x -> `Purchase x) cc in
let cc = Session.send cart cc in
let cpag, cc = Session.receive cc in
let cpag = Session.send info_client cpag in
let cc = Session.send cpag cc in
match Session.branch cc with
    | `PurchaseOk cc -> Session.close cc; EmptyCart
    | `PurchaseError cc -> let _ = client_abandon cc cart in EmptyCart;;


(* Main *)

let _ =
let catalog = list_to_catalog [{code="A1";quantity=6; price = 10.0}; {code="A2";quantity=0; price = 20.0};{code="A3";quantity=1; price = 30.0};{code="A4";quantity=5; price = 40.0}] in
let a, b = Session.create () in
let _ = Thread.create (server_cart a) catalog in
let (b, cart) = client_add b "A1" 3 EmptyCart in
let (b, cart) = client_add b "A1" 3 cart in
let (b, cart) = client_remove_n b "A1" 2 cart in
let cart = client_purchase b {name="Juan Perez"; card_number=1} cart in
print_cart cart; print_string "\n";
print_float (total_price cart); print_string "\n";;

(* Main v2 *)

let _ =
let catalog = list_to_catalog [{code="A1";quantity=6; price = 10.0}; {code="A2";quantity=0; price = 20.0};{code="A3";quantity=1; price = 30.0};{code="A4";quantity=5; price = 40.0}] in
let a, b = Session.create () in
let _ = Thread.create (server_cart_v2 a catalog) 3 in
let (b, cart) = client_add b "A1" 3 EmptyCart in
let (b, cart) = client_add b "A1" 3 cart in
let (b, cart) = client_remove_n b "A1" 2 cart in
let cart = client_purchase_v2_alt b {name="Juan Perez"; card_number=1} cart in
print_cart cart; print_string "\n";
print_float (total_price cart); print_string "\n";;


(* Main v3 *)

let _ =
let catalog = list_to_catalog [{code="A1";quantity=6; price = 10.0}; {code="A2";quantity=0; price = 20.0};{code="A3";quantity=1; price = 30.0};{code="A4";quantity=5; price = 40.0}] in
let a, b = Session.create () in
let _ = Thread.create (server_cart_v3 a) catalog in
let (b, cart) = client_add b "A1" 3 EmptyCart in
let (b, cart) = client_add b "A1" 3 cart in
let (b, cart) = client_remove_n b "A1" 2 cart in
let cart = client_purchase_v3 b {name="Juan Perez"; card_number=1} cart in
print_cart cart; print_string "\n";
print_float (total_price cart); print_string "\n";;
