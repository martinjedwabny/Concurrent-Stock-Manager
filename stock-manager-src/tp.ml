module Session = Session.Bare

(* Types *)

type item = {code: string; quantity: int; price: float};;
type cart = CartItem of item * cart | EmptyCart;;
type catalogo = Item of item * catalogo | Vacio;;
type datos_cliente = {nombre:string; numero_tarjeta:int};;

let rec foldCatalogo f catalogo base_case = match catalogo with
    | Vacio -> base_case
    | Item(item, tail) -> f item (foldCatalogo f tail base_case);;

let rec foldCart f cart base_case = match cart with
    | EmptyCart -> base_case
    | CartItem(item, tail) -> f item (foldCart f tail base_case);;

let lista_to_catalogo lista =
    List.fold_right (fun x res -> Item(x, res)) lista Vacio;;

let string_cart = function
        | EmptyCart -> "Carrito Vacio"
        | CartItem(item,c) -> let rec aux = function
            | EmptyCart -> ""
            | CartItem(item, c) -> item.code ^ " - " ^ (string_of_int item.quantity) ^ " - " ^ (string_of_float item.price) ^ ";" ^ aux c
            in aux (CartItem(item,c));;

let print_cart c = print_string (string_cart c);;

(* Funciones catalogo *)

let stock code quantity catalogo =
    foldCatalogo (fun item (hay_stock, c2, price) -> if item.code = code then (item.quantity>=quantity, item.quantity, item.price) else (hay_stock, c2, price) ) catalogo (false, 0, 0.0);;

let reservar code quantity catalogo =
    foldCatalogo (fun item res -> Item((if item.code = code then {code=code; quantity=item.quantity - quantity; price=item.price} else item), res) ) catalogo Vacio;;

let cancelar_reservar_n code quantity catalogo =
    foldCatalogo (fun item res -> Item((if item.code = code then {code=code; quantity=item.quantity + quantity; price=item.price} else item), res) ) catalogo Vacio;;

let devolver cart catalogo = foldCart (fun item res -> cancelar_reservar_n item.code item.quantity res) cart catalogo;;

(* Funciones cart *)

let monto cart = foldCart (fun item res -> (float_of_int item.quantity *. item.price) +. res) cart 0.0;;

let quantity_en_cart cart code = foldCart (fun item res -> if (item.code = code) then item.quantity + res else res) cart 0;;

let esta_en_cart cart code = quantity_en_cart cart code > 0;;

let agregar_cart_n cart code quantity price =
    foldCart (fun item res -> CartItem((if item.code = code then {code=code; quantity=item.quantity + quantity; price=item.price} else item), res) ) cart EmptyCart;;

let agregar_cart cart code quantity price hayStock =
    if hayStock then
        if (esta_en_cart cart code) then
            agregar_cart_n cart code quantity price
        else
            CartItem({code=code; quantity=quantity; price=price},cart)
    else
        cart;;

let quitar_cart cart code =
    foldCart (fun item res ->
        if item.code = code then
            res else
            CartItem(item, res)) cart EmptyCart;;

let quitar_cart_n cart code quantity =
    foldCart (fun item res ->
        if item.code = code && item.quantity > quantity then
            CartItem({code=code; quantity=item.quantity - quantity; price=item.price}, res) else
            CartItem(item, res)) cart EmptyCart;;

(* Server *)

let server_cart_agregar sc catalogo =
    let code, sc = Session.receive sc in
    let quantity, sc = Session.receive sc in
    let (hayStock,quantity_catalogo, price) = stock code quantity catalogo in
    let catalogo = if hayStock then (reservar code quantity catalogo) else catalogo in
    let quantity_restante = if hayStock then quantity_catalogo - quantity else quantity_catalogo in
    let sc = Session.send ((hayStock,quantity_restante, price)) sc in
    sc;;


let server_cart_quitar sc catalogo =
    let code, sc = Session.receive sc in
    let quantity, sc = Session.receive sc in
    let catalogo = cancelar_reservar_n code quantity catalogo in
    sc;;

let server_cart_abandonar sc catalogo=
    let cart, sc = Session.receive sc in
    let catalogo = devolver cart catalogo in
    (sc, catalogo);;

let validar_cliente info_cliente cart = true;;

let rec server_cart_comprar sc catalogo =
    let info_cliente, sc = Session.receive sc in
    let cart, sc = Session.receive sc in
    let res = validar_cliente info_cliente cart in
    if res then
        let sc = Session.select (fun x -> `CompraOk x) sc in Session.close sc else
        let sc = Session.select (fun x -> `CompraError x) sc in
        match Session.branch sc with
            | `Comprar sc -> server_cart_comprar sc catalogo
            | `Abandonar sc -> let sc, catalogo = server_cart_abandonar sc catalogo in
                Session.close sc;;

(* Operaciones Server: Agregar, Quitar, Comprar, Abandonar *)

let rec server_cart sc catalogo =
match Session.branch sc with
        | `Agregar sc -> let sc = server_cart_agregar sc catalogo in
            server_cart sc catalogo
        | `Quitar sc -> let sc = server_cart_quitar sc catalogo in
            server_cart sc catalogo
        | `Comprar sc -> server_cart_comprar sc catalogo
        | `Abandonar sc -> let sc, catalogo = server_cart_abandonar sc catalogo in
            Session.close sc;;

(* Server extension ejercicio 3 *)

let rec server_cart_comprar_ext3 sc catalogo n =
    let info_cliente, sc = Session.receive sc in
    let cart, sc = Session.receive sc in
    if n = 0 then
        let sc = Session.select (fun x -> `CompraFail x) sc in Session.close sc
    else
        let res = validar_cliente info_cliente cart in
        if res then
            let sc = Session.select (fun x -> `CompraOk x) sc in
                Session.close sc
        else
            let sc = Session.select (fun x -> `CompraError x) sc in
                match Session.branch sc with
                    | `CompraReintentar sc -> server_cart_comprar_ext3 sc catalogo (n-1)
                    | `CompraDesistir sc -> let sc, catalogo = server_cart_abandonar sc catalogo in
                        Session.close sc;;

let rec server_cart_ext3 sc catalogo n =
match Session.branch sc with
        | `Agregar sc -> let sc = server_cart_agregar sc catalogo in
            server_cart_ext3 sc catalogo n
        | `Quitar sc -> let sc = server_cart_quitar sc catalogo in
            server_cart_ext3 sc catalogo n
        | `Comprar sc -> server_cart_comprar_ext3 sc catalogo n
        | `Abandonar sc -> let sc, catalogo = server_cart_abandonar sc catalogo in
            Session.close sc;;

(* Server extension ejercicio 4 *)

let servicio_pagos spag =
    let monto_a_pagar, spag = Session.receive spag in
    let info_cliente, spag = Session.receive spag in
    let spag = Session.send true spag in
    Session.close spag;;

let server_cart_comprar_ext4 sc catalogo =
    let cart, sc = Session.receive sc in
    let csc, cpag = Session.create () in
    let _ = Thread.create servicio_pagos cpag in
    let monto_a_pagar = monto cart in
    let csc = Session.send monto_a_pagar csc in
    let sc = Session.send csc sc in
    let csc, sc = Session.receive sc in
    let validacion, csc = Session.receive csc in
    Session.close csc;
    if validacion then
        let sc = Session.select (fun x -> `CompraOk x) sc in Session.close sc else
        let sc = Session.select (fun x -> `CompraError x) sc in
        match Session.branch sc with
            | `Comprar sc -> server_cart_comprar sc catalogo
            | `Abandonar sc -> let sc, catalogo = server_cart_abandonar sc catalogo in
                Session.close sc;;

let rec server_cart_ext4 sc catalogo =
match Session.branch sc with
        | `Agregar sc -> let sc = server_cart_agregar sc catalogo in
            server_cart_ext4 sc catalogo
        | `Quitar sc -> let sc = server_cart_quitar sc catalogo in
            server_cart_ext4 sc catalogo
        | `Comprar sc -> server_cart_comprar_ext4 sc catalogo
        | `Abandonar sc -> let sc, catalogo = server_cart_abandonar sc catalogo in
            Session.close sc;;

(* Cliente *)

let cliente_agregar cc code quantity cart =
let cc = Session.select (fun x -> `Agregar x) cc in
let cc = Session.send code cc in
let cc = Session.send quantity cc in
let (hayStock,quantity_restante, price), cc = Session.receive cc in
let res = agregar_cart cart code quantity price hayStock in
(cc, res);;

let cliente_quitar cc code cart =
let cc = Session.select (fun x -> `Quitar x) cc in
let cc = Session.send code cc in
let quantity = quantity_en_cart cart code in
let cc = Session.send quantity cc in
let res = quitar_cart cart code in
(cc, res);;

let cliente_quitar_n cc code quantity cart =
let cc = Session.select (fun x -> `Quitar x) cc in
let cc = Session.send code cc in
let cc = Session.send quantity cc in
let res = quitar_cart_n cart code quantity in
(cc, res);;

let cliente_abandonar cc cart =
let cc = Session.select (fun x -> `Abandonar x) cc in
let cc = Session.send cart cc in
Session.close cc;
Vacio;;

let cliente_comprar cc info_cliente cart =
let cc = Session.select (fun x -> `Comprar x) cc in
let cc = Session.send info_cliente cc in
let cc = Session.send cart cc in
match Session.branch cc with
    | `CompraOk cc -> Session.close cc; EmptyCart
    | `CompraError cc -> cliente_abandonar cc cart; EmptyCart;;

(* Cliente extension ejercicio 3 *)

(* Si la compra da ERROR, reintenta hasta que de OK o FAIL *)
let cliente_comprar_ext3 cc info_cliente cart =
let cc = Session.select (fun x -> `Comprar x) cc in
let rec aux cc =
    let cc = Session.send info_cliente cc in
    let cc = Session.send cart cc in
    match Session.branch cc with
        | `CompraOk cc -> Session.close cc; EmptyCart
        | `CompraError cc -> let cc = Session.select (fun x -> `CompraReintentar x) cc in aux cc
        | `CompraFail cc -> Session.close cc; EmptyCart
    in aux cc;;

(* Si la compra da ERROR, abandona *)
let cliente_comprar_ext3_alt cc info_cliente cart =
let cc = Session.select (fun x -> `Comprar x) cc in
let aux cc =
    let cc = Session.send info_cliente cc in
    let cc = Session.send cart cc in
    match Session.branch cc with
        | `CompraOk cc -> Session.close cc; EmptyCart
        | `CompraError cc ->
            let cc = Session.select (fun x -> `CompraDesistir x) cc in
            let cc = Session.send cart cc in
            Session.close cc; EmptyCart
        | `CompraFail cc -> Session.close cc; EmptyCart
    in aux cc;;

(* Cliente extension ejercicio 4 *)

let cliente_comprar_ext4 cc info_cliente cart =
let cc = Session.select (fun x -> `Comprar x) cc in
let cc = Session.send cart cc in
let cpag, cc = Session.receive cc in
let cpag = Session.send info_cliente cpag in
let cc = Session.send cpag cc in
match Session.branch cc with
    | `CompraOk cc -> Session.close cc; EmptyCart
    | `CompraError cc -> cliente_abandonar cc cart; EmptyCart;;


(* Main *)

let _ =
let catalogo = lista_to_catalogo [{code="A1";quantity=6; price = 10.0}; {code="A2";quantity=0; price = 20.0};{code="A3";quantity=1; price = 30.0};{code="A4";quantity=5; price = 40.0}] in
let a, b = Session.create () in
let _ = Thread.create (server_cart a) catalogo in
let (b, cart) = cliente_agregar b "A1" 3 EmptyCart in
let (b, cart) = cliente_agregar b "A1" 3 cart in
let (b, cart) = cliente_quitar_n b "A1" 2 cart in
let cart = cliente_comprar b {nombre="Juan Perez"; numero_tarjeta=1} cart in
print_cart cart; print_string "\n";
print_float (monto cart); print_string "\n";;

(* Main extension ejercicio 3 *)

let _ =
let catalogo = lista_to_catalogo [{code="A1";quantity=6; price = 10.0}; {code="A2";quantity=0; price = 20.0};{code="A3";quantity=1; price = 30.0};{code="A4";quantity=5; price = 40.0}] in
let a, b = Session.create () in
let _ = Thread.create (server_cart_ext3 a catalogo) 3 in
let (b, cart) = cliente_agregar b "A1" 3 EmptyCart in
let (b, cart) = cliente_agregar b "A1" 3 cart in
let (b, cart) = cliente_quitar_n b "A1" 2 cart in
let cart = cliente_comprar_ext3_alt b {nombre="Juan Perez"; numero_tarjeta=1} cart in
print_cart cart; print_string "\n";
print_float (monto cart); print_string "\n";;


(* Main extension ejercicio 4 *)

let _ =
let catalogo = lista_to_catalogo [{code="A1";quantity=6; price = 10.0}; {code="A2";quantity=0; price = 20.0};{code="A3";quantity=1; price = 30.0};{code="A4";quantity=5; price = 40.0}] in
let a, b = Session.create () in
let _ = Thread.create (server_cart_ext4 a) catalogo in
let (b, cart) = cliente_agregar b "A1" 3 EmptyCart in
let (b, cart) = cliente_agregar b "A1" 3 cart in
let (b, cart) = cliente_quitar_n b "A1" 2 cart in
let cart = cliente_comprar_ext4 b {nombre="Juan Perez"; numero_tarjeta=1} cart in
print_cart cart; print_string "\n";
print_float (monto cart); print_string "\n";;
