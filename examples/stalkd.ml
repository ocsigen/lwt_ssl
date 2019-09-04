open Lwt.Infix

let log s = Lwt_io.printf "[stalkd] %s\n%!" s

let sp = Printf.sprintf

let addr_string : Unix.sockaddr -> string = function
| Unix.ADDR_INET (n, p) ->
  sp "%s: %s" (Unix.string_of_inet_addr n) (string_of_int p)
| Unix.ADDR_UNIX _ ->
  sp "%s" Unix.(string_of_inet_addr inet_addr_any)


type connection_handler = Unix.sockaddr -> Lwt_ssl.socket -> unit Lwt.t

let establish_server :
  connection_handler -> port:int -> max_backlog_conn:int -> unit Lwt.t =
 fun connection_handler ~port ~max_backlog_conn ->
 let handle_client_connection (client_ssl, client_addr) =
   Lwt.async
   @@ fun () ->
   Lwt.catch
     (fun () ->
       log (sp "opening connection for [%s]" @@ addr_string client_addr)
       >>= fun () -> connection_handler client_addr client_ssl)
     (fun exn ->
       log (sp "error while talking with client %s" (Printexc.to_string exn)))
 in
 let rec accept_clients server_sock ssl_ctx =
   Lwt.catch
     (fun () ->
       Lwt_unix.accept server_sock
       >>= fun (client_sock, client_addr) ->
       Lwt_ssl.ssl_accept client_sock ssl_ctx
       >>= fun client_ssl ->
       log "accepted a new connection"
       >>= fun () ->
       handle_client_connection (client_ssl, client_addr) ;
       accept_clients server_sock ssl_ctx)
     (fun exn ->
       log (sp "error while accepting clients, %s" @@ Printexc.to_string exn))
 in
 log ("establishing server on localhost port: " ^ string_of_int port)
 >>= fun () ->
 let ssl_ctx = Ssl.create_context Ssl.SSLv23 Ssl.Server_context in
 Ssl.use_certificate ssl_ctx "./examples/server1.crt" "./examples/server1.key" ;
 Lwt_unix.(
   let server_sock = socket PF_INET SOCK_STREAM 0 in
   setsockopt server_sock SO_REUSEADDR true ;
   bind server_sock (Unix.ADDR_INET (Unix.inet_addr_any, port))
   >>= fun () ->
   listen server_sock max_backlog_conn ;
   log "listening for connections"
   >>= fun () -> accept_clients server_sock ssl_ctx)


let connected_clients = ref []

let connection_handler : connection_handler =
 fun client_addr client_ssl ->
 let rec talk = function
 | "exit" ->
   log (sp "client [%s] has quit" @@ addr_string client_addr)
   <&>
   ( connected_clients :=
     List.filter
       (fun (_, client_ssl') -> client_ssl' != client_ssl)
       !connected_clients ;
     Lwt_ssl.ssl_shutdown client_ssl )
 | _ ->
   let ic = Lwt_ssl.in_channel_of_descr client_ssl in
   Lwt_io.read_line_opt ic
   >>= (function
   | None ->
     Lwt.return_unit
   | Some line ->
     log (sp "received '%s'" line)
     <&> Lwt_list.iter_p
       (fun (_, client_ssl') ->
         let oc = Lwt_ssl.out_channel_of_descr client_ssl' in
         Lwt_io.write_line oc line)
           !connected_clients
     >>= fun () -> talk line)
 in
 connected_clients := (client_addr, client_ssl) :: !connected_clients ;
 talk ""


let () =
  let port = try int_of_string Sys.argv.(1) with _ -> 9876 in
  Ssl.init () ;
  establish_server connection_handler ~port ~max_backlog_conn:100
  |> Lwt_main.run
