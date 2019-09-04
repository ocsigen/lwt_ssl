open Lwt.Infix

let log s = Lwt_io.printf "[stalkd] %s\n%!" s

let sp = Printf.sprintf

let addr_string : Unix.sockaddr -> string = function
| Unix.ADDR_INET (n, p) ->
  sp "%s: %s" (Unix.string_of_inet_addr n) (string_of_int p)
| Unix.ADDR_UNIX _ ->
  sp "%s" Unix.(string_of_inet_addr inet_addr_any)


let establish_server connection_handler :
  port:int -> max_backlog_conn:int -> unit Lwt.t =
 fun ~port ~max_backlog_conn ->
 let handle_client_connection (client_ssl, client_addr) =
   Lwt.async
   @@ fun () ->
   Lwt.catch
     (fun () ->
       log (sp "opening connection for [%s]" @@ addr_string client_addr)
       <&> connection_handler client_addr client_ssl)
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
       handle_client_connection (client_ssl, client_addr) ;
       accept_clients server_sock ssl_ctx)
     (fun exn ->
       log (sp "error while accepting clients, %s" @@ Printexc.to_string exn))
 in
 log ("establishing server on localhost port: " ^ string_of_int port)
 <&>
 let ssl_ctx = Ssl.create_context Ssl.SSLv23 Ssl.Server_context in
 Ssl.use_certificate ssl_ctx "./examples/server1.crt" "./examples/server1.key" ;
 Lwt_unix.(
   let server_sock = socket PF_INET SOCK_STREAM 0 in
   setsockopt server_sock SO_REUSEADDR true ;
   bind server_sock (Unix.ADDR_INET (Unix.inet_addr_any, port))
   >>= fun () ->
   listen server_sock max_backlog_conn ;
   log "listening for connections" <&> accept_clients server_sock ssl_ctx)


let bufsize = 1024

let buf = Bytes.create bufsize

let connected_clients = ref []

let connection_handler : Unix.sockaddr -> Lwt_ssl.socket -> unit Lwt.t =
 fun client_addr client_ssl ->
 connected_clients := (client_addr, client_ssl) :: !connected_clients ;
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
   Lwt_ssl.read client_ssl buf 0 bufsize
   >>= fun l ->
   let m = Bytes.sub buf 0 l in
   let msg = Bytes.sub m 0 (Bytes.length m - 1) in
   let msg = Bytes.to_string msg in
   log (sp "received '%s'" msg)
   >>= fun () ->
   List.iter
     (fun (_, lwt_ssl') ->
       match Lwt_ssl.ssl_socket lwt_ssl' with
       | None ->
         ()
       | Some s ->
         Ssl.output_string s (Bytes.to_string m))
     !connected_clients ;
   talk msg
 in
 log "accepted a new connection" >>= fun () -> talk ""


let () =
  let port = try int_of_string Sys.argv.(1) with _ -> 9876 in
  Ssl.init () ;
  establish_server connection_handler ~port ~max_backlog_conn:100
  |> Lwt_main.run
