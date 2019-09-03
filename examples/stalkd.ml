open Lwt.Infix

let log s = Lwt_io.printf "[II] %s\n%!" s

let establish_server connection_handler ~port ~backlog_max_conn =
  let handle_client_connection (client_ssl_sock, client_addr) =
    let inet_addr_of_sockaddr = function
      | Unix.ADDR_INET (n, _) ->
          n
      | Unix.ADDR_UNIX _ ->
          Unix.inet_addr_any
    in
    let client_addr = inet_addr_of_sockaddr client_addr in
    let client_ip = Unix.string_of_inet_addr client_addr in
    log (Printf.sprintf "opening connection for [%s]" client_ip)
    >>= fun () ->
    connection_handler client_addr client_ssl_sock
    >|= fun () -> Lwt_ssl.shutdown client_ssl_sock Unix.SHUTDOWN_ALL
  in
  let rec accept_clients server_sock ssl_ctx =
    let try_to_accept =
      Lwt_unix.accept server_sock
      >>= fun (client_sock, client_addr) ->
      Lwt_ssl.ssl_accept client_sock ssl_ctx
      >>= fun client_ssl_sock ->
      handle_client_connection (client_ssl_sock, client_addr)
    in
    Lwt.choose [ try_to_accept ]
    >>= fun () -> accept_clients server_sock ssl_ctx
  in
  log ("establishing server on localhost port: " ^ string_of_int port)
  >>= fun () ->
  let server_sock = Lwt_unix.(socket PF_INET SOCK_STREAM 0) in
  let ssl_ctx = Ssl.create_context Ssl.SSLv23 Ssl.Server_context in
  Ssl.use_certificate ssl_ctx "./examples/server1.crt" "./examples/server1.key" ;
  Lwt_unix.(setsockopt server_sock SO_REUSEADDR true) ;
  Lwt_unix.bind server_sock (Unix.ADDR_INET (Unix.inet_addr_any, port))
  >>= fun () ->
  Lwt_unix.listen server_sock backlog_max_conn ;
  log "listening for connections"
  >>= fun () -> accept_clients server_sock ssl_ctx


let bufsize = 1024

let buf = Bytes.create bufsize

let connected_clients = ref []

let connection_handler client_addr client_lwt_ssl =
  connected_clients := (client_addr, client_lwt_ssl) :: !connected_clients ;
  let rec talk msg =
    if msg = "exit"
    then (
      log "A client has quit"
      >|= fun () ->
      connected_clients :=
        List.filter
          (fun (_, lwt_ssl') -> lwt_ssl' != client_lwt_ssl)
          !connected_clients ;
      Lwt_ssl.shutdown client_lwt_ssl Unix.SHUTDOWN_ALL )
    else
      Lwt_ssl.read client_lwt_ssl buf 0 bufsize
      >>= fun l ->
      let m = Bytes.sub buf 0 l in
      let msg = Bytes.sub m 0 (Bytes.length m - 1) in
      let msg = Bytes.to_string msg in
      log (Printf.sprintf "received '%s'" msg)
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
  establish_server connection_handler ~port ~backlog_max_conn:100
  |> Lwt_main.run
