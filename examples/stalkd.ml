open Lwt.Infix

let certfile = "./examples/server1.crt"

let privkey = "./examples/server.key"

let port = 9876

let log s = Lwt_io.printf "[II] %s\n%!" s

let establish_server server_handler sockaddr nbconn =
  log "establishing server"
  >>= fun () ->
  let domain =
    match sockaddr with
    | Unix.ADDR_UNIX _ ->
        Unix.PF_UNIX
    | Unix.ADDR_INET (_, _) ->
        Unix.PF_INET
  in
  let sock = Lwt_unix.(socket domain SOCK_STREAM 0) in
  let handle_connection (s, caller) =
    let inet_addr_of_sockaddr = function
      | Unix.ADDR_INET (n, _) ->
          n
      | Unix.ADDR_UNIX _ ->
          Unix.inet_addr_any
    in
    let inet_addr = inet_addr_of_sockaddr caller in
    let ip = Unix.string_of_inet_addr inet_addr in
    log (Printf.sprintf "opening connection for [%s]" ip)
    >>= fun () ->
    server_handler inet_addr s
    >|= fun () -> Lwt_ssl.shutdown s Unix.SHUTDOWN_ALL
  in
  let accept sock ssl_ctx =
    let rec loop () =
      let try_to_accept =
        Lwt_unix.accept sock
        >>= fun (client_sock, client_addr) ->
        Lwt_ssl.ssl_accept client_sock ssl_ctx
        >>= fun socket -> handle_connection (socket, client_addr)
      in
      Lwt.choose [ try_to_accept ] >>= fun () -> loop ()
    in
    loop ()
  in
  let ssl_ctx = Ssl.create_context Ssl.SSLv23 Ssl.Server_context in
  Ssl.use_certificate ssl_ctx certfile privkey ;
  Lwt_unix.(setsockopt sock SO_REUSEADDR true) ;
  Lwt_unix.bind sock sockaddr
  >>= fun () ->
  Lwt_unix.listen sock nbconn ;
  accept sock ssl_ctx


let () =
  let bufsize = 1024 in
  let buf = Bytes.create bufsize in
  let connected_clients = ref [] in
  Ssl.init () ;
  establish_server
    (fun addr lwt_ssl ->
      connected_clients := (addr, lwt_ssl) :: !connected_clients ;
      let rec loop msg =
        if msg = "exit"
        then (
          log "A client has quit"
          >|= fun () ->
          connected_clients :=
            List.filter
              (fun (_, lwt_ssl') -> lwt_ssl' != lwt_ssl)
              !connected_clients ;
          Lwt_ssl.shutdown lwt_ssl Unix.SHUTDOWN_ALL )
        else
          Lwt_ssl.read lwt_ssl buf 0 bufsize
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
          loop msg
      in
      log "accepted a new connection" >>= fun () -> loop "")
    (Unix.ADDR_INET (Unix.inet_addr_any, port))
    100
  |> Lwt_main.run
