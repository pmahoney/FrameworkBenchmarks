(**

A pre-forking http server using Async. Note that rather than simply
   forking child processes, the supervisor process forks then execs
   itself, with args instructing the worker process which inherited
   file descriptor to use as the listening socket.

The reason for fork+exec instead of just fork is that once Async is
   started, it's difficult (or impossible) for a forked child process
   to function correctly.

The supervisor finds its executable via Sys.argv.(0), which thus must
   be the path to the executable.

*)

open Core
open Async
open Log.Global
open Httpaf
open Httpaf_async

(* Dates *)

let http_date now =
  let (date, ofday) = Time_ns.to_date_ofday ~zone:Time.Zone.utc now in
  let parts = Time_ns.Ofday.to_parts ofday in
  (* existing Day_of_week.to_string returns in all caps *)
  let day_name = match Date.day_of_week date with
    | Sun -> "Sun"
    | Mon -> "Mon"
    | Tue -> "Tue"
    | Wed -> "Wed"
    | Thu -> "Thu"
    | Fri -> "Fri"
    | Sat -> "Sat"
  in
  let open Date in
  (* Wed, 17 Apr 2013 12:00:00 GMT *)
  Format.sprintf "%s, %02d %s %4d %02d:%02d:%02d GMT"
    day_name
    (day date)
    (month date |> Month.to_string)
    (year date)
    parts.hr
    parts.min
    parts.sec

let memo_date = ref @@ http_date (Time_ns.now ())

(* HTTP *)

let request_handler (_ : Socket.Address.Inet.t) reqd =
  let req = Reqd.request reqd in
  match req.target with
  | "/json" ->
      let obj = `Assoc [ ("message", `String "Hello, World!") ] in
      let payload = Yojson.to_string obj in
      let headers =
        Headers.of_rev_list
          [
            ("content-length", string_of_int @@ String.length payload);
            ("content-type", "application/json");
            ("server", "httpaf");
            ("date", !memo_date);
          ]
      in
      let rsp = Response.create ~headers `OK in
      Reqd.respond_with_string reqd rsp payload
  | "/plaintext" ->
      let payload = "Hello, World!" in
      let headers =
        Headers.of_rev_list
          [
            ("content-length", string_of_int @@ String.length payload);
            ("content-type", "text/plain");
            ("server", "httpaf");
            ("date", !memo_date);
          ]
      in
      let rsp = Response.create ~headers `OK in
      Reqd.respond_with_string reqd rsp payload
  | _ ->
      let moo = "m00." in
      let headers =
        Headers.of_list
          [ ("content-length", string_of_int @@ String.length moo) ]
      in
      let rsp = Response.create ~headers `OK in
      Reqd.respond_with_string reqd rsp moo

let error_handler (_ : Socket.Address.Inet.t) ?request:_ error start_response =
  let response_body = start_response Headers.empty in
  begin match error with
  | `Exn exn ->
    Body.write_string response_body (Exn.to_string exn);
    Body.write_string response_body "\n";
  | #Status.standard as error ->
    Body.write_string response_body (Status.default_reason_phrase error)
  end;
  Body.close_writer response_body

let work ?tags:(tags=[]) listener =
  let handler =
    Server.create_connection_handler ~request_handler ~error_handler
  in

  every Time.Span.second
    (fun () -> memo_date := http_date (Scheduler.cycle_start_ns ()));

  info ~tags "listening %s" "TODO:someaddr";

  Deferred.repeat_until_finished () (fun () ->
      Unix.Socket.accept_at_most listener ~limit:10
      >>= function
      | `Socket_closed ->
         return (`Finished ())
      | `Ok conns ->
         List.iter conns ~f:(fun conn ->
             let (socket, client_address) = conn in
             handler client_address socket |> don't_wait_for);
         return (`Repeat ()))

let child id fd =
  let tags =
    [
      ("id", id);
      ("pid", Core.Unix.getpid() |> Pid.to_string);
    ]
  in

  (* import the (listening) socket inherited from parent *)
  let listener =
    Unix.Socket.of_fd
      Fd.(create (Kind.Socket `Passive)
            (Core.Unix.File_descr.of_int fd)
            (Core.Info.of_string ""))
      Socket.Type.tcp
  in

  Core.Unix.set_close_on_exec (Socket.fd listener |> Fd.file_descr_exn);

  work ~tags listener

let waitpid pid name =
  Unix.waitpid pid
  >>| fun status ->
  info "worker exited: %s: %s[%s]"
    (Core.Unix.Exit_or_signal.to_string_hum status)
    name
    (Pid.to_string pid)

let main ~port ~nworkers =
  let listen_address = Unix.Socket.Address.Inet.create_bind_any port in
  let socket = Unix.Socket.(create Type.tcp) in

  Unix.Socket.(bind socket listen_address)
  >>= fun bound_socket ->

  let listen_socket = Unix.Socket.listen bound_socket in

  begin match nworkers with
  | Some n -> return n
  | None ->
     begin match Unix.cores with
     | Ok cores ->
        cores ()
        >>= fun cores ->
        info "detected %d cores" cores;
        return cores
     | Error _ ->
        info "unknown number of cores";
        return 2
     end
  end

  >>= function
  | n when n = 1 ->
     info "single-process mode";
     work listen_socket
  | nworkers ->
     info "pre-forking %d workers" nworkers;

     let fd = Socket.fd listen_socket in

     (* Unix.Socket.bind sets close_on_exec; clear to allow worker to inherit *)
     Core.Unix.clear_close_on_exec (Fd.file_descr_exn fd);
     
     let prog = (Sys.get_argv ()).(0) in
     let fd = (Fd.to_int_exn fd) |> Int.to_string in
     let children =
       Sequence.range 1 nworkers ~start:`inclusive ~stop:`inclusive
       |> Sequence.map ~f:(fun i ->
              let argv = [
                  prog;
                  "worker";
                  "-id"; (Int.to_string i);
                  "-fd"; fd;
                ]
              in
              Core.Unix.fork_exec ~prog ~argv ())
       |> Sequence.to_list
     in
     Deferred.List.iteri children
       ~f:(fun i pid -> waitpid pid (Format.sprintf "child-%d" (i+1)))

let () =
  Command.group ~summary:"pre-forking http server"
    [
      ("supervisor",
       Command.async ~summary:"supervisor process"
         Command.Let_syntax.(
         let %map_open nworkers =
           flag "-n" (optional int)
             ~doc:"number of worker processes"
         and port =
           flag "-port" (optional_with_default 8080 int)
             ~doc:"the port on which to listen"
         in
         (fun () -> main ~port ~nworkers)));

      ("worker",
       Command.async ~summary:"worker process; only invoked by supervisor"
         Command.Let_syntax.(
         let%map_open id =
           flag "-id" (required string)
             ~doc:"child identifier"
         and fd =
           flag "-fd" (required int)
             ~doc:"listening socket"
         in
         (fun () -> child id fd)));
    ]
  |> Command.run
