(* This file is part of Learn-OCaml.
 *
 * Copyright (C) 2019-2023 OCaml Software Foundation.
 * Copyright (C) 2015-2018 OCamlPro.
 *
 * Learn-OCaml is distributed under the terms of the MIT license. See the
 * included LICENSE file for details. *)

open Learnocaml_data
open Lwt.Infix
module Api = Learnocaml_api

open Cmdliner
open Arg

let version = Api.version

let url_conv =
  conv ~docv:"URL" (
      (fun s ->
        try Ok (Uri.of_string s)
        with e -> Error (`Msg (Printexc.to_string e))),
      Uri.pp_hum
    )

let token_conv =
  conv ~docv:"TOKEN" (
      (fun s ->
        try Ok (Token.parse s)
        with Failure msg ->
          Error (`Msg (Printf.sprintf "Invalid token %s: %s" s msg))),
      (fun fmt t -> Format.pp_print_string fmt (Token.to_string t))
    )

module Args_server = struct
  (* Subset of Args_global, to be used if "--token" is irrelevant *)
  type t = {
      server_url: Uri.t option;
      local: bool;
    }

  let server_url =
    value & opt (some url_conv) None &
    info ["s";"server"] ~docv:"URL" ~doc:
      "The URL of the learn-ocaml server."
      ~env:(Cmd.Env.info "LEARNOCAML_SERVER" ~doc:
              "Sets the learn-ocaml server URL. Overridden by $(b,--server).")
  let local =
    value & flag & info ["local"] ~doc:
      "Use a configuration file local to the current directory, rather \
       than user-wide."

  let apply server_url local =
    {server_url; local}

  let term =
    Term.(const apply $server_url $local)

  let term_server =
    Term.(const (fun x -> x) $ server_url)
end

module Args_global = struct
  type t = {
      server_url: Uri.t option;
      token: Learnocaml_data.student Learnocaml_data.token option;
      local: bool;
    }

  let server_url =
    value & opt (some url_conv) None &
    info ["s";"server"] ~docv:"URL" ~doc:
      "The URL of the learn-ocaml server."
      ~env:(Cmd.Env.info "LEARNOCAML_SERVER" ~doc:
              "Sets the learn-ocaml server URL. Overridden by $(b,--server).")

  let token =
    value & opt (some token_conv) None & info ["token";"t"] ~docv:"TOKEN" ~doc:
      "Your token on the learn-ocaml server. This is required when submitting \
       solutions or interacting with the server."
      ~env:(Cmd.Env.info "LEARNOCAML_TOKEN" ~doc:
              "Sets the learn-ocaml user token on the sever. Overridden by \
               $(b,--token).")

  let local =
    value & flag & info ["local"] ~doc:
      "Use a configuration file local to the current directory, rather \
       than user-wide."

  let apply server_url token local =
    {server_url; token; local}

  let term =
    Term.(const apply $server_url $token $local)

  let term_server =
    Term.(const (fun x -> x) $ server_url)
end

module Args_create_token = struct
  type t = {
      nickname : string option;
      secret : string;
    }

  let nickname =
    value & pos 0 (some string) None & info [] ~docv:"NICKNAME" ~doc:
      "The desired nickname."

  let secret =
    value & pos 1 string "" & info [] ~docv:"SECRET" ~doc:
      "The secret. If not provided, use \"\" as a secret."

  let apply nickname secret = {nickname; secret}

  let term = Term.(const apply $ nickname $ secret)
end

module Args_exercise_id = struct
  let id =
    value & pos 0 (some string) None & info [] ~docv:"ID" ~doc:
      "The exercise identifier."

  let term = Term.(const (fun x -> x) $ id)
end

module Args_exercises = struct
  type t = {
    solution_file: string option;
    exercise_id: string option;
    output_format: [`Console|`Json|`Html|`Raw];
    submit: bool;
    color: bool;
    verbosity: int;
    }

  let solution_file =
    value & pos 0 (some file) None & info [] ~docv:"FILE" ~doc:
      "The file containing the user's solution to the exercise."

  let exercise_id =
    value & opt (some string) None & info ["id"] ~docv:"ID" ~doc:
      "The exercise identifier. If unspecified, this is inferred from the file \
       name."

  let output_format =
    value & vflag `Console & [
      `Console, info ["console"] ~doc:
        "output the results to the console. This is the default.";
      `Json, info ["json"] ~doc:
        "output the results as JSON, to stdout.";
      `Html, info ["html"] ~doc:
        "output the results as HTML, to the console.";
      `Raw, info ["raw"] ~doc:
        "output the results as raw text.";
    ]

  let dont_submit =
    value & flag & info ["n";"dry-run"] ~doc:
      "Perform the grading locally, but don't submit back to the server."

  let color_when =
    let when_enum = ["always", Some true; "never", Some false; "auto", None] in
    value & opt (enum when_enum) None & info ["color"] ~docv:"WHEN" ~doc:
      ("Colorise the output, and also allows use of UTF-8 characters. $(docv) \
        must be "^doc_alts_enum ~quoted:true when_enum ^".")

  let verbose =
    value & flag_all & info ["v";"verbose"] ~doc:
      "Be more verbose. Can be repeated."

  let apply solution_file exercise_id output_format dont_submit
        color_when verbose  =
    let color = match color_when with
      | Some o -> o
      | None -> Unix.(isatty stdout) && Sys.getenv_opt "TERM" <> Some "dumb"
    in
    {
      solution_file;
      exercise_id;
      output_format;
      submit = not dont_submit;
      color;
      verbosity = List.length verbose;
    }

  let term =
    Term.(const apply
          $solution_file $exercise_id $output_format $dont_submit
          $color_when $verbose )
end

module Args_fetch = struct
  let id =
    value & pos_all string [] & info [] ~docv:"EXERCISE_ID" ~doc:
      "Exercise identifier. Can be repeated. \
       If not present, all the exercises will be downloaded."

  let term = Term.(const (fun x -> x) $ id)
end

module ConfigFile = struct

  type t = {
    server: Uri.t;
    token: Token.t option;
  }

  let local_path, user_path =
    let ( / ) = Filename.concat in
    Sys.getcwd () / ".learnocaml-client",
    (try Sys.getenv "HOME" with Not_found -> ".")
    / ".config" / "learnocaml" / "client.json"

  let path ?(local=false) () =
    if local then
      if Sys.file_exists local_path then Some local_path else None
    else
      List.find_opt Sys.file_exists [local_path; user_path]

  let enc =
    let open Json_encoding in
    conv
      (fun {server; token} -> server, token)
      (fun (server, token) -> {server; token}) @@
    obj2
      (req "server" (conv Uri.to_string Uri.of_string string))
      (req "token" (option Token.(conv to_string parse string)))

  let read file =
    Lwt_io.with_file ~mode:Lwt_io.Input file Lwt_io.read >|=
    Ezjsonm.from_string >|=
    Json_encoding.destruct enc

  let write path t =
    Lwt_utils.mkdir_p (Filename.dirname path) >>= fun () ->
    Lwt_io.(with_file ~mode:Output ~perm:0o600 path) @@ fun oc ->
    Json_encoding.construct enc t |> function
    | `O _ | `A _ as json -> Lwt_io.write oc (Ezjsonm.to_string json)
    | _ -> assert false
end

module Console = struct

  let enable_colors = ref false
  let enable_utf8 = ref false

  let color cols =
    let code = function
      | `Bold -> "1"
      | `Underline -> "4"
      | `Crossed -> "9"
      | `Black -> "30"
      | `Red -> "31"
      | `Green -> "32"
      | `Yellow -> "33"
      | `Blue -> "34"
      | `Magenta -> "35"
      | `Cyan -> "36"
      | `White -> "37"
      | `Bg `Black -> "40;37"
      | `Bg `Red -> "41;37"
      | `Bg `Green -> "42;30"
      | `Bg `Yellow -> "43;30"
      | `Bg `Blue -> "44;37"
      | `Bg `Magenta -> "45;30"
      | `Bg `Cyan -> "46;30"
      | `Bg `White -> "47;30"
    in
    if !enable_colors then
      Printf.sprintf "\027[%sm%s\027[m"
        (String.concat ";" @@ (List.map code cols))
    else
      fun s -> s

  let status_line s =
    if !enable_colors then
      (flush stdout; Printf.eprintf "%s..%!\r\027[K" s)
    else
      Printf.eprintf "%s..\n" s

  let utf default c = if !enable_utf8 then c else default

  let block ?title ?border_color ?text_color ?(no_open=false) s =
    let top = utf "+" "\xe2\x94\x8c\xe2\x94\x80" (*U+250C U+2500*) in
    let left = utf "|" "\xe2\x94\x82" (*U+2502*) in
    let bottom = utf "`" "\xe2\x94\x94\xe2\x94\x80" (*U+2514 U+2500*) in
    let buf = Buffer.create (String.length s + String.length s / 10) in
    let oc = function None -> (fun s -> s) | Some col -> color col in
    if not no_open then
      Buffer.add_string buf (oc border_color top);
    (match title with
     | None -> ()
     | Some s ->
         match String.split_on_char '\n' s with
         | s1::r ->
             if not no_open then
               Buffer.add_string buf " ";
             Buffer.add_string buf s1;
             Buffer.add_char buf '\n';
             List.iter
               (fun si ->
                  if not no_open then
                    (Buffer.add_string buf left;
                     Buffer.add_string buf "  ");
                  Buffer.add_string buf si;
                  Buffer.add_char buf '\n')
               r;
             if not no_open then
               Buffer.add_string buf left;
         | [] -> ());
    if not no_open then
      Buffer.add_char buf '\n';
    List.iter
      (fun s ->
         Buffer.add_string buf (oc border_color left);
         Buffer.add_string buf "  ";
         Buffer.add_string buf (oc text_color s);
         Buffer.add_char buf '\n')
      (String.split_on_char '\n' s);
    Buffer.add_string buf (oc border_color bottom);
    Buffer.add_char buf '\n';
    Buffer.contents buf

  let button col s =
    let left = utf "<" "\xe2\x9d\xb0" (*U+2770*) in
    let right = utf ">" "\xe2\x9d\xb1" (*U+2771*) in
    color [`Bg col] left ^ color [`Bg col] s ^ color [`Bg col] right

  let hline ?(width=80) () =
    let c = utf "-" "\xe2\x94\x80" (*U+2500*) in
    let ln = String.length c in
    let b = Bytes.create (width * ln + 1) in
    for i = 0 to width - 1 do String.blit c 0 b (i * ln) ln done;
    Bytes.set b (width * ln) '\n';
    Bytes.to_string b

  let rec input ?default parse =
    flush stderr;
    let on_empty () =
      match default with
      | Some d -> d
      | None ->
          Printf.eprintf "I beg you pardon? %!";
          input ?default parse
    in
    try match read_line () with
      | "" -> on_empty ()
      | s ->
          try parse s with Failure msg ->
            Printf.eprintf "Invalid input: %s\nPlease try again: %!" msg;
            input ?default parse
    with
    | End_of_file -> prerr_newline (); on_empty ()
    | Sys.Break as e -> prerr_newline (); raise e

  let yesno ?(default=false) () =
    input ~default (fun s -> match String.lowercase_ascii s with
        | "y" | "yes" -> true
        | "n" | "no" -> false
        | _ -> failwith "please answer 'y' or 'n'.")

end

let get_score =
  let open Learnocaml_report in
  let rec get_score report =
    List.fold_left (fun acc -> function
        | Section (_text, report) -> get_score acc report
        | SectionMin (_text, report, min) -> get_score acc report |> max min
        | Message (_text, status) -> match status with
          | Success i -> acc + i
          | Penalty i -> acc - i
          | _ -> acc)
      report
  in
  get_score 0

let max_score exo = Learnocaml_exercise.(access File.max_score exo)

let print_score ?(max=1) ?color i =
  let color = match color with
    | None -> if i <= 0 then `Red else if i >= max then `Green else `Yellow
    | Some c -> c
  in
  if i <= 1 then
    Console.button color (Printf.sprintf " %3d pt  " i)
  else
    Console.button color (Printf.sprintf " %3d pts " i)

let console_report ?(verbose=false) ex report =
  let open Console in
  let open Learnocaml_report in
  let score = get_score report in
  let max_score = max_score ex in
  print_string (hline ());
  Printf.printf
    "## %-*s %s\n"
    (65 + String.length (color [`Bold] ""))
    (color [`Bold]
       (if score <= 0 then "Exercise failed"
        else if score >= max_score then "Exercise complete"
        else Printf.sprintf "Exercise incomplete (%02d%%)" (100 * score / max_score)))
    (print_score ~max:max_score score);
  print_string (hline ());
  print_newline ();
  let format_text t =
    String.concat " " @@ List.map (function
        | Text w -> w
        | Break -> "\n"
        | Code s when String.contains s '\n' -> "\n"^block ~border_color:[`Cyan] s
        | Code s -> color [`Cyan] s
        | Output s -> block ~border_color:[`Yellow] s)
      t
  in
  let rec all_good report =
    (List.for_all @@ function
      | Section (_, report) | SectionMin (_, report, _) -> all_good report
      | Message (_, (Success _ | Penalty _
                   | Informative | Warning | Important)) -> true
      | Message (_, Failure) -> false)
      report
  in
  let rec format_item = function
    | Section (text, report) | SectionMin (text, report, _) ->
        let good = all_good report in
        let score = get_score report in
        let title =
          let color =
            if good then `Green else if score = 0 then `Red else `Yellow
          in
          print_score ~color score ^ " " ^ format_text text
        in
        if not verbose && all_good report then
          title
        else
          "\n" ^ block ~title ~no_open:true
            (String.concat "\n" @@ List.map format_item report)
    | Message (text, Success i) ->
        print_score i ^ "   " ^ format_text text
    | Message (text, Penalty i) ->
        print_score (-i) ^ "   " ^ format_text text
    | Message (text, Failure) ->
        print_score 0 ^ "   " ^ format_text text
    | Message (text, Warning) ->
        color [`Bg `Yellow] "[ warning ]" ^ "   " ^ format_text text
    | Message (text, Informative) ->
        format_text text
    | Message (text, Important) ->
        color [`Bg `Cyan]   "[important]" ^ "   " ^ format_text text
  in
  List.iter (fun i -> print_endline (format_item i)) report;
  print_newline ()

module Api_client = Learnocaml_api.Client (Learnocaml_store.Json_codec)

let fetch server_url req =
  let url path args =
    let path = String.concat "/" (Uri.path server_url :: path) in
    let uri = Uri.with_path server_url path in
    Uri.with_query' uri args
  in
  let open Cohttp in
  let open Cohttp_lwt_unix in
  let do_req = function
    | { Learnocaml_api.meth = `GET; path; args } ->
        Client.get (url path args)
    | { Learnocaml_api.meth = `POST body; path; args } ->
        Client.post ~body:(Cohttp_lwt.Body.of_string body) (url path args)
  in
  Api_client.make_request
    (fun http_request ->
       do_req http_request >>= function
       | {Response.status = `OK; _}, body ->
           Cohttp_lwt.Body.to_string body >|= fun s -> Ok s
       | {Response.status = `Not_found; _}, _ ->
           Lwt.return (Error `Not_found)
       | {Response.status; _}, _ ->
           Lwt.return (Error (`Failure (Code.string_of_status status))))
    req
  >>= function
  | Ok x -> Lwt.return x
  | Error `Not_found -> raise Not_found
  | Error (`Failure s) -> Lwt.fail_with ("Server request failed: "^ s)

let fetch_exercise server_url token id =
  Lwt.catch (fun () -> fetch server_url (Api.Exercise (token, id, false)))
  @@ function
  | Not_found ->
      Printf.ksprintf Lwt.fail_with
        "Exercise %S was not found on the server."
        id
  | e -> Lwt.fail e

let upload_save server_url token save =
  Lwt.catch (fun () -> fetch server_url (Api.Update_save (token, save)))
  @@ function
  | e ->
      Printf.ksprintf Lwt.fail_with
        "Could not upload the results to the server: %s"
        (match e with Failure s -> s | e -> Printexc.to_string e)

let upload_report server token ex solution report =
  let score = get_score report in
  let max_score = max_score ex in
  let id = Learnocaml_exercise.(access File.id ex) in
  let mtime = Unix.gettimeofday () in
  let exercise_state =
    { Answer.
      solution;
      grade = if max_score = 0 then None else Some (score * 100 / max_score);
      report = Some report;
      mtime;
    }
  in
  let new_save =
    { Save.
      nickname = "";
      all_exercise_editors = SMap.empty;
      all_exercise_states = SMap.singleton id exercise_state;
      all_toplevel_histories = SMap.empty;
      all_exercise_toplevel_histories = SMap.empty;
    }
  in
  Lwt.catch (fun () -> upload_save server token new_save)
  @@ function
  | Not_found ->
      Printf.ksprintf Lwt.fail_with
        "Token %S not found on the server before upload."
        (Token.to_string token)
  | e -> Lwt.fail e

  (** [is_supported cached_version req] checks if the request is server-compat.
      (and if the client Learnocaml_version.v >= the server_version)

      [is_supported (Some version) req] = Ok version, if it's compatible,
      [is_supported (Some version) req] = Error message, if it's not,
      [is_supported None req] = let v = GET Version in Ok v, if it's compatible,
      [is_supported None req] = let v = GET Version in Error m, if it's not;

      and [is_supported None Version] will also do a GET Version request *)
  let is_supported_server
    : type resp.
      Api.Compat.t option -> Uri.t -> resp Api.request -> (Api.Compat.t, string) result Lwt.t
    = fun server_version server req ->
        (match server_version with
        | Some server_version -> Lwt.return server_version
        | None ->
           fetch server (Api.Version ()) >|= fun (server_version, _todo) ->
           Api.Compat.v server_version) >|= fun server_version ->
        match Api.is_supported ~server:server_version req with
      | Ok () -> Ok server_version
      | Error msg -> Error msg

let check_server_version ?(allow_static=false) server =
  Lwt.catch (fun () ->
      is_supported_server
        None (* if need be: Implement some server_version cache *)
        server
        (Api.Version ()) (* TODO: pass more precise requests *)
      >|= function
      | Ok server_version -> Some server_version
      | Error msg -> (* See [Learnocaml_api.is_supported]'s message *)
         Printf.eprintf
           "[ERROR] %s\nDo you use the latest learn-ocaml-client binary?\n" msg;
         exit 70)
  @@ fun e ->
     if not allow_static then
       begin
         Printf.eprintf "[ERROR] Could not reach server: %s\n"
           (match e with
            | Unix.Unix_error (err, _, _) -> Unix.error_message err
            | Failure m -> m
            | e -> Printexc.to_string e);
         exit 1
       end
     else
       Lwt.return_none

let get_server =
  let default_server = Uri.of_string "http://learn-ocaml.org" in
  function
  | Some s -> s
  | None ->
     Printf.eprintf
       "Please specify the address of the learn-ocaml server to use \
        [default: %s]: " (Uri.to_string default_server);
     let uri s =
       let u = Uri.of_string s in
       match Uri.scheme u with
       | None -> Uri.with_scheme u (Some "http")
       | Some ("http" (* | "https" *)) -> u
       | Some s ->
          failwith (Printf.sprintf
                      "unsupported scheme %S, please use http://."
                      s)
     in
     Console.input ~default:default_server uri

let get_nonce_and_create_token server nickname secret_candidate =
  let secret_candidate = Sha.sha512 secret_candidate in
  fetch server (Api.Nonce ())
  >>= fun nonce ->
  fetch server
    (Api.Create_token (Sha.sha512 (nonce ^ secret_candidate), None, nickname))

let get_config_option ?local ?(save_back=false) ?(allow_static=false) server_opt token_opt =
  match ConfigFile.path ?local () with
  | Some f ->
      ConfigFile.read f >>= fun c ->
      let c = match server_opt, token_opt with
        | Some server, Some _ -> { ConfigFile.server=server; ConfigFile.token=token_opt }
        | Some server, None -> { c with ConfigFile.server }
        | None, Some _ -> { c with ConfigFile.token=token_opt}
        | None, None -> c
      in
      check_server_version ~allow_static c.ConfigFile.server
      >>= fun _version -> (* could use this arg like get_config_option_server *)
      (
        if save_back
        then
          ConfigFile.write f c >|= fun () ->
          Printf.eprintf "Configuration written to %s\n%!" f
        else
          Lwt.return_unit
      )
      >|= fun () -> Some c
  | None -> Lwt.return_none

let get_config ?local ?(save_back=false) ?(allow_static=false) server_opt token_opt =
  get_config_option ?local ~save_back ~allow_static server_opt token_opt
  >>= function
  | Some c -> Lwt.return c
  (* TODO: Make it possible to change this error message (from get_config_o) *)
  | None -> Lwt.fail_with "No config file found. Please do `learn-ocaml-client init`"

let man p = 
  let open Manpage in
  [
    `S s_description;
    `P p;
    `S s_options;
    `S s_authors;
    `P "The original authors are Benjamin Canou, \
        Çağdaş Bozman, Grégoire Henry and Louis Gesbert (OCamlPro). \
        It is licensed under the MIT License.";
    `S s_bugs;
    `P "Bugs should be reported to \
        $(i,https://github.com/ocaml-sf/learn-ocaml/issues)";
  ]

let get_config_o ?save_back ?(allow_static=false) o =
  let open Args_global in
  get_config ~local:o.local ?save_back ~allow_static o.server_url o.token

(* Likewise, but without dealing with tokens *)

let get_config_option_server ?local ?(save_back=false) ?(allow_static=false) server_opt =
  match ConfigFile.path ?local () with
  | Some f ->
      ConfigFile.read f >>= fun c ->
      let c = match server_opt with
        | Some server -> { c with ConfigFile.server }
        | None -> c
      in
      check_server_version ~allow_static c.ConfigFile.server
      >>= fun server_version ->
      (
        if save_back
        then
          ConfigFile.write f c >|= fun () ->
          Printf.eprintf "Configuration written to %s\n%!" f
        else
          Lwt.return_unit
      )
      >|= fun () -> (Some c, server_version)
  | None ->
     match server_opt with
     | Some server ->
        let c = ConfigFile.{server; token = None} in
        check_server_version ~allow_static server
        >>= fun server_version ->
        (* Note: could raise an error if save_back=true *)
        Lwt.return (Some c, server_version)
     | None -> Lwt.return (None, None)

let get_config_server ?local ?(save_back=false) ?(allow_static=false) server_opt =
  get_config_option_server ?local ~save_back ~allow_static server_opt
  >>= function
  | Some c, o -> Lwt.return (c, o)
  (* TODO: Make it possible to change this error message (from get_config_o_server) *)
  | None, _ -> Lwt.fail_with "No config file found. Please do `learn-ocaml-client init`, or pass a --server=\"URL\" option"

let get_config_o_server ?save_back ?(allow_static=false) o =
  let open Args_server in
  get_config_server ~local:o.local ?save_back ~allow_static o.server_url

module Init = struct
  open Args_global
  open Args_create_token
     
  let init global_args create_token_args =
    let path = if global_args.local then ConfigFile.local_path else ConfigFile.user_path in
    let get_token server =
      match global_args.token with
      | Some token -> Lwt.return token
      | None ->
         match create_token_args.nickname with
         | Some n -> (get_nonce_and_create_token server (Some n) create_token_args.secret)
         | _ -> Lwt.fail_with "You must provide a token or a nickname+secret pair."
    in
    let get_server () =
      match global_args.server_url with
      | None -> Lwt.fail_with "You must provide a server."
      | Some s -> Lwt.return s
    in
    get_server () >>= fun server ->
    check_server_version ~allow_static:true server >>= fun has_server ->
    let token = if has_server <> None then
                  get_token server >>= Lwt.return_some
                else
                  Lwt.return_none in
    token >>= fun token ->
    let config = { ConfigFile. server; token=token } in
    ConfigFile.write path config >|= fun () ->
    Printf.eprintf "Configuration written to %s.\n%!" path;
    0

  let man = man "Initialize the configuration file with the server, and \
                 a token or a nickname+secret pair"

  let exits =
    let open Cmd.Exit in
    [ info ~doc:"Default exit." ok
    ; info ~doc:"Input error: unable to find a server URL or connection token." 1
    ; info ~doc:"The client's version is incompatible (too old?) w.r.t. the server." 70
    ]

  let info =
    Cmd.info 
      ~man
      ~exits
      ~sdocs:Manpage.s_options
      ~doc:"Initialize the configuration file."
      "init"
  
  let term =
    Term.(
      const (fun go co -> Stdlib.exit (Lwt_main.run (init go co)))
      $ Args_global.term $ Args_create_token.term) 
end

module Grade = struct
  open Args_exercises
  let grade go eo =
    Console.enable_colors := eo.color;
    Console.enable_utf8 := eo.color;
    get_config_o ~allow_static:true go
    >>= fun { ConfigFile.server; token } ->
    let status_line =
      if eo.verbosity >= 2 then Printf.eprintf "%s..\n" else Console.status_line
    in
    let solution, exercise_id =
      match eo.solution_file, eo.exercise_id with
      | None, _ -> Printf.eprintf "You must specify a file to grade.\n%!"; exit 2
      | Some f, Some id -> f, id
      | Some f, None ->
         let id = Filename.remove_extension f in
         f, id
    in
    status_line "Reading solution.";
    Lwt_io.with_file ~mode:Lwt_io.Input solution Lwt_io.read
    >>= fun solution ->
    status_line "Fetching exercise data from server.";
    fetch_exercise server token exercise_id
    >>= fun (_meta, exercise, deadline) ->
    if deadline = Some 0. then
      Printf.eprintf
        "[ERROR] The deadline is expired, you won't be able to submit.\n";
    Grading_cli.get_grade ~callback:status_line ?timeout:None
      exercise solution
    >>= fun (report, ex_stdout, ex_stderr, ex_outcome) ->
    flush stderr;
    let pr col title s =
      if eo.verbosity >= 1 then
        let s = String.trim s in
        if s <> "" then
          prerr_string (Console.block ~title ~border_color:[col] s)
    in
    pr `Green "stdout" ex_stdout;
    pr `Red "stderr" ex_stderr;
    pr `Cyan "outcome" ex_outcome;
    if eo.verbosity >= 1 then prerr_newline ();
    match report with
    | Error err ->
       Printf.eprintf "[ERROR] Could not do the grading:\n%s\n"
         (Grading.string_of_err err);
       Lwt.return 10
    | Ok report ->
       (match eo.output_format with
        | `Console -> console_report ~verbose:(eo.verbosity > 0) exercise report
        | `Raw ->
           Report.print Format.std_formatter report
        | `Html ->
           Report.output_html Format.std_formatter report
        | `Json ->
           match Json_encoding.construct Report.enc report
           with
           | `O _ | `A _ as json -> Ezjsonm.to_channel ~minify:false stdout json
           | _ -> assert false);
       if deadline = Some 0. then
         (Printf.eprintf "Results NOT saved to server (deadline expired)\n";
          Lwt.return 1)
       else
         match eo.submit, token with
         | true, Some token ->
            upload_report server token exercise solution report >>= fun _ ->
            Printf.eprintf "Results saved to server\n"; Lwt.return 0
         | _ -> Lwt.return 0

  let man =
    man
      "Grades an OCaml exercise using a learn-ocaml server, and submits \
        solutions."

  let exits =
    let open Cmd.Exit in
    [ info ~doc:"Default exit." ok
    ; info ~doc:"Server-side error: either unable to reach the server or request has been failed." 1
    ; info ~doc:"Input error: unable to find a file to grade or token isn't valid." 2
    ; info ~doc:"Grading failed." 10
    ; info ~doc:"The client's version is incompatible (too old?) w.r.t. the server." 70
    ]

  let info =
    Cmd.info 
      ~man
      ~exits
      ~sdocs:Manpage.s_options
      ~doc:"Learn-ocaml grading client."
      "grade"        
  
  let term = 
    Term.(
      const (fun go eo -> Stdlib.exit (Lwt_main.run (grade go eo)))
      $ Args_global.term $ Args_exercises.term)
end

let use_global f =
  Term.(
    const (fun o -> Stdlib.exit (Lwt_main.run (f o)))
    $ Args_global.term)

module Print_token = struct
  let print_tok o =
    get_config_o o
    >>= fun config ->
    (match config.ConfigFile.token with
     | Some token -> Lwt_io.print (Token.to_string token ^ "\n")
     | None -> Lwt_io.print "Static server -- no token\n")
    >|= fun () -> 0

  let explanation = "Just print the configured user token."

  let man = man explanation

  let exits =
    let open Cmd.Exit in
    [ info ~doc:"Default exit." ok
    ; info ~doc:"Server error: could not reach the server or config file wasn't found." 1
    ; info ~doc:"The client's version is incompatible (too old?) w.r.t. the server." 70
    ]

  let info = Cmd.info ~man ~exits ~doc:explanation ~sdocs:Manpage.s_options "print-token"

  let term = use_global print_tok
end

module Print_server = struct
  let print_server o =
    get_config_o o
    >>= fun config ->
    Lwt_io.printl (Uri.to_string config.ConfigFile.server)
    >|= fun () -> 0
                
  let explanation = "Just print the configured server."
                  
  let man = man explanation
  
  let exits =
    let open Cmd.Exit in
    [ info ~doc:"Default exit." ok
    ; info ~doc:"Server error: could not reach the server or config file wasn't found." 1
    ; info ~doc:"The client's version is incompatible (too old?) w.r.t. the server." 70
    ]

  let info = Cmd.info ~man ~exits ~doc:explanation ~sdocs:Manpage.s_options "print-server"

  let term = use_global print_server        
    
end

module Args_server_version = struct
  type t = {
      minimum: bool;
    }

  let minimum =
    value & flag & info ["min"] ~doc:
      "Return the min of server and learn-ocaml-client versions. \
       This flag is useless for now as we only support backward-compatibility \
       (so an old learn-ocaml-client won't try to reach a more recent server) \
       but it is already provided if we later decide to relax this constraint."

  let apply minimum = {minimum}

  let term = Term.(const apply $ minimum)
end

module Server_version = struct
  open Args_server_version
  open Learnocaml_api

  let server_version server_args server_version_args =
    Lwt.catch
      (fun () ->
        get_config_o_server ~save_back:false ~allow_static:false server_args)
      begin fun e ->
      Lwt_io.eprintf "[ERROR] Input error: %s\n"
        (match e with
         | Unix.Unix_error (err, _, _) -> Unix.error_message err
         | Failure m -> m
         | e -> Printexc.to_string e)
      >>= fun () -> exit 2
      end >>= fun cf ->
    match cf with
    | ConfigFile.{server; token = _}, server_version ->
    (Lwt.catch (fun () ->
         is_supported_server
           server_version (* some server_version cache *)
           server
           (Api.Version ())
         >>= function
         | Ok server_version ->
            let version =
              let {minimum} = server_version_args in
              if minimum then
                let client_version = Compat.v Learnocaml_version.v in
                if Compat.le server_version client_version
                then server_version
                else client_version
              else server_version in
            Lwt_io.printl (Learnocaml_api.Compat.to_string version)
            >|= fun () -> 0
         (* TODO: Factor-out error messages *)
         | Error msg -> (* See [Learnocaml_api.is_supported]'s message *)
            Lwt_io.eprintf
              "[ERROR] %s\nDo you use the latest learn-ocaml-client binary?\n" msg
            >|= fun () -> 70)
     @@ fun e ->
        begin
          Lwt_io.eprintf "[ERROR] Could not reach server: %s\n"
            (match e with
             | Unix.Unix_error (err, _, _) -> Unix.error_message err
             | Failure m -> m
             | e -> Printexc.to_string e)
          >|= fun () -> 1
        end)

  let explanation =
    "Print the version of the server (from CLI or from the cookie file, which is kept untouched anyway)."

  let man = man explanation

  let exits =
    let open Cmd.Exit in
    [ info ~doc:"Default exit." ok
    ; info ~doc:"Unable to reach the server." 1
    ; info ~doc:"Input error: unable to find a server URL or config file." 2
    ; info ~doc:"The client's version is incompatible (too old?) w.r.t. the server." 70
    ]

  let info = Cmd.info ~man ~exits ~doc:explanation ~sdocs:Manpage.s_options "server-version"
  
  (* TODO: Generalize & Use [use_global] *)
  let term =
    Term.(
      const (fun o l -> Stdlib.exit (Lwt_main.run (server_version o l)))
      $ Args_server.term $ Args_server_version.term)
end

module Set_options = struct
  let set_opts o =
    get_config_o ~save_back:true ~allow_static:true o
    >|= fun _ -> 0

  let man =
    man
      "Overwrite the configuration file with the command-line options \
       ($(b,--server), $(b,--token))."

  let exits =
    let open Cmd.Exit in
    [ info ~doc:"Default exit." ok
    ; info ~doc:"Server error: config file wasn't found." 1
    ; info ~doc:"The client's version is incompatible (too old?) w.r.t. the server." 70
    ]

  let info =
    Cmd.info 
      ~man
      ~exits
      ~sdocs:Manpage.s_options
      ~doc:"Set configuration."
      "set-options"

  let term = use_global set_opts
end

let write_exercise_file id str =
  let f = Filename.concat (Sys.getcwd ()) (id ^ ".ml") in
  if Sys.file_exists f then
    (Printf.eprintf "File %s already exists, not overwriting.\n" f;
     Lwt.return false)
  else
    Lwt_io.(with_file ~mode:Output ~perm:0o600 f) @@ fun oc ->
        Lwt_io.write oc str >|= fun () ->
        Printf.printf "Wrote file %s\n%!" f;
        true

module Fetch = struct
  let fetch_save server_url token =
    Lwt.catch (fun () -> fetch server_url (Api.Fetch_save token))
    @@ function
      | Not_found ->
         Printf.ksprintf Lwt.fail_with
           "Token %S not found on the server."
           (Token.to_string token)
      | e -> Lwt.fail e

  let write_save_files lst save =
    let has_to_fetch x =
      (* When no exercise identifier was specified, fetch everything *)
      match lst with
      | [] -> true
      | _ -> List.mem x lst
    in
    let already_exists = ref 0 in
    Lwt_list.fold_left_s (fun acc (id, st) ->
      if not (has_to_fetch id) then Lwt.return acc
      else
      let f = Filename.concat (Sys.getcwd ()) (id ^ ".ml") in
      (if Sys.file_exists f then
         (Printf.eprintf "File %s already exists, not overwriting.\n" f;
          already_exists := !already_exists + 1;
         Lwt.return_unit)
      else
        Lwt_io.(with_file ~mode:Output ~perm:0o600 f) @@ fun oc ->
        Lwt_io.write oc st.Answer.solution >|= fun () ->
        Printf.eprintf "Wrote file %s\n%!" f)
      >|= fun () -> id::acc )
      []
      (SMap.bindings (save.Save.all_exercise_states))
    >>= fun actually_found ->
    let not_found = List.filter (fun x -> not (List.mem x actually_found)) lst in
    Lwt_list.iter_s
      (Lwt_io.eprintf
         ("Warning: exercise %s was not found on the server.\n"))
      not_found
    >|= fun () ->
    let first = if !already_exists = 0 then 0 else 1 in
    let second = if List.length not_found = 0 then 0 else 1 in
    first + 2*second

  let fetch o lst =
    get_config_o o
    >>= fun { ConfigFile.server; token } ->
    match token with
    | Some token -> fetch_save server token >>= write_save_files lst
    | None -> Lwt.return 0

  let man =
    man
      "Fetch the user's solutions on the server to the current directory."

  let exits =
    let open Cmd.Exit in
    [ info ~doc:"Default exit." ok
    ; info ~doc:"Either sever-side error or there was a file already present on the local side." 1
    ; info ~doc:"A specified exercise was not found on the server." 2
    ; info ~doc:"Both of 1 (for local side) and 2." 3
    ; info ~doc:"The client's version is incompatible (too old?) w.r.t. the server." 70
    ]

  let info =
    Cmd.info 
      ~man 
      ~exits
      ~sdocs:Manpage.s_options
      ~doc:"Fetch the user's solutions."
      "fetch"

  let term =
    Term.(
      const (fun o l -> Stdlib.exit (Lwt_main.run (fetch o l)))
      $ Args_global.term $ Args_fetch.term)
end

module Create_token = struct
  open Args_create_token

  let create_tok server_url co =
    match co.nickname with
    | None -> Lwt_io.print "You must provide a nickname\n"
              >|= fun () -> 2
    | Some nickname ->
       get_config_option server_url None
       >>= fun config ->
       let server =
         match config with
         | Some c -> c.ConfigFile.server
         | None -> get_server server_url
       in
       get_nonce_and_create_token server (Some nickname) co.secret
       >>= fun tok ->
       Lwt_io.print (Token.to_string tok ^ "\n")
       >|= fun () -> 0

  let man = man "Create a token on the server with the desired nickname.\
                 Prodiving a token will test if it exists on the server."

  let exits =
    let open Cmd.Exit in
    [ info ~doc:"Default exit." ok
    ; info ~doc:"Server error: either unable to reach the server or request has been failed." 1
    ; info ~doc:"Input error: nickname wasn't provided." 2
    ; info ~doc:"The client's version is incompatible (too old?) w.r.t. the server." 70
    ]

  let info =
    Cmd.info 
      ~man
      ~exits
      ~sdocs:Manpage.s_options
      ~doc:"Create a token."
      "create-token"
  
  let term =
    Term.(
      const (fun go co -> Stdlib.exit (Lwt_main.run (create_tok go co)))
      $ Args_global.term_server $ Args_create_token.term)
end

module Template = struct
  open ConfigFile

  let template o exercise_id =
    match exercise_id with
    | None -> Lwt.fail_with "You must provide an exercise id"
              >|= fun () -> 2
    | Some exercise_id ->
       get_config_o ~allow_static:true o
       >>= fun { server; token } ->
       fetch_exercise server token exercise_id
       >>= fun (_meta, exercise, _deadline) ->
       write_exercise_file
         exercise_id
         Learnocaml_exercise.(access File.template exercise)
       >|= function
       | true -> 0
       | false -> 3

  let man = man "Get the template of a given exercise"

  let exits =
    let open Cmd.Exit in
    [ info ~doc:"Default exit." ok
    ; info ~doc:"Sever error: unable to find config file or request has been failed." 1
    ; info ~doc:"Input error: exercise id wasn't provided." 2
    ; info ~doc:"Exercise with given id already exists." 3
    ; info ~doc:"The client's version is incompatible (too old?) w.r.t. the server." 70
    ]

  let info =
    Cmd.info 
      ~man
      ~exits
      ~sdocs:Manpage.s_options
      ~doc:"Get the template of a given exercise."
      "template"

  let term =
    Term.(
      const (fun o id -> Stdlib.exit (Lwt_main.run (template o id)))
      $ Args_global.term $ Args_exercise_id.term)
end

module Exercise_list = struct
  let doc= "Get a structured json containing a list of the exercises of the server"

  let exercise_list o  =
    get_config_o ~allow_static:true o
    >>= fun {ConfigFile.server;token} ->
    fetch server (Learnocaml_api.Exercise_index token)
    >>= (fun index->
    let open Json_encoding in
    let ezjsonm = (Json_encoding.construct
                  (tup2 Exercise.Index.enc (assoc float))
                  index)
    in
    let json =
           match ezjsonm with
           | `O _ | `A _ as json -> json 
           | _ -> assert false
    in
    Ezjsonm.to_channel ~minify:false stdout json;
    Lwt.return 0;)

  let man = man doc

  let exits =
    let open Cmd.Exit in
    [ info ~doc:"Default exit." ok
    ; info ~doc:"Sever error: unable to find config file or request has been failed." 1
    ; info ~doc:"The client's version is incompatible (too old?) w.r.t. the server." 70
    ]

  let info = Cmd.info ~man ~exits ~doc:doc ~sdocs:Manpage.s_options "exercise-list"

  let term = use_global exercise_list
end

module Main = struct
  open Manpage
  
  let man =
    [
    `S s_description;
    `P "A Learn-ocaml CLI that allows one to communicate directly with a Learn-ocaml server \
        in the same way as the web application does. Default command is $(b,grade). \
        Use either '$(b,learn-ocaml-client <command> --help)' for more information on a specific command.";
    `S s_commands;
    `S "GRADE";
    `P "From now on, this manual describes the specification of the default $(b,grade) command, that could \
        also be retrieved by '$(b,learn-ocaml-client grade --help)'.";
    `S s_arguments;
    `S s_options;
    `S s_authors;
    `P "The original authors are Benjamin Canou, \
        Çağdaş Bozman, Grégoire Henry and Louis Gesbert. It is licensed under \
        the MIT License.";
    `S s_bugs;
    `P "Bugs should be reported to \
        $(i,https://github.com/ocaml-sf/learn-ocaml/issues)";
    ]

  let info = 
    Cmd.info
      ~version 
      ~man
      ~exits:Grade.exits
      ~sdocs:s_options
      ~doc:"Learn-ocaml client CLI."
      "learn-ocaml-client"
  
  let term = Grade.term
        
end

let () =
  let open Cmd in 
  let cmd = group ~default:Main.term Main.info
    [ v Init.info Init.term
    ; v Grade.info Grade.term
    ; v Print_token.info Print_token.term
    ; v Set_options.info Set_options.term
    ; v Fetch.info Fetch.term
    ; v Print_server.info Print_server.term
    ; v Server_version.info Server_version.term
    ; v Template.info Template.term
    ; v Create_token.info Create_token.term
    ; v Exercise_list.info Exercise_list.term ] in
  match Cmd.eval_value ~catch:false cmd with
  | exception Failure msg ->
      Printf.eprintf "[ERROR] %s\n" msg;
      exit 1
  | Error _ -> exit 2
  | _ -> exit 0
