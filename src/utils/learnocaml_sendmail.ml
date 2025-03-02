(* -*- coding: utf-8-unix; -*- *)
(* This file is part of Learn-OCaml.
 *
 * Copyright (C) 2019-2025 OCaml Software Foundation.
 * Copyright (C) 2016-2018 OCamlPro.
 *
 * Learn-OCaml is distributed under the terms of the MIT license. See the
 * included LICENSE file for details. *)

(* We don't use
   [ocamlnet](https://gitlab.com/gerdstolpmann/lib-ocamlnet3/-/blob/master/code/src/netstring/netsendmail_tut.txt)
   anymore as it requires "ocaml" < 5 *)

(*
  LEARNOCAML_BASE_URL: "http://localhost:8080"
  # (ocaml variable) <container_name>.<network_name>:
  FROM_DOMAIN: "backend.localdomain"
  # (smtp variable) hostname of the SMTP server:
  SMTPSERVER: "maildev"
  # SMTPSERVER: "postfix"
  # (ocaml + smtp variable) Reply-To = Return-Path:
  EMAIL: "noreply@example.com"
 *)

let smtp_server =
  Sys.getenv_opt "LEARNOCAML_SMTP_SERVER"

let smtp_auth_user =
  Sys.getenv_opt "LEARNOCAML_SMTP_AUTH_USER"

let smtp_auth_passwd =
  Sys.getenv_opt "LEARNOCAML_SMTP_AUTH_PASSWD"

let smtp_port =
  Sys.getenv_opt "LEARNOCAML_SMTP_PORT"

let smtp_from_email = (* Reply-To = Return-Path *)
  Sys.getenv_opt "LEARNOCAML_SMTP_FROM_EMAIL"
  |> Option.value
       ~default:"Learn-OCaml <learnocaml@backend.localdomain>"
       (* TODO Extract domain name from LEARNOCAML_BASE_URL *)

open Letters

(* # with_starttls vs. smtp_port *)
let conf_letters =
  match smtp_server, smtp_auth_user, smtp_auth_passwd with
  | Some server, Some user, Some passwd ->
     Some (Config.create ~username:user ~password:passwd ~hostname:server ~with_starttls:false ())
  | Some server, _, _ ->
     Some (Config.create ~username:"" ~password:"" ~hostname:server ~with_starttls:false ())
  | None, _, _ ->
     None

(* XXX The following format strings must not contain unsafe HTML chars
   ('<', '>', '"', '&'), as they are not escaped *)

let hello : (string -> string, unit, string) format =
  {|Hello%s,
|}

let confirm : (string -> string, unit, string) format =
  {|
Please follow the link below to confirm your e-mail address:

%s
|}

let confirm_subject = "Confirm your e-mail address"
let change_new_subject = "Confirm your new e-mail address"
let change_old_subject = "Changing your e-mail address"

let change_common : (string -> string -> string, unit, string) format =
  {|
You requested to change your e-mail address on the server.
Old address: %s
New address: %s
|}

let change_old : (string -> string, unit, string) format =
  {|
An e-mail has been sent to the new address for you to confirm it.
Please check your corresponding mailbox (%s).
|}

let change_new : (string -> string, unit, string) format =
  {|
Please follow the link below to confirm this change:

%s
|}

let reset : (string -> string, unit, string) format =
  {|
Someone (probably you) requested changing your Learn-OCaml password.

Please follow the following link to do so:

%s

Otherwise, no further action is required.

Note: the reset link will expire in 4 hours.
|}

let reset_subject = "Change your password"

let closing : string =
  {|
The Learn-OCaml server.|}

(***************************************************************)
(* Now the following helper strings & functions deal with HTML *)

(* TODO Checkout Netencoding spec, find-out implem *)
let encode_html_utf8 s = s
(*
  let encode_html_utf8 =
  Netencoding.Html.encode
  ~in_enc:`Enc_utf8
  ~out_enc:`Enc_utf8
  ~prefer_name:true
  ~unsafe_chars:Netencoding.Html.unsafe_chars_html4 ()

  let encode_url = Netencoding.Url.encode ~plus:false
 *)

let link_format : (string -> string -> string, unit, string) format =
  {|<a href="%s">%s</a>|}

(* XXX The message language is hardcoded here: "en" *)
let html_format : (string -> string -> string, unit, string) format =
  {|<!DOCTYPE html>
<html lang="en">
<head><meta charset="UTF-8"><title>%s</title></head>
<body>
<p>%s</p>
</body>
</html>|}

let wrap_url url =
  Printf.sprintf link_format url (encode_html_utf8 url)

let wrap_html ~title text =
  let lines = Str.global_replace (Str.regexp "$") "<br>" text in
  Printf.sprintf html_format ((*encode_html_utf8*) title) lines

let send_email
      ~(nick : string option) ~to_addr ~subject
      ?(hello=hello) ?(pretext="") ~text ?(posttext=closing) url =
  let padding, nickname =
    match nick with
    | None | Some "" -> "", ""
    | Some nickname -> " ", nickname in
  let str_plain = Printf.sprintf hello (padding ^ nickname)
                  ^ pretext
                  ^ Printf.sprintf text url
                  ^ posttext in
  match conf_letters with
  | Some conf ->
     let str_html =
       wrap_html ~title:subject
         (Printf.sprintf hello (padding ^ nickname)
          ^ pretext
          ^ Printf.sprintf text (wrap_url url)
          ^ posttext) in
     let sender = smtp_from_email in
     let recipients = [ To to_addr; ] in
     let text = str_plain in
     let html = str_html in
     let email = create_email ~reply_to: sender ~from:sender ~recipients ~subject ~body:(Mixed (text, html, None)) () in
     begin match email with
     | Ok message ->
        Printf.printf {|[INFO] mailto:%s?subject="%s"
                       %!|} to_addr subject;
        send ~config:conf ~sender ~recipients ~message
     | Error err ->
        Printf.printf {|[ERROR] %s (*
Can't mailto:%s?subject="%s" with body """
%s
""" *)
%!|} err to_addr subject str_plain; Lwt.return_unit
     end
  | None -> Printf.printf {|[WARNING] Environment variables LEARNOCAML_SMTP_SERVER and LEARNOCAML_SMTP_FROM_EMAIL must be set! (*
Can't mailto:%s?subject="%s" with body """
%s
""" *)
%!|} to_addr subject str_plain; Lwt.return_unit

(* let charset = ["charset", Netmime_string.mk_param "utf-8"] in *)
(* XXX as Netsendmail doesn't support Reply-To, we use From *)
(* If need be
let check_email email =
  try match Netaddress.parse email with
      | [`Mailbox _] (* a single mail *) -> ()
      | _ -> invalid_arg "check_email: no single email"
  with
  | Netaddress.Parse_error (_i, str) -> invalid_arg ("check_email: " ^ str)
 *)

(* Sendmail_lwt/Colombe:
   (* let domain_of_string str =
   match Domain_name.of_string str with
   | Ok raw_domain ->
   (match Domain_name.host raw_domain with
   | Ok domain -> domain
   | Error (`Msg error) -> (*XX*)failwith error)
   | Error (`Msg error) -> (*XX*)failwith error in
   let dest = domain_of_string ((*XX*)Option.get smtp_server) in
   let from_domain = domain_of_string ((*XX*)Option.get from_domain) in

   (* XXX replace send_mail with submit
   when replacing maildev with external SMTP using port 465 *)
   Sendmail_lwt.sendmail ~destination:(`Domain_name dest) ~port:1025 ~domain:(Colombe.Domain.Domain ["backend"; "localdomain"])
   (Colombe.Reverse_path (Some (Colombe.Path. ?)
   Colombe.Forward_path; *)
   sendmail ~mailer ~crlf:false mail;
 *)

open Lwt.Infix

let confirm_email ~(nick:string option) ~(url:string) to_addr =
  send_email ~nick ~to_addr ~subject:confirm_subject
    ~text:confirm url

let change_email ~(nick:string option) ~(url:string) old_email new_email =
  send_email ~nick ~to_addr:new_email
    ~subject:change_new_subject
    ~pretext:(Printf.sprintf change_common old_email new_email)
    ~text:change_new url >>= fun () ->
  send_email ~nick ~to_addr:old_email
    ~subject:change_old_subject
    ~pretext:(Printf.sprintf change_common old_email new_email)
    ~text:change_old ("mailto:" ^ new_email)

let reset_password ~(nick:string option) ~(url:string) to_addr =
  send_email ~nick ~to_addr ~subject:reset_subject
    ~text:reset url
