(* This file is part of Learn-OCaml.
 *
 * Copyright (C) 2021 Learn-OCaml developers
 *
 * Learn-OCaml is distributed under the terms of the MIT license. See the
 * included LICENSE file for details. *)

open Learnocaml_common

module H = Tyxml_js.Html

let rec drop_2_trailing = function
  | [] | [_] | [_; _] -> []
  | x :: l -> x :: drop_2_trailing l

(* Replace location: from [http://localhost:8080/.../...]
   to [http://localhost:8080] *)
let redirect () =
  let open Js_of_ocaml__Url in
  match Url.Current.get () with
  | Some (Http http_url) ->
     let new_url = {http_url with hu_path = drop_2_trailing http_url.hu_path; hu_arguments = []} in
     Url.Current.set (Http new_url)
  | Some (Https http_url) ->
     let new_url = {http_url with hu_path = drop_2_trailing http_url.hu_path; hu_arguments = []} in
     Url.Current.set (Https new_url)
  | Some _ | None -> ()

let new_path (kind, id) =
  match kind with
  | "exercise" -> ["exercises";id;""]
  | "playground" -> ["playground";id;""]
  | "lesson" -> []
  | _ -> failwith "never append"

let new_args (kind, id) =
  match kind with
  | "exercise" -> "tab=text"
  | "playground" -> "tab=toplevel"
  | "lesson" -> "activity=lessons&lesson"^id
  | _ -> failwith "never append"

let redirect_many (kind, id) () =
  let open Js_of_ocaml__Url in
  match Url.Current.get () with
  | Some (Http http_url) ->
     let new_url = {http_url with hu_path = new_path (kind,id); hu_fragment = new_args (kind,id); hu_arguments = []} in
     Url.Current.set (Http new_url)
  | Some (Https http_url) ->
     let new_url = {http_url with hu_path = new_path (kind,id); hu_fragment = new_args (kind,id); hu_arguments = []} in
     Url.Current.set (Https new_url)
  | Some _ | None -> ()

let () =
  (match Js_utils.get_lang () with Some l -> Ocplib_i18n.set_lang l | None -> ());
  let args = Url.Current.arguments in
  match List.assoc_opt "kind" args, List.assoc_opt "id" args, List.assoc_opt "many" args with
    | Some kind, Some id, Some _ -> cb_alert ~title:[%i"Redirection"] ([%i "Error: too many parameters specified. Redirecting to "]^kind^" "^id^".") (redirect_many (kind,id))
    | Some kind, Some id, None ->  cb_alert ~title:[%i"Redirection"] ([%i"Error: "]^kind^" "^id^[%i" was not found. Redirecting to the homepage."]) redirect
    | _ -> cb_alert ~title:[%i"Redirection"] [%i "Redirecting to the homepage."] redirect
  
