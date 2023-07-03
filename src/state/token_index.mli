(* This file is part of Learn-OCaml.
 *
 * Copyright (C) 2019-2020 OCaml Software Foundation.
 * Copyright (C) 2016-2018 OCamlPro.
 *
 * Learn-OCaml is distributed under the terms of the MIT license. See the
 * included LICENSE file for details. *)

(** Used both for file i/o and request handling *)
module Json_codec: Learnocaml_api.JSON_CODEC

module type IndexRW = sig
  type t

  val init : unit -> t
  val read : t -> string -> (string -> 'a) -> 'a Lwt.t
  val write : t -> string -> ('a -> string) -> 'a -> unit Lwt.t
end

module IndexFile: IndexRW

module TokenIndex: sig
  (** Create or regenerate token index from sync/ and write sync/token.json.
      This step may take a long time (up to several minutes).  Automatically
      called (once and for all) by [get_tokens] or [add_token] if need be.
      The first argument denotes the sync directory path. *)
  val create_index : string -> unit Lwt.t

  (** Get the list of all tokens. *)
  val get_tokens : string -> Learnocaml_data.Token.t list Lwt.t

  (** Add a registered token in the index. *)
  val add_token : string -> Learnocaml_data.Token.t -> unit Lwt.t
end

module MoodleIndex: sig
  val create_index : string -> unit Lwt.t

  val add_user : string -> string -> Learnocaml_data.Token.t -> unit Lwt.t

  (** Get a Moodle user's token, create it if not exist *)
  val get_user_token : string -> string -> Learnocaml_data.Token.t Lwt.t

  val get_users : string -> (string * Learnocaml_data.Token.t) list Lwt.t
  val user_exists : string -> string -> bool Lwt.t
  val token_exists : string -> Learnocaml_data.Token.t -> bool Lwt.t
end

module OauthIndex: sig
  val create_index : string -> string Lwt.t

  val get_first_oauth : string -> (string * string list) Lwt.t
  val get_current_secret : string -> string Lwt.t

  (** Delete all secrets + nonce associated excepted the current secret with its nonces *)
  val purge : string -> unit Lwt.t
end

val check_oauth : string -> string -> (string * string) list -> (string, string) result Lwt.t

type user =
  | Token of (Learnocaml_data.Token.t * bool)
  | Password of (Learnocaml_data.Token.t * string * string * string option)

type authentication =
  | AuthToken of Learnocaml_data.Token.t
  | Passwd of (string * string)

module UserIndex: sig

  (* If [tokens = None], generate the index from [TokenIndex.get_tokens];
   * If [tokens = Some []], write the index with an empty list of users. *)
  val create_index : ?tokens:(Learnocaml_data.Token.t list) -> string -> user list Lwt.t

  val authenticate : string -> authentication -> Learnocaml_data.Token.t option Lwt.t
  val exists : string -> string -> bool Lwt.t
  val add : string -> user -> unit Lwt.t

  (** Upgrade account from TOKEN to Moodle/LTI *)
  val upgrade_moodle : string -> Learnocaml_data.Token.t -> unit Lwt.t

  (** Upgrade account from TOKEN to password *)
  val upgrade : string -> Learnocaml_data.Token.t -> string -> string -> unit Lwt.t

  (** Update password *)
  val update : string -> Learnocaml_data.Token.t -> string -> unit Lwt.t

  (** Set the field "confirm_email" for the corresponding token to `None` *)
  val confirm_email : string -> Learnocaml_data.Token.t -> unit Lwt.t

  (** Check if the login attempt is valid, based on the provided token:
   *  The boolean `true` is returned only if the token exists in the database,
   *  and is not used by an upgraded account (either moodle or password).
   *)
  val can_login :
    ?use_passwd:bool -> ?use_moodle:bool ->
    string -> Learnocaml_data.Token.t -> bool Lwt.t
  val token_of_email : string -> string -> Learnocaml_data.Token.t option Lwt.t

  (** Four cases for the result:
   * [None]: not found
   * [Some (email, Some email)]: init state, unverified email
   * [Some (email, None)]: verified email
   * [Some (email, Some other_email)]: pending email change
   *)
  val emails_of_token : string -> Learnocaml_data.Token.t  -> ((string * string option) option) Lwt.t
  val change_email : string -> Learnocaml_data.Token.t -> string -> unit Lwt.t

  (** Abort an email change initiated by some user:
   *  The field "email_change" is modified from `Some pending` to `None`,
   *  and an error is raised if the token was already used by a non-upgraded account.
   *)
  val abort_email_change : string -> Learnocaml_data.Token.t -> unit Lwt.t
end

module UpgradeIndex: sig
  (* returns a handle *)
  val change_email : string -> Learnocaml_data.Token.t -> string Lwt.t
  val reset_password : string -> Learnocaml_data.Token.t -> string Lwt.t

  (* return a ChangeEmail handle if it exists *)
  val ongoing_change_email : string -> Learnocaml_data.Token.t -> string option Lwt.t

  (* takes a handle *)
  val can_change_email : string -> string -> Learnocaml_data.Token.t option Lwt.t
  val can_reset_password : string -> string -> Learnocaml_data.Token.t option Lwt.t

  val revoke_operation : string -> string -> unit Lwt.t
  val filter_old_operations : string -> unit Lwt.t
  val abort_email_change : string -> Learnocaml_data.Token.t -> unit Lwt.t
end
