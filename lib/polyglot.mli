(*
 * Copyright (c) 2015 David Sheets <sheets@alum.mit.edu>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

val doctype : string
(**
   @see <http://www.w3.org/TR/html5/syntax.html#the-doctype> The (X)HTML5 DOCTYPE.
*)

(** [Stream] contains functions for turning XML signal streams into
    polyglot (X)HTML5 signal streams. *)
module Stream : sig
  type 'a t = 'a -> ('a * Xmlm.signal) option
  (** ['a t] is the type of XML signal streams springing from pumping
      ['a]. When the stream is dry, [None] is returned. *)

  val output :
    ?nl:bool -> ?indent:int option -> ?ns_prefix:(string -> string option) ->
    Xmlm.dest -> 'a t -> 'a -> unit
  (** [output dest pump source] outputs valid (X)HTML5 polyglot markup
      from a {!t} [pump] and [source]. Only non-void element handling
      is implemented so far.  For more information about the
      parameters, see {!Xmlm.make_output}.

      @see <http://www.w3.org/TR/html-polyglot/> Polyglot Markup
  *)

  val output_doc :
    ?nl:bool -> ?indent:int option -> ?ns_prefix:(string -> string option) ->
    Xmlm.dest -> 'a t -> 'a -> unit
    (** [output_doc dest pump source] outputs a valid (X)HTML5
        polyglot document from a {!t} [pump] and [source]. Only
        non-void element handling and default HTML5 DOCTYPE insertion
        is implemented so far.  For more information about the
        parameters, see {!Xmlm.make_output}.

        @see <http://www.w3.org/TR/html-polyglot/> Polyglot Markup
    *)

end

(** [Tree] contains functions for turning XML trees into polyglot
    (X)HTML5 trees. *)
module Tree : sig
  type t = [ `Data of string | `El of Xmlm.tag * 'a list ] as 'a
  (** [t] is the type of (X)HTML5 trees. *)

  val output :
    ?nl:bool -> ?indent:int option -> ?ns_prefix:(string -> string option) ->
    Xmlm.dest -> t list -> unit
  (** [output dest trees] outputs valid (X)HTML5 polyglot markup
      from a [{!t} list] [trees]. Only non-void element handling is
      implemented so far.  For more information about the parameters,
      see {!Xmlm.make_output}.

      @see <http://www.w3.org/TR/html-polyglot/> Polyglot Markup
  *)

  val output_doc :
    ?dtd:string -> ?nl:bool -> ?indent:int option ->
    ?ns_prefix:(string -> string option) ->
    Xmlm.dest -> t -> unit
    (** [output_doc dest tree] outputs a valid (X)HTML5 polyglot
        document from a {!t} [tree]. Only non-void element handling
        and default HTML5 DOCTYPE is implemented so far.  For more
        information about the parameters, see {!Xmlm.make_output}.

        @see <http://www.w3.org/TR/html-polyglot/> Polyglot Markup
    *)

end
