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

val xmlns : string
(**
   @see <http://www.w3.org/TR/html-polyglot/#element-level-namespaces>
   Polyglot Markup Element-level namespaces advice.
*)

val is_html_doctype : string -> bool
(** [is_html_doctype doctype] is true when [doctype] is a complete HTML
    DOCTYPE declaration, e.g. [<!DOCTYPE html SYSTEM "about:legacy-compat">]. *)

(** [Stream] contains functions for turning XML signal streams into
    polyglot (X)HTML5 signal streams. *)
module Stream : sig
  type 'a pump = 'a -> ('a * Xmlm.signal) option
  (** ['a pump] is the type of XML signal streams springing from pumping
      ['a]. When the stream is dry, [None] is returned. *)

  type t
  (** [t] is the type of polyglot stream contexts over {!Xmlm.output}. *)

  val of_xml_output : ?dtd:string -> Xmlm.output -> t
  (** [of_xml_output ?dtd xml_out] creates a fresh polyglot stream
      context writing to [xml_out]. No default document type is
      prepended to the output so if one is desired, [dtd] should be
      passed with {!doctype}. *)

  val smart_doc_of_xml_output : ?dtd:string -> Xmlm.output -> t
  (** [smart_doc_of_xml_output ?dtd xml_out] creates a fresh polyglot
      stream context writing to [xml_out]. The context will detect if
      the output is a complete HTML document and, if so, stream out
      Polyglot Markup. If the document is not a complete HTML
      document, as determined by DOCTYPE and root element, normal XML
      output will be used. No default document type is prepended to
      the output so if one is desired, [dtd] should be passed with
      {!doctype}. *)

  val push_signals : t -> Xmlm.signal list -> t
  (** [push_signals stream signals] will convert [signals] to polyglot
      according to the context carried by [stream] and output them to
      [stream]'s sink. *)

  val output :
    ?nl:bool -> ?indent:int option -> ?ns_prefix:(string -> string option) ->
    Xmlm.dest -> 'a pump -> 'a -> unit
  (** [output dest pump source] outputs valid (X)HTML5 polyglot markup
      from a {!t} [pump] and [source]. Only non-void element handling
      is implemented so far.  For more information about the
      parameters, see {!Xmlm.make_output}.

      @see <http://www.w3.org/TR/html-polyglot/> Polyglot Markup
  *)

  val output_doc :
    ?nl:bool -> ?indent:int option -> ?ns_prefix:(string -> string option) ->
    Xmlm.dest -> 'a pump -> 'a -> unit
    (** [output_doc dest pump source] outputs a valid (X)HTML5
        polyglot document from a {!t} [pump] and [source]. Only
        non-void element handling and default HTML5 DOCTYPE insertion
        are implemented so far.  For more information about the
        parameters, see {!Xmlm.make_output}.

        @see <http://www.w3.org/TR/html-polyglot/> Polyglot Markup
    *)

  val smart_output_doc :
    ?nl:bool -> ?indent:int option -> ?ns_prefix:(string -> string option) ->
    Xmlm.dest -> 'a pump -> 'a -> unit
    (** [smart_output_doc dest pump source] detects whether the stream
        of signals from [pump] is a complete HTML document, as
        determined by DOCTYPE and root element, and if so outputs a
        valid (X)HTML5 polyglot document like {!output_doc}. If the
        input is not a complete HTML document, [smart_output_doc]
        simply outputs the document as XML.

        @see <http://www.w3.org/TR/html-polyglot/> Polyglot Markup *)

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
        and default HTML5 DOCTYPE are implemented so far.  For more
        information about the parameters, see {!Xmlm.make_output}.

        @see <http://www.w3.org/TR/html-polyglot/> Polyglot Markup
    *)

  val smart_output_doc :
    ?dtd:string -> ?nl:bool -> ?indent:int option ->
    ?ns_prefix:(string -> string option) ->
    Xmlm.dest -> t -> unit
    (** [smart_output_doc dest tree] detects whether [tree] is a
        complete HTML document, as determined by DOCTYPE and root
        element, and if so outputs a valid (X)HTML5 polyglot document
        like {!output_doc}. If the input is not a complete HTML
        document, [smart_output_doc] simply outputs the document as
        XML.

        @see <http://www.w3.org/TR/html-polyglot/> Polyglot Markup *)

end
