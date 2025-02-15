(* Partial bindings to DOM *)

module Event : sig
  type t
  val t_of_js: Ojs.t -> t
  val t_to_js: t -> Ojs.t


  val client_x: t -> int (* mouse *)
  val client_y: t -> int (* mouse *)

  val page_x: t -> float (* mouse *)
  val page_y: t -> float (* mouse *)

  val screen_x: t -> int (* mouse *)
  val screen_y: t -> int (* mouse *)

  val offset_x: t -> int (* mouse *)
  val offset_y: t -> int (* mouse *)

  val buttons: t -> int  (* mouse *)

  val alt_key: t -> bool (* key *)
  val ctrl_key: t -> bool (* key *)
  val shift_key: t -> bool (* key *)
  val which: t -> int    (* key *)
  val code : t -> string (* key *)
  val key : t -> string (* key *)
end

module Element : sig
  (* Only arguments marked with a "T" may be a textNode. *)
  (* When element arguments are required to be a specific element,
      it is marked with <tag> (where 'tag' is the element's tag name). *)

  type t
  val t_of_js: Ojs.t -> t
  val t_to_js: t -> Ojs.t

  val append_child: t -> t (* T *) -> unit
  val add_event_listener: t (* T *) -> string -> (Event.t -> unit) -> bool -> unit

  val get_elements_by_tag_name: t -> string -> t array

  val has_attribute: t -> string -> bool
  val get_attribute: t -> string -> string
  val set_attribute: t -> string -> string -> unit

  val value: t (* <input> *) -> string

  val selected_index: t (* <select> *) -> int
  val checked: t (* <input> *) -> bool
  val set_checked: t (* <input> *) -> bool -> unit

  val set_text_content: t -> string -> unit

  val set_class_name: t -> string -> unit
  val class_name: t -> string

  val width: t -> int
  val height: t -> int
end

module Document: sig
  type t
  val t_of_js: Ojs.t -> t
  val t_to_js: t -> Ojs.t

  val create_element: t -> string -> Element.t
  val create_text_node: t -> string -> Element.t

  val get_element_by_id: t -> string -> Element.t
  val get_elements_by_class_name: t -> string -> Element.t array

  val body: t -> Element.t

  val set_title: t -> string -> unit
end

module Window: sig
  type t
  val t_of_js: Ojs.t -> t
  val t_to_js: t -> Ojs.t

  val add_event_listener: t -> string -> (Event.t -> unit) -> bool -> unit
end


module Canvas : sig
  module RenderingContext2D : sig
    type t = private Ojs.t

    val set_fill_style: t -> string -> unit
    val fill_rect: t -> int -> int -> int -> int -> unit
    val line_to: t -> int -> int -> unit
    val move_to: t -> int -> int -> unit
    val stroke: t -> unit
    val fillText: t->string->int->int->unit
    val set_font:t->string->unit
    val stroke_rect: t -> int -> int -> int -> int -> unit
    val clear_rect: t -> int -> int -> int -> int -> unit
  end

  type t = private Ojs.t

  val of_element: Element.t -> t
      [@@js.cast]

  val get_context_2d: t -> RenderingContext2D.t
      [@@js.custom
          val get_context: t -> string -> Ojs.t
          [@@js.call]

          let get_context_2d x =
            get_context x "2d"
      ]
end


val window: Window.t
val document: Document.t
