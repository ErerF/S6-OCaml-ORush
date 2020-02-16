open Dom
open Port
open Moves
open Solver

let draw_number c (n:int):unit=
  let topx=70 in
  let leftx=20 in
  let topy=35 in
  let lefty=85 in
  Canvas.RenderingContext2D.fillText c (string_of_int n) (n*50+topx) (topy);
  Canvas.RenderingContext2D.fillText c (string_of_int n) (leftx) (n*50+lefty)


let draw_plateau context :unit =
  Canvas.RenderingContext2D.set_fill_style context "black";
  Canvas.RenderingContext2D.fill_rect context 0 0 400 50;
  Canvas.RenderingContext2D.fill_rect context 0 50 50 350;
  Canvas.RenderingContext2D.fill_rect context 50 350 350 50;
  Canvas.RenderingContext2D.fill_rect context 350 50 50 100;
  Canvas.RenderingContext2D.fill_rect context 350 200 50 150;
  Canvas.RenderingContext2D.set_fill_style context "white";
  Canvas.RenderingContext2D.set_font context "25px serif";
  List.iter (draw_number context) [0;1;2;3;4;5]

let state_of_string (s:string):state=
  let strings=String.split_on_char '\n' s in
  (* List.map boat_of_string strings *)
  try
    List.fold_right (fun (s:string) (st:state)->(if s="" then st else (add_boat (boat_of_string s) st))) strings []
  with
    Invalid_argument e->
    (
      let msgErreur=Document.get_element_by_id document "msgErreur" in
      Element.set_text_content msgErreur ("Erreur:Invalid_argument:"^e);
      []
    )

let draw_boat context (index:int) (b:boat):unit=
  let colors=[|"#ff9900";"#ffff00";"#00ff00";"#00cc99";"#00ffff";"#0000ff";"#6600cc";"#800000";"#ff00ff";"#996600";"#336600";"#ffccff"|] in
  (if b.i='A' then
    Canvas.RenderingContext2D.set_fill_style context "red"
  else
    Canvas.RenderingContext2D.set_fill_style context colors.(index mod Array.length colors));
     (* index:=!index+1 *)
  let topx=67 in
  let leftx=17 in
  let topy=35 in
  let lefty=85 in
  if b.o=H then
  (
    Canvas.RenderingContext2D.fill_rect context ((b.x+1)*50) ((b.y+1)*50) (b.l*50) 50;
    Canvas.RenderingContext2D.set_fill_style context "black";
    Canvas.RenderingContext2D.fillText context (Char.escaped b.i) (b.x*50+topx) ((b.y+1)*50+topy);
  )
  else
  (
    Canvas.RenderingContext2D.fill_rect context ((b.x+1)*50) ((b.y+1)*50) 50 (b.l*50);
    Canvas.RenderingContext2D.set_fill_style context "black";
    Canvas.RenderingContext2D.fillText context (Char.escaped b.i) ((b.x+1)*50+leftx) (b.y*50+lefty);
  )

let affiche_rsltMsg (cpt:int) (w:bool):unit=
  let rsltMsg=Document.get_element_by_id document "rsltMsg" in
  (* Element.set_class_name rsltMsg "win"; *)
  if w then
    (
      Element.set_class_name rsltMsg "win";
      Element.set_text_content rsltMsg ("Félicitation! Vous avez gagné en "^(string_of_int cpt)^" coup(s)!")
    )
  else
    (
      Element.set_class_name rsltMsg "";
      Element.set_text_content rsltMsg ((string_of_int cpt)^" coup(s)");
    )



let draw_bateaux (cpt:int ref) status:unit=
  let txtBateaux=Document.get_element_by_id document "txtBateaux" in
  let element=Document.get_element_by_id document "plateau" in
  let canvas=Canvas.of_element element in
  let context=Canvas.get_context_2d canvas in
  Canvas.RenderingContext2D.clear_rect context 50 50 300 300;
  let boats_str=Element.value txtBateaux in
  let index=ref 0 in
  status:=state_of_string boats_str;
  List.iter (fun b->(draw_boat context !index b;index:=!index+1)) !status;
  cpt:=0;
  affiche_rsltMsg !cpt (win !status)



(* let test (s:state):unit=
  let rec aux s msgErreur=
    match s with
      []->()
    |h::t->Element.set_text_content msgErreur (string_of_boat h);aux t msgErreur
  in
  let msgErreur=Document.get_element_by_id document "msgErreur" in
  aux s msgErreur *)


let search_bateau (s:state) (x:int) (y:int):int=
  let rec search (s:state) (x:int) (y:int) (index:int):int=
    if index>=(List.length s) then
      -1
    else
      (
        let b=List.nth s index in
        match b.o with
          H->(if (x>=((b.x+1)*50)&&x<((b.x+1+b.l)*50)&&y>=((b.y+1)*50)&&y<((b.y+2)*50)) then index
              else search s x y (index+1))
        |V->(if (x>=((b.x+1)*50)&&x<((b.x+2)*50)&&y>=((b.y+1)*50)&&y<((b.y+1+b.l)*50)) then index
             else search s x y (index+1))
      )
  in
  if (x<50||x>=350||y<50||y>=350) then
    -1
  else
    search s x y 0

let clear_boat context (b:boat):unit=
  match b.o with
    H->(Canvas.RenderingContext2D.clear_rect context ((b.x+1)*50) ((b.y+1)*50) (b.l*50) 50;)
  |V->(Canvas.RenderingContext2D.clear_rect context ((b.x+1)*50) ((b.y+1)*50) 50 (b.l*50);)

let clear_boat_border context (b:boat) (index:int):unit=
  clear_boat context b;
  draw_boat context index b

let draw_boat_border context (b:boat):unit=
  match b.o with
    H->(Canvas.RenderingContext2D.stroke_rect context ((b.x+1)*50+1) ((b.y+1)*50+1) (b.l*50-2) 48;
        (* Canvas.RenderingContext2D.clear_rect context ((b.x+1)*50) ((b.y+1)*50) (b.l*50) 50;
           draw_boat context (ref index) b *)
       )
  |V->(Canvas.RenderingContext2D.stroke_rect context ((b.x+1)*50+1) ((b.y+1)*50+1) 48 (b.l*50-2);
       (* Canvas.RenderingContext2D.clear_rect context ((b.x+1)*50) ((b.y+1)*50) 50 (b.l*50);
          draw_boat context (ref index) b *)
      )

let click_plateau (boat_choisi:int ref) context (status:state ref) e:unit=
  if !boat_choisi!=(-1) then
  (
    let bc=List.nth !status (!boat_choisi) in
    clear_boat_border context bc !boat_choisi
  );
  let index=search_bateau !status (Event.offset_x e) (Event.offset_y e) in
  if index=(-1) then ()
  else
  (
    boat_choisi:=index;
    draw_boat_border context (List.nth !status index)
  )


  let draw_addForm (cpt:int ref) status:unit=
    let add_bateau=Document.get_element_by_id document "add_bateau" in
    let txtBateaux=Document.create_element document "textarea" in
    let btnAdd=Document.create_element document "button" in
    let msgErreur=Document.create_element document "p" in
    Element.set_attribute msgErreur "id" "msgErreur";
    Element.set_attribute txtBateaux "id" "txtBateaux";
    Element.set_attribute txtBateaux "rows" "15";
    Element.set_attribute txtBateaux "cols" "30";
    Element.set_attribute btnAdd "id" "btnAdd";
    Element.set_text_content btnAdd "add";
    Element.add_event_listener btnAdd "click" (fun c->
        let msgErreur=Document.get_element_by_id document "msgErreur" in
        Element.set_text_content msgErreur "";
        draw_bateaux cpt status) false;
    Element.append_child add_bateau txtBateaux;
    Element.append_child add_bateau btnAdd;
    Element.append_child add_bateau msgErreur


let click_movebuttons (dir:string) context (boat_choisi:int ref) (cpt:int ref) (status:state ref) e:unit=
  try
    if !boat_choisi=(-1) then raise Cannot_move;
    if win (!status) then raise Cannot_move;

    let b:boat=List.nth !status !boat_choisi in
    let move_str:string=(Char.escaped b.i)^dir in
    let m:move=move_of_string move_str in
    status:=(apply_move m !status);
    clear_boat context b;
    let bNew:boat=List.nth !status !boat_choisi in
    draw_boat context !boat_choisi bNew;
    draw_boat_border context bNew;
    cpt:=!cpt+1;
    affiche_rsltMsg !cpt (win (!status))
  with
    Cannot_move->()

let draw_movebuttons context (boat_choisi:int ref) (cpt:int ref) status:unit=
  let buttons=Document.get_element_by_id document "buttons" in
  let btnReculer=Document.create_element document "button" in
  let btnAvancer=Document.create_element document "button" in
  Element.set_attribute btnReculer "id" "btnReculer";
  Element.set_attribute btnAvancer "id" "btnAvancer";
  Element.set_text_content btnReculer "<";
  Element.set_text_content btnAvancer ">";
  Element.append_child buttons btnReculer;
  Element.append_child buttons btnAvancer;
  Element.add_event_listener btnReculer "click" (click_movebuttons "<" context boat_choisi cpt status) false;
  Element.add_event_listener btnAvancer "click" (click_movebuttons ">" context boat_choisi cpt status) false

let draw_rsltMsg ():unit=
  let resultat=Document.get_element_by_id document "resultat" in
  let rsltMsg=Document.create_element document "p" in
  Element.set_attribute rsltMsg "id" "rsltMsg";

  Element.set_text_content rsltMsg "0 coup(s)";
  Element.append_child resultat rsltMsg

let charger_page ():unit=
  let element=Document.get_element_by_id document "plateau" in
  Element.set_attribute element "width" "400px" ;
  Element.set_attribute element "height" "400px";
  let canvas=Canvas.of_element element in
  let context=Canvas.get_context_2d canvas in
  let status=ref [] in
  let boat_choisi=ref (-1) in
  let cpt=ref 0 in
  draw_plateau context;
  Element.add_event_listener element "click" (click_plateau boat_choisi context status) false;
  draw_movebuttons context boat_choisi cpt status;
  draw_rsltMsg ();
  draw_addForm cpt status


let _ =  charger_page ()
