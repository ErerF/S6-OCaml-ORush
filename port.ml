exception End

type orientation=H|V
type boat={i:char;l:int;o:orientation;mutable x:int;mutable y:int}
type state=boat list

let boat_of_string (s:string):boat={i=(String.get s 0);
                                    l=(int_of_string(Char.escaped (String.get s 1)));
                                    o=(match (String.get s 2) with
                                        'H'->H
                                        |_->V);
                                    x=(int_of_string(Char.escaped (String.get s 3)));
                                    y=(int_of_string(Char.escaped (String.get s 4)))}
let string_of_boat (b:boat):string=(Char.escaped b.i)^(string_of_int b.l)^(match b.o with H->"H"|V->"V")^(string_of_int b.x)^(string_of_int b.y)

let boat_exist (c:char) (s:state):int=
  let ids:char list=List.map (fun(x)->x.i) s in
  let rec search_index (c:char) (l:char list) (cpt:int):int=
    match l with
      []->(-1)
    |h::t -> if h=c then cpt else search_index c t cpt+1
  in
  if not (List.mem c ids) then -1
    else search_index c ids 0

let add_boat (bo:boat) (s:state):state=
  let verifier (a:boat) (b:boat):unit=
    if b.o=H then
      if a.o=H&&a.y=b.y then
        if (b.x<=a.x&&a.x<b.x+b.l) || (a.x<=b.x&&b.x<a.x+a.l) then
          raise (Invalid_argument "add_boat")
        else if a.o=V then
          if (b.y<=a.x && a.x<b.y+b.l) && (a.y<=b.y && b.y<a.y+a.l) then
            raise (Invalid_argument "add_boat")
          else if b.o=V then
            if a.o=V && a.x=b.x then
              if (b.y<=a.y&&a.y<b.y+b.l) || (a.y<=b.y&&b.y<a.y+a.l) then
                raise (Invalid_argument "add_boat")
              else if a.o=H then
                if (a.x<=b.x && b.x<a.x+a.l) && (b.y<=a.y && a.y<b.y+b.l) then
                  raise (Invalid_argument "add_boat")
  in
  if (boat_exist bo.i s)!=(-1) then
    raise (Invalid_argument ("add_boat: boat "^(Char.escaped bo.i)^" already exists"));
  List.iter (verifier bo) s;
  s@[bo]

let grid_of_state (s:state):char array array=
  let matrice=Array.make_matrix 6 6 '~'in
  for i=0 to (List.length s)-1  do
    let b=List.nth s i in
    for j=0 to (b.l-1) do
      if b.o=H then
        matrice.(b.y).(b.x+j)<-b.i
      else
        matrice.(b.y+j).(b.x)<-b.i
    done;
  done;
  matrice

let input_state (i:in_channel):state=
  let res=ref [] in
  try
    while true do
      res:=add_boat (boat_of_string (input_line i)) !res
    done;
    !res
  with
    End_of_file->(!res)

let output_state (s:state) (o:out_channel):unit=
  let m=grid_of_state s in
  for i=0 to 5 do
    for j=0 to 5 do
      output_char o m.(i).(j)
    done;
    output_char o '\n'
  done;;
