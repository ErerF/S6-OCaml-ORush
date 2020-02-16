open Port;;
open List;;
(*A:avancer R:reculer*)
type direction=A|R
(*i:identifiant, d:direction*)
type move={i:char;d:direction}
exception Cannot_move
let string_of_move (m:move):string=
  match m.d with
  A->(Char.escaped m.i)^">"
  |_->(Char.escaped m.i)^"<"
let move_of_string (s:string):move={i=(String.get s 0);d=(if((String.get s 1)='>')then A else R)}

let copy_boat (b:boat):boat=
  {i=b.i;l=b.l;o=b.o;x=b.x;y=b.y}

let copy_state (s:state):state=
  let res=ref [] in
  List.iter (fun(b)->res:=(!res)@[copy_boat b]) s;
  (!res)

let apply_move (m:move) (p:state):state=
  let s=copy_state p in
  let index=(boat_exist m.i s) in
  if index=(-1) then
    raise Cannot_move
  else
  let matrice=grid_of_state s in
  let b=List.nth s index in
  (match m.d,b.o with
     A,H->(if (b.x+b.l)>5||matrice.(b.y).(b.x+b.l)!='~' then raise Cannot_move
          else matrice.(b.y).(b.x)<-'~';matrice.(b.y).(b.x+b.l)<-b.i;b.x<-b.x+1;)
    |A,V->(if (b.y+b.l)>5||matrice.(b.y+b.l).(b.x)!='~' then raise Cannot_move
              else matrice.(b.y).(b.x)<-'~';matrice.(b.y+b.l).(b.x)<-b.i;b.y<-b.y+1;)
    |R,H->(if (b.x-1)<0||matrice.(b.y).(b.x-1)!='~' then raise Cannot_move
        else matrice.(b.y).(b.x+b.l-1)<-'~';matrice.(b.y).(b.x-1)<-b.i;b.x<-b.x-1;)
    |R,V->(if (b.y-1)<0||matrice.(b.y-1).(b.x)!='~' then raise Cannot_move
           else matrice.(b.y+b.l-1).(b.x)<-'~';matrice.(b.y-1).(b.x)<-b.i;b.y<-b.y-1;));
  s

let win (s:state):bool=
  if (List.length s)=0 then false
  else
  (
    let b=(nth s (boat_exist 'A' s)) in
    if b.x=4 && b.y=2 then true
    else false
  )


let check_solution (s:state) (str:string):bool=
  let rec apply (s:state) (str:string):bool=
    if win s then true
    else
      (match str with
         ""->false
       | _->(let sNew=apply_move (move_of_string (String.sub str 0 2)) s in apply sNew (String.sub str 0 ((String.length str)-2))))
  in
  apply s str
