open Port;;
open Moves;;
open List;;

exception Solution_trouve of string;;
exception Unequal;;
type state_solving={state:state;moves:string}

let all_possible_moves (s:state):move list=
  let res=ref [] in
  let matrice=grid_of_state s in
  let test (b:boat):unit=
    match b.o with
        H->((if (b.x+b.l)<=5&&matrice.(b.y).(b.x+b.l)='~' then (res:=(!res)@[{i=b.i;d=A}]));
      (if (b.x-1)>=0&&matrice.(b.y).(b.x-1)='~' then (res:=(!res)@[{i=b.i;d=R}])))
      |V->((if (b.y+b.l)<=5&&matrice.(b.y+b.l).(b.x)='~' then (res:=(!res)@[{i=b.i;d=A}]));
           (if (b.y-1)>=0&&matrice.(b.y-1).(b.x)='~' then (res:=(!res)@[{i=b.i;d=R}])))
  in
  List.iter (fun(b)->test b) s;
    (!res)

let all_reachable_states (s:state) (moves:move list):state list=
  map (fun(x)->apply_move x s) moves

let compare_state (a:state) (b:state):bool=
  try
    let a_grid=grid_of_state a in
    let b_grid=grid_of_state b in
    for i=0 to 5 do
      for j=0 to 5 do
        if a_grid.(i).(j)<>b_grid.(i).(j) then
          raise Unequal
      done;
    done;
    true
  with
    Unequal->false

let rec check_state_occured (s:state) (all:state list):bool=
  match all with
    []->false
  | h::t->if (compare_state h s) then true else check_state_occured s t

let rec solve q (all:state list):string=
  try
      let s=Queue.take q in
      if win s.state then raise (Solution_trouve s.moves)
      else
        let all1=ref all in
        let movesNext=all_possible_moves s.state in
        let statesNext=all_reachable_states s.state movesNext in

        for i=0 to (List.length statesNext)-1 do
          let s_traite=nth statesNext i in
          if not (check_state_occured s_traite !all1) then
            (all1:=[s_traite]@(!all1);
             Queue.add {state=s_traite;moves=s.moves^(string_of_move (nth movesNext i))} q)
        done;
        solve q !all1
  with
    Queue.Empty->"Probleme insolvable!!"
  | Solution_trouve m->m

let solve_state (s:state):string=
  let q=Queue.create ()in
  Queue.add {state=s;moves=""} q;
  solve q [s]

let solve_input (i:in_channel):string=
  solve_state (input_state i)

(*let ()=
  let j=(open_in "./tests-orush/tests/pos1.txt") in
  let res=solve_input j in
  print_string res;
  print_newline ()*)
