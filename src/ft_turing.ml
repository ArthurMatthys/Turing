open Yojson

type transition = {
  to_state: int; (* index transition *)
  write: int; (* index alphabet *)
  action: bool; (* true: right false: left *)
}

type machine = {
    finals: int list;
    transitions: transition option list list;
}

type transition_string = {
    read: string;
    to_state: string;
    write: string;
    action: string;
}

type transition_string_with_state = {
    state: string;
    transition: transition_string list;
}

type machine_string = {
    name: string;
    alphabet: string list;
    blank: string;
    states: string list;
    initial: string;
    finals: string list;
    transitions: transition_string_with_state list;
}

(* alphabet index 0 is blank *)
type tape = {
    right: int list; (* list index alphabet *)
    left: int list; (* list index alphabet *)
    cur: int; (* index alphabet *)
    state: int; (* index transition *)
}

let run (prg: machine) (rb: tape): tape option = 
    let do_transition (tr: transition) =
        if tr.action then {
            right=if List.length rb.right > 0 then List.tl rb.right else [];
            left=rb.cur :: rb.left;
            cur=if List.length rb.right > 0 then List.hd rb.right else 0;
            state=tr.to_state;
        } else {
            right=rb.cur :: rb.right;
            left=if List.length rb.left > 0 then List.tl rb.left else [];
            cur=if List.length rb.left > 0 then List.hd rb.left else 0;
            state=tr.to_state;
        }
    in
    let transitions: transition option list = Core.List.nth prg.transitions rb.state in
    Option.join (
        Option.map (fun (ltr_opt: transition option list) ->
            Option.map do_transition (List.nth ltr_opt rb.cur)
        ) transitions
    )

(*
let run (prg: machine) (rb: tape): tape option = 
    Option.join (
        Option.map (fun (ltr_opt: transition option list) ->
            Option.map (fun (tr_opt: transition option) ->
                Option.map ( fun (tr: transition) ->
                    if tr.action then {
                        right=if List.length rb.right > 0 then List.tl rb.right else [];
                        left=rb.cur :: rb.left;
                        cur=if List.length rb.right > 0 then List.hd rb.right else 0;
                        state=tr.to_state;
                    } else {
                        right=rb.cur :: rb.right;
                        left=if List.length rb.left > 0 then List.tl rb.left else [];
                        cur=if List.length rb.left > 0 then List.hd rb.left else 0;
                        state=tr.to_state;
                    }
                )
            ) (List.nth ltr_opt rb.cur)
        ) (List.nth prg.transitions rb.state)
    )
 *)

(*
let machine_string_to_machine (ms: machine_string): (machine * tape) = 


;;
*)

let json = Yojson.from_file "Makefile"

