open Yojson
open Core

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
    transition: transition_string;
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
    let tr_opt: transition option = List.nth (List.nth prg.transitions rb.state) rb.cur in
    Option.map (fun (tr: transition) ->
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
        }) tr_opt

let machine_string_to_machine (ms: machine_string): machine = 


;;

let () = print_string "Hello world!"
let () = print_newline ()
