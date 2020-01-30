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

let print_error (str: string): unit =
    let () = print_string str in
    print_newline ()

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
    let transitions: transition option list option = Core.List.nth prg.transitions rb.state in
        Option.bind transitions (fun (ltr_opt: transition option list) ->
            Option.map do_transition @@ Option.join @@ Core.List.nth ltr_opt rb.cur
        ) 

(*
let machine_string_to_machine (ms: machine_string): (machine * tape) = 


;;
*)

let print_machine (ms: machine_string): (unit, string) result =
    let rec list_str_to_str (str:string list): string = match str with
    | [] -> ""
    | h::[] -> h
    | h::t -> h ^ ", " ^ (list_str_to_str t) 
    in
    let () = Printf.printf "********************************************************************************
*%78s*
*%*s%s%*s*
*%78s*
********************************************************************************\n" "" ((78 - String.length ms.name) / 2) "" ms.name ((78 - String.length ms.name) / 2 + (String.length ms.name) mod 2) "" ""
    in 
    let () = print_string @@ "Alphabet: [ " ^ (list_str_to_str ms.alphabet) ^ " ]\n" in
    let () = print_string @@ "States  : [ " ^ (list_str_to_str ms.states) ^ " ]\n" in
    let () = print_string @@ "Initial : " ^ ms.initial ^ "\n" in
    let () = print_string @@ "Finals  : [ " ^ (list_str_to_str ms.finals) ^ " ]\n" in
    let () = List.iter (fun (tr: transition_string_with_state): unit ->
        List.iter (fun (t:transition_string): unit ->
            print_string @@ "(" ^ tr.state ^ ", " ^ t.read ^ ") -> (" ^ t.to_state ^ ", " ^ t.write ^ ", " ^ t.action ^ ")\n"
        ) tr.transition
    ) ms.transitions
    in
    Result.Ok ()

let json_to_machine_string (j: Yojson.Basic.t): (machine_string, string) result =
    let open Yojson.Basic.Util in
    let (name: (string, string) result) = try Result.Ok (j |> member "name" |> to_string)
                with e -> Result.Error ("Invalid json key \"name\" : " ^ (Printexc.to_string e))
    in
    let alphabet = try Result.Ok (j |> member "alphabet" |> to_list |> filter_string)
                with e -> Result.Error ("Invalid json key \"alphabet\" : " ^ (Printexc.to_string e))
    in
    let blank = try Result.Ok (j |> member "blank" |> to_string) 
                with e -> Result.Error ("Invalid json key \"blank\" : " ^ (Printexc.to_string e))
    in
    let states = try Result.Ok (j |> member "states" |> to_list |> filter_string) 
                with e -> Result.Error ("Invalid json key \"states\" : " ^ (Printexc.to_string e))
    in
    let initial = try Result.Ok (j |> member "initial" |> to_string)
                with e -> Result.Error ("Invalid json key \"initial\" " ^ (Printexc.to_string e))
    in
    let finals = try Result.Ok (j |> member "finals" |> to_list |> filter_string) 
                with e -> Result.Error ("Invalid json key \"finals\" : " ^ (Printexc.to_string e))
    in
    let get_transitions (states: string list) (j: Yojson.Basic.t): transition_string_with_state list =
        List.filter_map (fun (e: string): transition_string_with_state option -> 
            try Some {
                state= e;
                transition= j |> member e |> to_list |> (List.map (fun (j: Yojson.Basic.t) -> {
                        read= j |> member "read" |> to_string;
                        to_state= j |> member "to_state" |> to_string;
                        write= j |> member "write" |> to_string;
                        action= j |> member "action" |> to_string;
                    }
                ))
            }
            with e -> None
        ) states
    in
    let transitions = try Result.Ok (j |> member "transitions" |> (get_transitions @@ Result.get_ok states))
                with e -> Result.Error ("Invalid json key \"transitions\" : " ^ (Printexc.to_string e))
    in
    Result.bind name (fun nm ->
    Result.bind alphabet (fun alpha ->
    Result.bind blank (fun blnk ->
    Result.bind states (fun stats ->
    Result.bind initial (fun init ->
    Result.bind finals (fun fin ->
    Result.bind transitions (fun tr ->
        Result.Ok {
            name= nm;
            alphabet= alpha;
            blank= blnk;
            states= stats;
            initial= init;
            finals= fin;
            transitions= tr;
        }
    )))))))

let read_json_file () =
    try Result.Ok (Yojson.Basic.from_file Sys.argv.(1))
    with e -> Result.Error (Printexc.to_string e)

let () =
    if Array.length Sys.argv <> 3 then
        let () = print_string "./ft_turing <config.jsoin> <input string>\n" in
        let _ = Exit in ()
    else
        let (json: (Yojson.Basic.t, string) result) = read_json_file () in
        let (res: (unit, string) result) =
            Result.bind (Result.bind json json_to_machine_string) print_machine 
        in
        let _ = Result.map_error print_error res in
        ()
