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
type machine = {
    finals: int list;
    transitions: transition option list list;
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
 *)

let machine_string_to_machine (ms:machine_string) (instr:string list): (tape * machine) = 
  let tp = {right=List.tl instr;
            left=[];
            cur=0;
            state=0;
    }    

let list_result_flip (lr: ('a, 'b) result list) : ('a list, 'b) result =
    let ler = List.filter Result.is_error lr in
    if List.length ler > 0
    then Result.Error (Result.get_error @@ List.hd ler)
    else Result.Ok (List.map Result.get_ok lr)

let print_machine (ms: machine_string): unit =
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
    in ()

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
    let json_to_transition_string (jtr: Yojson.Basic.t): (transition_string, string) result = 
        let rd = try Result.Ok (jtr |> member "read" |> to_string)
                with er -> Result.Error ("Invalid json key \"read\" : " ^ (Printexc.to_string er))
        in
        let tstate = try Result.Ok (jtr |> member "to_state" |> to_string)
                with er -> Result.Error ("Invalid json key \"to_state\" : " ^ (Printexc.to_string er))
        in
        let wr = try Result.Ok (jtr |> member "write" |> to_string)
                with er -> Result.Error ("Invalid json key \"write\" : " ^ (Printexc.to_string er))
        in
        let act = try Result.Ok (jtr |> member "action" |> to_string)
                with er -> Result.Error ("Invalid json key \"action\" : " ^ (Printexc.to_string er))
        in
        Result.bind rd (fun r ->
        Result.bind tstate (fun ts ->
        Result.bind wr (fun w ->
        Result.bind act (fun a: (transition_string, string) result -> Result.Ok {
            read= r;
            to_state= ts;
            write= w;
            action= a;
        }
        ))))
    in
    let get_transitions (j: Yojson.Basic.t) (states: string list): (transition_string_with_state list, string) result =
        list_result_flip @@ List.filter_map (fun (e: string): (transition_string_with_state, string) result option -> 
            let (trstr: Yojson.Basic.t list option) = try Some (j |> member e |> to_list)
                        with e -> None
            in
            Option.map (fun (jtrl: Yojson.Basic.t list) : (transition_string_with_state, string) result ->
                let (transition: (transition_string, string) result list) = List.map json_to_transition_string jtrl in
                let tr_error = List.filter Result.is_error transition in
                if List.length tr_error > 0
                then
                    Result.Error (Result.get_error @@ Result.map_error (fun msg -> msg ^ " in transition " ^ e) @@ List.hd tr_error)
                else
                    Result.Ok {
                        state=e;
                        transition=List.map Result.get_ok transition;
                    }
            ) trstr
        ) states
    in
    let transitions = try Result.bind states @@ get_transitions @@ member "transitions" j
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

let read_json_file (arg_json:string) =
    try Result.Ok (Yojson.Basic.from_file arg_json)
    with e -> Result.Error (Printexc.to_string e)

let has_duplicates (lst:'a list): bool =
    List.exists (fun e -> 1 < (List.length @@ List.filter ((=) e) lst)) lst

let check_transitions (tr:transition_string_with_state list) (seek: transition_string -> (unit, string) result): (unit, string) result = 
    let tr_err = List.flatten @@ List.map (fun (tr_str:transition_string_with_state) -> List.map (fun e -> Result.map_error ((^) @@ tr_str.state ^ ": ") (seek e) ) tr_str.transition) tr in
    Result.map (fun _ -> ()) @@ list_result_flip tr_err 

let check_machine (ms: machine_string): (machine_string, string) result = (
    if not @@ List.exists ((=) ms.initial) ms.states
    then Result.Error (ms.initial ^ " (inital value) is not in states list")
    else if not @@ List.for_all (fun (f:string): bool -> List.exists ((=) f) ms.states) ms.finals
    then Result.Error ("An element of finals is not in states list")
    else if not @@ List.exists ((=) ms.blank) ms.alphabet
    then Result.Error (ms.blank ^ " (blank character) is not in the alphabet")
    else
        let tr_res =  check_transitions ms.transitions  (fun (tr_str:transition_string): (unit, string) result ->
            if not @@ List.exists ((=) tr_str.read) ms.alphabet 
            then Result.Error (tr_str.read ^ " (read value) is not in the alphabet")
            else if not @@ List.exists ((=) tr_str.write) ms.alphabet 
            then Result.Error (tr_str.write ^ " (write value) is not in the alphabet")
            else if not @@ List.exists ((=) tr_str.to_state) ms.states 
            then Result.Error (tr_str.to_state ^ " (to_state value) is not in the state list")
            else if not @@ List.exists ((=) tr_str.action) @@ "RIGHT" :: "LEFT" :: []
            then Result.Error (tr_str.action ^ " (action) is supposed to be either \"RIGHT\" or \"LEFT\"")
            else Result.Ok ()
            )
        in
        Result.bind tr_res (fun _ ->
            if List.exists (fun e -> String.length e <> 1) ms.alphabet then Result.Error "member of alphabet are longer than a single character"
            else if has_duplicates ms.alphabet then Result.Error "Duplicates in alphabet"
            else if has_duplicates ms.states then Result.Error "Duplicates in states" 
            else Result.Ok ms
          ) 
)

let explode s :(string list)=
    let rec exp i l =
        if i < 0 then l else exp (i - 1) (Char.escaped s.[i] :: l) in
    exp (String.length s - 1) []

let check_intructions (ms: machine_string) (arg_instruction:string list): (machine_string, string) result =
    if List.for_all (fun c -> List.exists ((=) c) ms.alphabet) @@ explode arg_instruction then Result.Ok ms
    else Result.Error "Wrong character in instruction"

let () =
    if Array.length Sys.argv <> 3 then
        let () = print_string "./ft_turing <config.jsoin> <input string>\n" in
        let _ = Exit in ()
    else
        let (json: (Yojson.Basic.t, string) result) = read_json_file Sys.argv.(1) in
        let lst_instr = explode Sys.argv.(2) in
        let _ = Result.map_error print_error @@
             Result.map (fun (ms:machine_string) ->
              let machine = machine_string_to_machine ms lst_instr in
              ()
            ) @@ Result.bind (Result.bind (Result.bind json json_to_machine_string) check_machine) lst_instr
        in ()
