open Yojson

type transition = {
  to_state: int; (* index transition *)
  write: int; (* index alphabet *)
  action: bool; (* true: right false: left *)
}

type machine = {
    finals: int list;
    transitions: transition option list  option list;
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

let indexed (l: 'a list): ('a * int) list =
  let rec indexed_rec (l: 'a list) (i: int): ('a * int) list =
    match l with
      [] -> []
      | h :: t -> (h, i) :: (indexed_rec t (i + 1))
  in
  indexed_rec l 0

let cpe (f1: 'a -> ('b, 'e) result) (f2: 'b -> ('c, 'e) result): 'a -> ('c, 'e) result =
    (fun a -> Result.bind (f1 a) f2 )

let list_result_flip (lr: ('a, 'b) result list) : ('a list, 'b) result =
    let ler = List.filter Result.is_error lr in
    if List.length ler > 0
    then Result.Error (Result.get_error @@ List.hd ler)
    else Result.Ok (List.map Result.get_ok lr)

let index_of (l: 'a list) (e: 'a): int option=
    let rec search l i = match l with
    | [] -> None
    | h::t -> if h = e then Some i else search t (i+1)
    in
    search l 0

let explode s : string list=
    let rec exp i l =
        if i < 0 then l else exp (i - 1) (Char.escaped s.[i] :: l) in
    exp (String.length s - 1) []

let string_rev str =
   let rec aux  idx = match idx with
     0 -> Char.escaped (str.[0])
   | _ -> (Char.escaped str.[idx]) ^ (aux (idx-1)) in
  aux ((String.length str)-1)

(* --- *)

let print_rb (rb: tape) (ms: machine_string): unit = 
    let rec list_to_string (lst: int list) (width: int) : string =
    if width == 0
    then "" 
    else match lst with
        | [] -> ms.blank ^ (list_to_string lst (width-1))
        | h::t -> (List.nth ms.alphabet h) ^ (list_to_string t (width-1))
    in
    print_string @@ "[" ^ (string_rev (list_to_string rb.left 10)) ^
     "<" ^ (List.nth ms.alphabet rb.cur) ^ ">" ^
      (list_to_string rb.right 10) ^ "] (" ^
       (List.nth ms.states rb.state) ^ ", " ^
        (List.nth ms.alphabet rb.cur) ^ ") -> "

let print_tr (tr: transition) (ms: machine_string): unit =
    print_string @@ "(" ^ (List.nth ms.states tr.to_state) ^ ", " ^
     (List.nth ms.alphabet tr.write) ^ ", " ^
      (if tr.action then "RIGHT)\n" else "LEFT)\n") 

let print_error = print_endline

let do_transition (rb: tape) (tr: transition): tape =
    if tr.action then {
        right=if List.length rb.right > 0 then List.tl rb.right else [];
        left=tr.write :: rb.left;
        cur=if List.length rb.right > 0 then List.hd rb.right else 0;
        state=tr.to_state;
    } else {
        right=tr.write :: rb.right;
        left=if List.length rb.left > 0 then List.tl rb.left else [];
        cur=if List.length rb.left > 0 then List.hd rb.left else 0;
        state=tr.to_state;
    }

let print_machine (prg:machine) (ms:machine_string): unit = 
    let _ = List.iter (fun (trllst, i) -> 
    if Option.is_none trllst
    then print_endline "None"
    else 
        let _ = List.iter (fun (tr_opt:transition option) ->
            if Option.is_none tr_opt
            then let () = print_endline "\tNone" in ()
            else
                let tr = Option.get tr_opt in
                let () = print_string @@ "\tto_state: " ^ string_of_int tr.to_state in
                let () = print_string @@ " // write: " ^ string_of_int tr.write in
                let () = print_endline @@ " // action: " ^ string_of_bool tr.action in ()
        ) (Option.get trllst) 
        in ()
    ) @@ indexed prg.transitions in ()

let print_tran (tr:transition): unit =
    print_string @@ (string_of_int tr.to_state) ^ (string_of_int tr.write) ^ (string_of_bool tr.action)

let get_next_tr (prg: machine) (rb: tape): (transition, string) result = 
    let transitions: transition option list option = List.nth prg.transitions rb.state in
    if Option.is_none transitions then Result.error "The transition named in the previous instruction wasn't found in the transtion table"
    else let trs = Option.get transitions in
         let tr = List.nth trs rb.cur in 
         if Option.is_none tr then Result.error "In this state, the character read has no use"
         else Result.ok @@ Option.get tr

let run (prg: machine) (rb: tape) (ms: machine_string): (unit, string) result =
    let (rbr: tape ref) = ref rb in
    let (er: (unit, string) result ref) = ref @@ Result.Ok () in
    while Result.is_ok !er && not @@ List.exists ((=) !rbr.state) prg.finals do
        let tr = get_next_tr prg !rbr in
        if Result.is_error tr
        then
            er := Result.error @@ Result.get_error tr
        else
            let () = print_rb !rbr ms in
            let () = print_tr (Result.get_ok tr) ms in
            rbr := do_transition !rbr @@ Result.get_ok tr;
            ()
    done;
    !er

let order_machine_string (ms: machine_string): machine_string = {
        name=ms.name;
        alphabet= ms.blank :: (List.filter (fun e -> not @@ String.equal e ms.blank) ms.alphabet);
        blank= ms.blank;
        states= ms.initial :: (List.filter (fun e -> not @@ String.equal e ms.initial) ms.states);
        initial= ms.initial;
        finals= ms.finals;
        transitions= ms.transitions;
    }

let machine_string_to_machine (ms_order:machine_string) (instr:string list): (tape * machine) = 
    let transition_of_state (s: string): transition option list option =
        let (tr:  transition_string_with_state option) =
            List.find_opt (fun (trsws: transition_string_with_state) -> String.equal trsws.state s) ms_order.transitions
        in
        let sort_tr (tr: transition_string_with_state): transition option list = 
            let find_tr (a: string): transition option =
                let (trs_opt: transition_string option) =
                    List.find_opt (fun (trs:transition_string) -> String.equal trs.read a) tr.transition
                in
                let trs_to_tr (trs: transition_string): transition = {
                    to_state= Option.get @@ index_of ms_order.states trs.to_state;
                    write= Option.get @@ index_of ms_order.alphabet trs.write;
                    action= trs.action = "RIGHT"
                } in
                Option.map trs_to_tr trs_opt
            in
            List.map find_tr ms_order.alphabet
        in
        Option.map sort_tr tr
    in
    let (ma: machine) = {
        finals= List.filter_map (fun s -> index_of ms_order.states s) ms_order.finals;
        transitions=List.map transition_of_state ms_order.states;
    }
    in 
    let (lst_instr_index: int list) = List.filter_map (index_of ms_order.alphabet) instr in
    let (tp: tape) = {
        right= if List.length lst_instr_index > 1 then List.tl lst_instr_index else [];
        left= [];
        cur= if List.length lst_instr_index > 1 then List.hd lst_instr_index else 0;
        state= 0;
    } in
    (tp, ma)

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
    in 
    let () = Printf.printf "********************************************************************************\n"
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

let check_intructions (ms: machine_string) (arg_instruction:string list): (unit, string) result =
    if List.for_all (fun c -> List.exists (String.equal c) ms.alphabet) arg_instruction
    then Result.Ok ()
    else Result.Error "Wrong character in instruction"

let usage (): unit = print_string
"usage: ft_turing [-h] jsonfile input\n
positional arguments:
\tjsonfile\tjson description of the machine\n
\tinput\t\tinput of the machine\n
optional arguments:
\t-h, --help show this help message and exit\n"

let _ =
    if (Array.length Sys.argv <> 3) || (Array.exists (fun s -> (String.equal s "-h") || (String.equal s "--help")) Sys.argv)
    then usage ()
    else
        let (json: (Yojson.Basic.t, string) result) = read_json_file Sys.argv.(1) in
        let lst_instr = explode Sys.argv.(2) in
        let (machine_string_res: (machine_string, string) result) = 
            Result.bind (Result.bind json json_to_machine_string) check_machine
        in
        if Result.is_error machine_string_res
        then print_error @@ Result.get_error machine_string_res
        else 
            let ms = Result.get_ok machine_string_res in
            let error_lst_instr = check_intructions ms lst_instr in
            if Result.is_error error_lst_instr
            then print_error @@ Result.get_error error_lst_instr
            else
                let () = print_machine ms in
                let ms_order = order_machine_string ms in
                let (rb, mach) = machine_string_to_machine ms_order lst_instr in
                let er = run mach rb ms_order in
                let _ = Result.map_error print_error er in
                    ()
