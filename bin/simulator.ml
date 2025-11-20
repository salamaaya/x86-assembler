(* X86lite Simulator *)

(* See the documentation in the X86lite specification, available on the 
   course web pages, for a detailed explanation of the instruction
   semantics.
*)

open X86

(* simulator machine state -------------------------------------------------- *)

let mem_bot = 0x400000L          (* lowest valid address *)
let mem_top = 0x410000L          (* one past the last byte in memory *)
let mem_size = Int64.to_int (Int64.sub mem_top mem_bot)
let nregs = 17                   (* including Rip *)
let ins_size = 8L                (* assume we have a 8-byte encoding *)
let exit_addr = 0xfdeadL         (* halt when m.regs(%rip) = exit_addr *)

(* Your simulator should raise this exception if it tries to read from or
   store to an address not within the valid address space. *)
exception X86lite_segfault

(* The simulator memory maps addresses to symbolic bytes.  Symbolic
   bytes are either actual data indicated by the Byte constructor or
   'symbolic instructions' that take up eight bytes for the purposes of
   layout.

   The symbolic bytes abstract away from the details of how
   instructions are represented in memory.  Each instruction takes
   exactly eight consecutive bytes, where the first byte InsB0 stores
   the actual instruction, and the next sevent bytes are InsFrag
   elements, which aren't valid data.

   For example, the two-instruction sequence:
        at&t syntax             ocaml syntax
      movq %rdi, (%rsp)       Movq,  [~%Rdi; Ind2 Rsp]
      decq %rdi               Decq,  [~%Rdi]

   is represented by the following elements of the mem array (starting
   at address 0x400000):

       0x400000 :  InsB0 (Movq,  [~%Rdi; Ind2 Rsp])
       0x400001 :  InsFrag
       0x400002 :  InsFrag
       0x400003 :  InsFrag
       0x400004 :  InsFrag
       0x400005 :  InsFrag
       0x400006 :  InsFrag
       0x400007 :  InsFrag
       0x400008 :  InsB0 (Decq,  [~%Rdi])
       0x40000A :  InsFrag
       0x40000B :  InsFrag
       0x40000C :  InsFrag
       0x40000D :  InsFrag
       0x40000E :  InsFrag
       0x40000F :  InsFrag
       0x400010 :  InsFrag
*)
type sbyte = InsB0 of ins       (* 1st byte of an instruction *)
           | InsFrag            (* 2nd - 8th bytes of an instruction *)
           | Byte of char       (* non-instruction byte *)

(* memory maps addresses to symbolic bytes *)
type mem = sbyte array

(* Flags for condition codes *)
type flags = { mutable fo : bool
             ; mutable fs : bool
             ; mutable fz : bool
             }

(* Register files *)
type regs = int64 array

(* Complete machine state *)
type mach = { flags : flags
            ; regs : regs
            ; mem : mem
            }

(* simulator helper functions ----------------------------------------------- *)

(* The index of a register in the regs array *)
let rind : reg -> int = function
  | Rip -> 16
  | Rax -> 0  | Rbx -> 1  | Rcx -> 2  | Rdx -> 3
  | Rsi -> 4  | Rdi -> 5  | Rbp -> 6  | Rsp -> 7
  | R08 -> 8  | R09 -> 9  | R10 -> 10 | R11 -> 11
  | R12 -> 12 | R13 -> 13 | R14 -> 14 | R15 -> 15

(* Helper functions for reading/writing sbytes *)

(* Convert an int64 to its sbyte representation *)
let sbytes_of_int64 (i:int64) : sbyte list =
  let open Char in 
  let open Int64 in
  List.map (fun n -> Byte (shift_right i n |> logand 0xffL |> to_int |> chr))
           [0; 8; 16; 24; 32; 40; 48; 56]

(* Convert an sbyte representation to an int64 *)
let int64_of_sbytes (bs:sbyte list) : int64 =
  let open Char in
  let open Int64 in
  let f b i = match b with
    | Byte c -> logor (shift_left i 8) (c |> code |> of_int)
    | _ -> 0L
  in
  List.fold_right f bs 0L

(* Convert a string to its sbyte representation *)
let sbytes_of_string (s:string) : sbyte list =
  let rec loop acc = function
    | i when i < 0 -> acc
    | i -> loop (Byte s.[i]::acc) (pred i)
  in
  loop [Byte '\x00'] @@ String.length s - 1

(* Serialize an instruction to sbytes *)
let sbytes_of_ins (op, args:ins) : sbyte list =
  let check = function
    | Imm (Lbl _) | Ind1 (Lbl _) | Ind3 (Lbl _, _) -> 
      invalid_arg "sbytes_of_ins: tried to serialize a label!"
    | _ -> ()
  in
  List.iter check args;
  [InsB0 (op, args); InsFrag; InsFrag; InsFrag;
   InsFrag; InsFrag; InsFrag; InsFrag]

(* Serialize a data element to sbytes *)
let sbytes_of_data : data -> sbyte list = function
  | Quad (Lit i) -> sbytes_of_int64 i
  | Asciz s -> sbytes_of_string s
  | Quad (Lbl _) -> invalid_arg "sbytes_of_data: tried to serialize a label!"


(* It might be useful to toggle printing of intermediate states of your 
   simulator. Our implementation uses this mutable flag to turn on/off
   printing.  For instance, you might write something like:

     [if !debug_simulator then print_endline @@ string_of_ins u; ...]

*)
let debug_simulator = ref false

(* Interpret a condition code with respect to the given flags. *)
let interp_cnd {fo; fs; fz} : cnd -> bool = fun x -> 
  match x with
  | Eq -> fz
  | Neq -> not fz
  | Gt -> not (fs <> fo || fz)
  | Ge -> not (fs <> fo)
  | Lt -> fs <> fo
  | Le -> fs <> fo || fz



(* Maps an X86lite address into Some OCaml array index,
   or None if the address is not within the legal address space. *)
let map_addr (addr:quad) : int option =
  if addr >= mem_bot && (Int64.add addr 8L) <= mem_top then
    Some (Int64.to_int (Int64.sub addr mem_bot))
  else None

(* function to set the condition flags after a result has been computed *)
let set_conditions (result:Int64_overflow.t) (m:mach): unit =
    m.flags.fo <- result.overflow; (* handle overflow flag *)
    m.flags.fs <- (Int64.compare result.value 0L) < 0; (* handle sign flag *)
    m.flags.fz <- (Int64.compare result.value 0L) == 0(* handle zero flag *)

let get_addr (addr:quad) (m:mach) : int = 
  let maybe_addr = map_addr addr in 
  match maybe_addr with 
  | Some(a) -> a
  | None -> raise X86lite_segfault

(* gets the data at a given address *)
let get_data (addr:quad) (m:mach) : quad = 
  let a = get_addr addr m in 
  int64_of_sbytes
      [ m.mem.(a + 0); m.mem.(a + 1); m.mem.(a + 2); m.mem.(a + 3); 
        m.mem.(a + 4); m.mem.(a + 5); m.mem.(a + 6); m.mem.(a + 7) ]


(* calculates the indirect address *)
let interp_indirect (op:operand) (m:mach) : quad =
  match op with 
  | Ind1(Lit(disp)) -> disp 
  | Ind2(r) -> m.regs.(rind r)           
  | Ind3(Lit(q), r) -> Int64.add q (m.regs.(rind r))  
  | _ -> invalid_arg "not an indirect"


(* interprets the operands and returns the coresponding value *)
let interp_operand (op:operand) (m:mach) : quad = 
  match op with 
  | Imm(Lit(q)) -> q         
  | Reg(r) -> m.regs.(rind r)           
  | Ind1(_)  
  | Ind2(_)   
  | Ind3(_) ->  
    let addr = interp_indirect op m in 
    get_data addr m
  | _ -> invalid_arg "did not expect label"


(* given a register or address, will store the data and update the state of the machine *)
let put_data (op:operand) (m:mach) (data:quad) : unit = 
  match op with 
  | Reg(r) -> m.regs.(rind r) <- data
  | Ind1(_) 
  | Ind2(_) 
  | Ind3(_) ->
    let idx = interp_indirect op m in
    let a = get_addr idx m in
    let sbytes = sbytes_of_int64 data in
      m.mem.(a + 0) <- List.nth sbytes 0;
      m.mem.(a + 1) <- List.nth sbytes 1;
      m.mem.(a + 2) <- List.nth sbytes 2;
      m.mem.(a + 3) <- List.nth sbytes 3;
      m.mem.(a + 4) <- List.nth sbytes 4;
      m.mem.(a + 5) <- List.nth sbytes 5;
      m.mem.(a + 6) <- List.nth sbytes 6;
      m.mem.(a + 7) <- List.nth sbytes 7
  | _ -> ()

(* pushes a value q onto the stack *)
let push (q:quad) (m:mach) : unit = 
  let sp = m.regs.(rind Rsp) in 
  m.regs.(rind Rsp) <- Int64.sub sp 8L;
  put_data (Ind1(Lit(m.regs.(rind Rsp)))) m q

(* pops the stack and puts the value in q *)
let pop (o:operand) (m:mach) : unit =
  let sp = m.regs.(rind Rsp) in 
  let data = get_data sp m in 
  put_data o m data;
  m.regs.(rind Rsp) <- Int64.add sp 8L

let set_lower_bits (o:operand) (m:mach) (lower:sbyte): unit =
  match o with 
  | Reg(r) -> 
    let sbytes = sbytes_of_int64 m.regs.(rind r) in 
    let new_data = 
      [lower; List.nth sbytes 1; List.nth sbytes 2; List.nth sbytes 3;
        List.nth sbytes 4; List.nth sbytes 5; List.nth sbytes 6; List.nth sbytes 7] in 
    m.regs.(rind r) <- int64_of_sbytes new_data
  | Ind1(_) 
  | Ind2(_) 
  | Ind3(_) -> 
    let ind = interp_indirect o m in
    let a = get_addr ind m in
    m.mem.(a) <- lower
  | _ -> ()

let set_overflow (op:opcode) (src:quad) (dest:quad) (m:mach) : unit = 
  match op with 
  | Shlq -> 
    if (Int64.compare src 1L) == 0 
    then
        let msb = Int64.shift_right_logical dest 62 in 
        m.flags.fo <- (Int64.compare msb 2L) == 0 || (Int64.compare msb 1L) == 0
    else m.flags.fo <- true 
  | Sarq -> m.flags.fo <- not ((Int64.compare src 1L) == 0)
  | Shrq -> 
    if (Int64.compare src 1L) == 0 
    then m.flags.fo <- (Int64.compare dest 0L) < 0 
    else m.flags.fo <- true 
  | _ -> ()


(* carries out all arithmetic instructions that follow that format INS [SRC; DST] *)
let arith (ol:operand list) (op:opcode) (m:mach) : unit = 
  let src = interp_operand (List.nth ol 0) m in 
  let dest = List.nth ol 1 in 
  let dest_val = interp_operand dest m in 
  let v = 
    match op with 
    | Addq -> Int64_overflow.add dest_val src
    | Subq -> Int64_overflow.sub dest_val src
    | Andq -> Int64_overflow.ok (Int64.logand dest_val src) 
    | Orq -> Int64_overflow.ok (Int64.logor dest_val src)
    | Xorq -> Int64_overflow.ok (Int64.logxor dest_val src)
    | Shlq -> Int64_overflow.ok (Int64.shift_left dest_val (Int64.to_int src))
    | Sarq ->  Int64_overflow.ok (Int64.shift_right dest_val (Int64.to_int src))
    | Shrq -> Int64_overflow.ok (Int64.shift_right_logical dest_val (Int64.to_int src))
    | _ -> invalid_arg "wrong operand"
  in begin 
    put_data dest m (v.Int64_overflow.value);
    set_conditions v m;
    set_overflow op src dest_val m;
    m.regs.(rind Rip) <- Int64.add m.regs.(rind Rip) 8L;
  end
  
(* given an instruction, will evaluate it and update the machine's state *)
let interp (instruction:ins) (m:mach) : unit =
  match instruction with 
  | (op, ol) -> 
    begin 
      match op with 
      | Addq 
      | Subq 
      | Andq 
      | Orq 
      | Xorq 
      | Shlq
      | Sarq 
      | Shrq -> arith ol op m
      | Imulq -> 
        let src = interp_operand (List.nth ol 0) m in
        let dest = List.nth ol 1 in
        begin 
        match dest with 
        | Reg r -> 
          let dest_val = interp_operand dest m in 
          let mul = Int64_overflow.mul dest_val src in
          put_data dest m (mul.Int64_overflow.value);
          set_conditions mul m;
          m.regs.(rind Rip) <- Int64.add m.regs.(rind Rip) 8L;
        | _ -> ()
        end 
      | Movq -> 
        let src = interp_operand (List.nth ol 0) m in
        let dest = List.nth ol 1 in
        put_data dest m src;
        m.regs.(rind Rip) <- Int64.add m.regs.(rind Rip) 8L;
      | Pushq ->
        let src = interp_operand (List.nth ol 0) m in
        push src m;
        m.regs.(rind Rip) <- Int64.add m.regs.(rind Rip) 8L;
      | Popq -> 
        let dest = List.nth ol 0 in
        pop dest m;
        m.regs.(rind Rip) <- Int64.add m.regs.(rind Rip) 8L;
      | Leaq -> 
        let ind = interp_indirect (List.nth ol 0) m in
        let dest = List.nth ol 1 in
        put_data dest m ind;
        m.regs.(rind Rip) <- Int64.add m.regs.(rind Rip) 8L;
      | Negq -> 
        let dest = List.nth ol 0 in 
        let dest_val = interp_operand dest m in 
        let neg = Int64_overflow.neg dest_val in 
        put_data dest m neg.value;
        set_conditions neg m;
        m.regs.(rind Rip) <- Int64.add m.regs.(rind Rip) 8L;
      | Cmpq -> 
        let src1 = interp_operand (List.nth ol 0) m in 
        let src2 = interp_operand (List.nth ol 1) m in 
        let sub = Int64_overflow.sub src2 src1 in 
        set_conditions sub m;
        m.regs.(rind Rip) <- Int64.add m.regs.(rind Rip) 8L;
      | Notq ->
        let dest = List.nth ol 0 in 
        let dest_val = interp_operand dest m in 
        let not = Int64.lognot dest_val in 
        put_data dest m not;
        m.regs.(rind Rip) <- Int64.add m.regs.(rind Rip) 8L;
      | Incq ->
        let dest = List.nth ol 0 in 
        let dest_val = interp_operand dest m in 
        let inc = Int64_overflow.succ dest_val in 
        put_data dest m inc.value;
        set_conditions inc m;
        m.regs.(rind Rip) <- Int64.add m.regs.(rind Rip) 8L;
      | Decq ->
        let dest = List.nth ol 0 in 
        let dest_val = interp_operand dest m in 
        let dec = Int64_overflow.pred dest_val in 
        put_data dest m dec.value;
        set_conditions dec m;
        m.regs.(rind Rip) <- Int64.add m.regs.(rind Rip) 8L;
      | Jmp ->
        let src = interp_operand (List.nth ol 0) m in 
        m.regs.(rind Rip) <- src;
      | J(c) -> 
        let src = interp_operand (List.nth ol 0) m in 
        if interp_cnd m.flags c 
        then m.regs.(rind Rip) <- src 
        else m.regs.(rind Rip) <- Int64.add m.regs.(rind Rip) 8L;
      | Set(c) ->
        let dest = List.nth ol 0 in 
        let cond = interp_cnd m.flags c in
        let lower = if cond then Byte(Char.chr 1) else Byte(Char.chr 0) in
        set_lower_bits dest m lower;
        m.regs.(rind Rip) <- Int64.add m.regs.(rind Rip) 8L;
      | Callq ->
        let src = interp_operand (List.nth ol 0) m in
        m.regs.(rind Rip) <- Int64.add m.regs.(rind Rip) 8L;
        push m.regs.(rind Rip) m;
        m.regs.(rind Rip) <- src;
      | Retq -> 
        pop (Reg Rip) m
    end


(* simulates one step of the machine by evaluating an instruction *)
let simulate (instruction:sbyte) (m:mach) : unit =
  match instruction with
  | InsB0(i) -> interp i m
  | InsFrag | Byte _ -> ()
  
(* Simulates one step of the machine:
    - fetch the instruction at %rip
    - compute the source and/or destination information from the operands
    - simulate the instruction semantics
    - update the registers and/or memory appropriately
    - set the condition flags
*)
let step (m:mach) : unit =
  let reg_ins = m.regs.(rind Rip) in 
  let addr = get_addr reg_ins m in
  let instruction = m.mem.(addr) in (* fetch the instruction at %rip *)
  simulate instruction m

(* Runs the machine until the rip register reaches a designated
   memory address. Returns the contents of %rax when the 
   machine halts. *)
let run (m:mach) : int64 = 
  while m.regs.(rind Rip) <> exit_addr do step m done;
  m.regs.(rind Rax)

(* assembling and linking --------------------------------------------------- *)

(* A representation of the executable *)
type exec = { entry    : quad              (* address of the entry point *)
            ; text_pos : quad              (* starting address of the code *)
            ; data_pos : quad              (* starting address of the data *)
            ; text_seg : sbyte list        (* contents of the text segment *)
            ; data_seg : sbyte list        (* contents of the data segment *)
            }

(* Assemble should raise this when a label is used but not defined *)
exception Undefined_sym of lbl

(* Assemble should raise this when a label is defined more than once *)
exception Redefined_sym of lbl

(* gets the size of a single data section from a program *)
let rec get_data_size (d:data list) : quad = 
  match d with 
  | [] -> 0L
  | h :: t -> 
    let curr_block = 
    begin 
      match h with 
      | Asciz a -> (Int64.add (Int64.of_int (String.length a)) (1L)) 
      | Quad q -> 8L 
    end in 
    let rest = (get_data_size t) in 
      Int64.add curr_block rest

(* gets the size of all text sections in a program *)
let rec get_text_size (p:prog) : quad = 
  match p with 
  | [] -> 0L 
  | e :: tl -> 
    begin 
      match e.asm with
      | Text t ->
        let curr_size = (Int64.mul (Int64.of_int (List.length t)) 8L) in 
        Int64.add curr_size (get_text_size tl)
      | Data d -> 0L
    end

(* constructs the symbol table given a program *)
let rec construct_sym_table (sym_table:(lbl, quad) Hashtbl.t) (p:prog) (a:quad) : unit = 
  match p with
  | [] -> ()
  | e :: tl -> 
    let label = e.lbl in 
    begin
      match Hashtbl.find_opt sym_table label with 
      | Some _ -> raise (Redefined_sym e.lbl)
      | None -> let next_addr = 
        begin 
          match e.asm with 
          | Text t -> 
            let curr_size = (Int64.mul (Int64.of_int (List.length t)) 8L) in
            Int64.add curr_size a
          | Data d -> Int64.add (get_data_size d) a
        end in
        Hashtbl.add sym_table label a;
        construct_sym_table sym_table tl next_addr
    end 


(* given a symbol table, will return the address of the table if it exists *)
let rec find_entry (sym_table:(lbl, quad) Hashtbl.t) (label:lbl) : quad = 
  match Hashtbl.find_opt sym_table label with 
  | Some a -> a 
  | None -> raise (Undefined_sym label)
  

(* resolves any labels in a complete instruction *)
let rec resolve_ins (ol:operand list) (sym_table:(lbl, quad) Hashtbl.t) : operand list = 
  match ol with 
  | [] -> [] 
  | o :: t -> 
    let resolved = 
    begin 
      match o with
      | Imm(Lbl l) -> 
        let addr = find_entry sym_table l in 
        Imm(Lit addr)
      | Ind1(Lbl l) -> 
        let addr = find_entry sym_table l in 
        Ind1(Lit addr)
      | Ind3(Lbl l, r) -> 
        let addr = find_entry sym_table l in 
        Ind3(Lit addr, r)
      | _ -> o 
    end in 
    resolved :: resolve_ins t sym_table

(* resolves an entire text section *)
let rec resolve_text (i:ins list) (sym_table:(lbl, quad) Hashtbl.t) : ins list =
  match i with 
  | [] -> []
  | h :: t -> 
    begin 
      match h with 
      | (op, ol) -> 
        let resolved = resolve_ins ol sym_table in 
        (op, resolved) :: resolve_text t sym_table
    end

(* resolves an entire data section *)   
let rec resolve_data (d:data list) (sym_table:(lbl, quad) Hashtbl.t) : data list =
  match d with 
  | [] -> []
  | h :: t -> 
    begin 
      match h with 
      | Quad i -> 
        begin 
          match i with 
          | Lit _ -> h :: resolve_data t sym_table
          | Lbl l -> Quad(Lit (find_entry sym_table l)) :: resolve_data t sym_table
        end 
      | _ -> h :: resolve_data t sym_table
    end

(* resolves an entire program by replacing labels with addresses *)
let rec resolve (p:prog) (sym_table:(lbl, quad) Hashtbl.t) (i:elem list) (d:elem list) : (prog * prog) = 
  match p with 
  | [] -> (i, d)
  | e :: tl -> 
    begin 
      match e.asm with 
      | Text t -> 
        let new_text = Text(resolve_text t sym_table) in 
        let new_elem = {lbl = e.lbl; global = e.global; asm = new_text} in
         resolve tl sym_table (i @ [new_elem]) d
      | Data d' ->
        let new_data = Data(resolve_data d' sym_table) in 
        let new_elem = {lbl = e.lbl; global = e.global; asm = new_data} in
        resolve tl sym_table i (d @ [new_elem])
    end

(* seralizes a program into sbytes  *)
let rec seralize (p:prog) : sbyte list =
  match p with
  | [] -> []
  | e :: tl -> 
    begin 
      match e.asm with 
      | Text t -> 
        List.flatten (List.map sbytes_of_ins t) @ seralize tl
      | Data d -> 
        List.flatten (List.map sbytes_of_data d) @ seralize tl
    end
  
let rec get_text_elems (p:prog) : prog = 
  match p with 
  | [] -> []
  | e :: tl -> 
    begin 
      match e.asm with  
      | Text t -> 
        e :: get_text_elems tl
      | Data d -> 
        get_text_elems tl
    end

let rec get_data_elems (p:prog) : prog = 
  match p with 
  | [] -> []
  | e :: tl -> 
    begin 
      match e.asm with  
      | Text t -> 
        get_data_elems tl
      | Data d -> 
        e :: get_data_elems tl
    end

(* Convert an X86 program into an object file:
   - separate the text and data segments
   - compute the size of each segment
      Note: the size of an Asciz string section is (1 + the string length)
            due to the null terminator

   - resolve the labels to concrete addresses and 'patch' the instructions to 
     replace Lbl values with the corresponding Imm values.

   - the text segment starts at the lowest address
   - the data segment starts after the text segment

  HINT: List.fold_left and List.fold_right are your friends.
 *)
let assemble (p:prog) : exec =
  let p = (get_text_elems p) @ (get_data_elems p) in
  let text_pos = mem_bot in 
  let text_size = get_text_size p in
  let data_pos = Int64.add text_size mem_bot in 
  let sym_table = Hashtbl.create 100 in
  construct_sym_table sym_table p mem_bot;
  let (resolved_text, resolved_data) = resolve p sym_table [] [] in
  let text_seg = seralize resolved_text in 
  let data_seg = seralize resolved_data in 
  let entry = find_entry sym_table "main" in
  {
    entry = entry; 
    text_pos = text_pos; 
    data_pos = data_pos;
    text_seg = text_seg;
    data_seg = data_seg;
  }

(* Convert an object file into an executable machine state. 
    - allocate the mem array
    - set up the memory state by writing the symbolic bytes to the 
      appropriate locations 
    - create the inital register state
      - initialize rip to the entry point address
      - initializes rsp to the last word in memory 
      - the other registers are initialized to 0
    - the condition code flags start as 'false'

  Hint: The Array.make, Array.blit, and Array.of_list library functions 
  may be of use.
*)
let load {entry; text_pos; data_pos; text_seg; data_seg} : mach = 
  let mem = Array.make (mem_size-8) InsFrag in
  let mem_text = Array.of_list text_seg in 
  let mem_data = Array.of_list data_seg in
  Array.blit mem_text 0 mem 0 (Array.length mem_text);
  Array.blit mem_data 0 mem (Array.length mem_text) (Array.length mem_data);
  let exit = Array.of_list (sbytes_of_int64 exit_addr) in
  let mem = Array.append mem exit in
  let regs = Array.make nregs 0L in 
  regs.(rind Rip) <- entry; 
  regs.(rind Rsp) <- Int64.sub mem_top 8L;
  let flags = {fo = false; fs = false; fz = false} in 
  {
    flags = flags; 
    regs = regs; 
    mem = mem;
  }