open Util.Assert
open X86
open Simulator
open Gradedtests
open Asm

(* These tests are provided by you -- they will be graded manually *)

(* You should also add additional test cases here to help you   *)
(* debug your program.                                          *)

let mov_ri =
 test_machine
 [
 InsB0 (Movq, Asm.[ ~$42; ~%Rax ]);
 InsFrag;
 InsFrag;
 InsFrag;
 InsFrag;
 InsFrag;
 InsFrag;
 InsFrag;
 ]

let incr_loop =
[
  text "main"
    [
      Movq, Asm.[ ~$10; ~%Rcx ];
      Cmpq, Asm.[ ~$0; ~%Rcx ];
      J Eq, Asm.[ ~$(Int64.to_int exit_addr) ];
      Addq, Asm.[ ~%Rcx; ~%Rax ];
      Decq, Asm.[ ~%Rcx ];
      Jmp, Asm.[ ~$(Int64.to_int mem_bot + 8) ];
    ]
]


let repeated_label =
[
  text "main"
    [
      Movq, Asm.[ ~$10; ~%Rcx ];
      Cmpq, Asm.[ ~$0; ~%Rcx ];
      J Eq, Asm.[ ~$(Int64.to_int exit_addr) ];
      Addq, Asm.[ ~%Rcx; ~%Rax ];
      Decq, Asm.[ ~%Rcx ];
      Jmp, Asm.[ ~$(Int64.to_int mem_bot + 8) ];
    ];
    text "main" 
    [
      Movq, Asm.[ ~$10; ~%Rcx ];
      Cmpq, Asm.[ ~$0; ~%Rcx ];
      J Eq, Asm.[ ~$(Int64.to_int exit_addr) ];
      Addq, Asm.[ ~%Rcx; ~%Rax ];
      Decq, Asm.[ ~%Rcx ];
      Jmp, Asm.[ ~$(Int64.to_int mem_bot + 8) ];
    ]
]


let call_ret =
[
  text "main" 
    [
      Callq, [~$(Int64.to_int mem_bot + 8 * 2)];
      Jmp, [ ~$(Int64.to_int exit_addr) ];
      Movq,  [ ~$69; ~%Rax ];
      Retq, [];
      Movq, [ ~$42; ~%Rax ];
    ]
]

let top_of_mem =
[
  text "main" 
    [
      Movq, [Imm(Lit 10L); Ind1(Lit (Int64.sub mem_top 16L))];
      Movq, [Ind1(Lit (Int64.sub mem_top 16L)); Reg Rax];
      Retq, [];
    ]
]

let euclid_gcd n1 n2 = 
  [
    text "gcd" 
    [
      Cmpq,  [~%Rsi; ~$0];
      J Ge,  [~$$"exit"];
      Callq, [~$$"mod"];  
      Movq, [~%Rsi; ~%Rdi];
      Movq, [~%Rax; ~%Rsi];
      Callq, [~$$"gcd"]; 
      Retq,  [];
    ];
    text "mod"
    [
      Cmpq, [~%Rdi; ~%Rsi];
      J Gt, [~$$"finish_mod"];
      Subq, [~%Rsi; ~%Rdi];
      Jmp, [~$$"mod"];
    ];
    text "finish_mod"
    [
      Movq, [~%Rdi; ~%Rax];
      Retq, [];
    ];
    data "unordered" 
    [
      Asciz "test data in between text";
      Quad (Lit 1L);
    ];
    text "exit"
    [
      Movq,  [~%Rdi; ~%Rax];
      Retq, [];
    ];
    gtext "main" 
    [
        Movq,  [~$n1; ~%Rdi]; 
        Movq,  [~$n2; ~%Rsi]; 
        Callq, [~$$"gcd"]; 
        Retq,  [];
    ];
  ]

let my_mod n1 n2 = 
  [
    text "mod"
    [
      Cmpq, [~%Rdi; ~%Rsi];
      J Ge, [~$$"finish_mod"];
      Subq, [~%Rsi; ~%Rdi];
      Jmp, [~$$"mod"];
    ];
    text "finish_mod"
    [
      Movq, [~%Rdi; ~%Rax];
      Retq, [];
    ];
    gtext "main" 
    [
        Movq,  [~$n1; ~%Rdi]; 
        Movq,  [~$n2; ~%Rsi]; 
        Callq, [~$$"mod"]; 
        Retq,  [];
    ];
  ]

let redefinedsym_test (p:prog) () =
  try ignore (assemble p);
    failwith "Should have raised Redefined_sym"
  with 
    | Redefined_sym _ -> ()
    | _ -> failwith "Should have raised Redefined_sym"

let provided_tests : suite = [
  Test ("Student-Provided Big Test for Part III: Score recorded as PartIIITestCase", [
    ("incr_loop", program_test incr_loop 55L);
    ("top_mem", program_test top_of_mem 10L);
    ("repeat_label", redefinedsym_test repeated_label);
    ("call_ret", program_test call_ret 69L);
    ("mod1", program_test (my_mod 2614 236) 18L);
    ("mod2", program_test (my_mod 213 2614) 213L);
    ("euclidian_gcd1", program_test (euclid_gcd 1 100) 1L);
    ("euclidian_gcd2", program_test (euclid_gcd 16753 384) 1L);
    ("euclidian_gcd3", program_test (euclid_gcd 7623 3962) 7L);
    ("euclidian_gcd4", program_test (euclid_gcd 273864 4284234) 6L);
  ]);
] 
