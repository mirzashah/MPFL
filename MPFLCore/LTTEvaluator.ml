(* LTTEvaluator.ml
   Mirza A. Shah
   This module is transitions a plan instance tree based on its current lifetime state to next lt state before
   being handed off to planner invocation evaluator    
*)

open MPFLTypes;;
open MPFLPrettyPrint;;
open Printf;;

exception BadCurrentSerialState;;

let rec eval_mpflBool (boolVal : mpflBool) = MPFLUnits.mpflBool_2_bool boolVal;;

let rec currentSerial s1 s2 =
	match (s1, s2) with
		| (Off(a), Off(b)) -> Off(a)
		| (Off(a), On(b))  -> raise BadCurrentSerialState
		| (Off(a), End(b)) -> raise BadCurrentSerialState
		| (On(a), Off(b))  -> On(a)
		| (On(a), On(b))   -> raise BadCurrentSerialState
		| (On(a), End(b))  -> raise BadCurrentSerialState
		| (End(a), Off(b)) -> Off(b)
		| (End(a), On(b))  -> On(b)
		| (End(a), End(b)) -> End(b);;

exception BadPrecedenceGroup;; 

let precedenceGroup s1 s2 =
	match (s1, s2) with
		| (Off(BLOCK), Off(d))       -> Off(BLOCK)
		| (Off(d), Off(BLOCK))       -> Off(BLOCK)
		| (Off(DISABLE), Off(d))     -> Off(DISABLE)
		| (Off(d), Off(DISABLE))     -> Off(DISABLE) 
		| (Off(SYS_RETRACT), Off(d)) -> Off(SYS_RETRACT)
		| (Off(d), Off(SYS_RETRACT)) -> Off(SYS_RETRACT)
		| (Off(INIT), Off(d))        -> Off(INIT)
		| (Off(d), Off(INIT))        -> Off(INIT)
		| (On(RUN), On(d))           -> On(RUN)
		| (On(d), On(RUN))           -> On(RUN) 
		| (On(FORCE_RUN), On(d))     -> On(FORCE_RUN)
		| (On(d), On(FORCE_RUN))     -> On(FORCE_RUN)
		| (On(READY), On(d))         -> On(READY)
		| (On(d), On(READY))         -> On(READY)
		| (End(COMPLETE), End(d))    -> End(COMPLETE)
		| (End(d), End(COMPLETE))    -> End(COMPLETE)
		| (End(RETRACT), End(d))     -> End(RETRACT)
		| (End(d), End(RETRACT))     -> End(RETRACT) 
		| (d1,d2)                    -> raise BadPrecedenceGroup;;
	
let rec currentGroup s1 s2 =
	match (s1, s2) with
		| (Off(a), Off(b)) -> precedenceGroup s1 s2
		| (Off(a), On(b))  -> On(b)
		| (Off(a), End(b)) -> Off(a)
		| (On(a), Off(b))  -> On(a)
		| (On(a), On(b))   -> precedenceGroup s1 s2
		| (On(a), End(b))  -> On(a)
		| (End(a), Off(b)) -> Off(b)
		| (End(a), On(b))  -> On(b)
		| (End(a), End(b)) -> precedenceGroup s1 s2;;

exception BadCurrentXorState;;


let rec currentXor s1 s2 =
    (*printf "inside current xor: s1 = %s, s2 = %s\n" (pp_ltState s1) (pp_ltState s2);*)
	match (s1, s2) with
		| (Off(a), Off(b)) -> Off(a)
		| (Off(a), On(b))  -> raise BadCurrentXorState
		| (Off(a), End(b)) -> raise BadCurrentXorState
		| (On(a), Off(b))  -> On(a)
		| (On(a), On(b))   -> raise BadCurrentXorState
		| (On(a), End(b))  -> raise BadCurrentXorState
		| (End(a), Off(b)) -> if (a=RETRACT) then Off(b) else End(a)
		| (End(a), On(b))  -> if (a=RETRACT) then On(b) else raise BadCurrentXorState
		| (End(a), End(b)) -> if (a=b && a=RETRACT) then End(RETRACT) else End(COMPLETE);;

exception BadPrecedenceParallel;;

let precedenceParallel s1 s2 =
	match (s1, s2) with
		| (Off(BLOCK), Off(d))       -> Off(BLOCK)
		| (Off(d), Off(BLOCK))       -> Off(BLOCK)
		| (Off(SYS_RETRACT), Off(d)) -> Off(SYS_RETRACT)
		| (Off(d), Off(SYS_RETRACT)) -> Off(SYS_RETRACT) 
		| (Off(DISABLE), Off(d))     -> Off(DISABLE)
		| (Off(d), Off(DISABLE))     -> Off(DISABLE)
		| (Off(INIT), Off(d))        -> Off(INIT)
		| (Off(d), Off(INIT))        -> Off(INIT)
		| (On(RUN), On(d))           -> On(RUN)
		| (On(d), On(RUN))           -> On(RUN) 
		| (On(FORCE_RUN), On(d))     -> On(FORCE_RUN)
		| (On(d), On(FORCE_RUN))     -> On(FORCE_RUN)
		| (On(READY), On(d))         -> On(READY)
		| (On(d), On(READY))         -> On(READY)
		| (End(COMPLETE), End(d))    -> End(COMPLETE)
		| (End(d), End(COMPLETE))    -> End(COMPLETE)
		| (End(RETRACT), End(d))     -> End(RETRACT)
		| (End(d), End(RETRACT))     -> End(RETRACT) 
		| _                          -> raise BadPrecedenceParallel;;

exception BadCurrentParallelState;;

let rec currentParallel s1 s2 =
	match (s1, s2) with
		| (Off(a), Off(b)) -> precedenceParallel s1 s2
		| (Off(a), On(b))  -> On(b)
		| (Off(a), End(b)) -> Off(a)
		| (On(a), Off(b))  -> On(a)
		| (On(a), On(b))   -> precedenceParallel s1 s2
		| (On(a), End(b))  -> On(a)
		| (End(a), Off(b)) -> Off(b)
		| (End(a), On(b))  -> On(b)
		| (End(a), End(b)) -> precedenceParallel s1 s2;;

exception BadCurrentState;;

let rec current e =
   (*printf "Inside current() -> e = %s\n--------------------------------------\n" (pp_planExp e);*)
   match (e) with
		| (PlanInst(n, s, NIL, t, cs, ih, ch))   -> s
		| (PlanInst(n, s, Do(e), t, cs, ih, ch)) -> current e
		| (Op(SERIAL, e1, e2))                   -> currentSerial (current e1) (current e2)
		| (Op(XOR, e1, e2))                      -> currentXor (current e1) (current e2)
		| (Op(GROUP, e1, e2))                    -> currentGroup (current e1) (current e2)
		| (Op(PARALLEL, e1, e2))                 -> currentParallel (current e1) (current e2)
		| (IfThenElse(cond, e1, e2))             -> if(eval_mpflBool cond) then (current e1) else (current e2);; 

exception BadEvalSerial;;

(** The LTT Evaluator does not have permission to change a running or forced running instance to ready. Applying ready
results in no effect. The changeOnState function is used to enforce this policy. *)

exception InvalidUseOfChangeOnState;;

let changeOnState e a=
	match(e, a) with
		| (On(RUN), On(READY))       -> On(RUN)
		| (On(FORCE_RUN), On(READY)) -> On(FORCE_RUN)
		| (On(RUN), On(FORCE_RUN))   -> On(RUN)
		| (On(e1), e2)               -> e2
		| (e1, e2)                   -> raise InvalidUseOfChangeOnState;;

let rec nextSerial e1 e2 app = 
	match (e1, e2, app) with
		| (Off(a), Off(b), Off(c)) -> (Off(c), Off(c))
		| (Off(a), Off(b), On(c))  -> (On(c), Off(BLOCK))
		| (Off(a), Off(b), End(c)) -> (End(c), End(c)) (*Note: Jan 27 changing this *)
		| (Off(a), On(b), Off(c))  -> raise BadEvalSerial
		| (Off(a), On(b), On(c))   -> raise BadEvalSerial
		| (Off(a), On(b), End(c))  -> raise BadEvalSerial
		| (Off(a), End(b), Off(c)) -> raise BadEvalSerial
		| (Off(a), End(b), On(c))  -> raise BadEvalSerial
		| (Off(a), End(b), End(c)) -> raise BadEvalSerial
		| (On(a), Off(b), Off(c))  -> (Off(c), Off(c))
		| (On(a), Off(b), On(c))   -> ((changeOnState (On(a)) (On(c))), Off(BLOCK))
		| (On(a), Off(b), End(c))  -> (End(c), End(c)) (*Note: Jan 27 changing this *)
		| (On(a), On(b), Off(c))   -> raise BadEvalSerial
		| (On(a), On(b), On(c))    -> raise BadEvalSerial
		| (On(a), On(b), End(c))   -> raise BadEvalSerial
		| (On(a), End(b), Off(c))  -> raise BadEvalSerial
		| (On(a), End(b), On(c))   -> raise BadEvalSerial
		| (On(a), End(b), End(c))  -> raise BadEvalSerial
		| (End(a), Off(b), Off(c)) -> (End(a), Off(c))
		| (End(a), Off(b), On(c))  -> (End(a), On(c))
		| (End(a), Off(b), End(c)) -> (End(a), End(c))
		| (End(a), On(b), Off(c))  -> (End(a), Off(c))
		| (End(a), On(b), On(c))   -> (End(a), (changeOnState (On(b)) (On(c))))
		| (End(a), On(b), End(c))  -> (End(a), End(c))
		| (End(a), End(b), Off(c)) -> (End(a), End(b))
		| (End(a), End(b), On(c))  -> (End(a), End(b))
		| (End(a), End(b), End(c)) -> (End(a), End(b));;

exception BadEvalGroup;;

let rec nextGroup e1 e2 app =
	match (e1, e2, app) with
		| (Off(a), Off(b), Off(c)) -> (Off(c), Off(c))
		| (Off(a), Off(b), On(c))  -> (On(c), On(c))
		| (Off(a), Off(b), End(c)) -> (End(c), End(c))
		| (Off(a), On(b), Off(c))  -> (Off(c), Off(c))
		| (Off(a), On(b), On(c))   -> (On(c), (changeOnState (On(b)) (On(c))))
		| (Off(a), On(b), End(c))  -> (End(c), End(c))
		| (Off(a), End(b), Off(c)) -> (Off(c), End(b))
		| (Off(a), End(b), On(c))  -> (On(c), End(b))
		| (Off(a), End(b), End(c)) -> (End(c), End(b))
		| (On(a), Off(b), Off(c))  -> (Off(c), Off(c))
		| (On(a), Off(b), On(c))   -> ((changeOnState (On(a)) (On(c))), On(c))
		| (On(a), Off(b), End(c))  -> (End(c), End(c))
		| (On(a), On(b), Off(c))   -> (Off(c), Off(c))
		| (On(a), On(b), On(c))    -> ((changeOnState (On(a)) (On(c))), (changeOnState (On(b)) (On(c))))
		| (On(a), On(b), End(c))   -> (End(c), End(c))
		| (On(a), End(b), Off(c))  -> (Off(c), Off(c))
		| (On(a), End(b), On(c))   -> ((changeOnState (On(a)) (On(c))), End(b))
		| (On(a), End(b), End(c))  -> (End(c), End(b))
		| (End(a), Off(b), Off(c)) -> (End(a), Off(c))
		| (End(a), Off(b), On(c))  -> (End(a), On(c))
		| (End(a), Off(b), End(c)) -> (End(a), End(c))
		| (End(a), On(b), Off(c))  -> (End(a), Off(c))
		| (End(a), On(b), On(c))   -> (End(a), (changeOnState (On(b)) (On(c))))
		| (End(a), On(b), End(c))  -> (End(a), End(c))
		| (End(a), End(b), Off(c)) -> (End(a), End(b))
		| (End(a), End(b), On(c))  -> (End(a), End(b))
		| (End(a), End(b), End(c)) -> (End(a), End(b));;

exception BadEvalXor;;

let rec nextXor e1 e2 app =
	match (e1, e2, app) with
		| (Off(a), Off(b), Off(c)) -> (Off(c), Off(SYS_RETRACT))
		| (Off(a), Off(b), On(c))  -> (On(c), Off(SYS_RETRACT))
		| (Off(a), Off(b), End(c)) -> (End(c), End(RETRACT))
		| (Off(a), On(b), Off(c))  -> raise BadEvalXor
		| (Off(a), On(b), On(c))   -> raise BadEvalXor
		| (Off(a), On(b), End(c))  -> raise BadEvalXor
		| (Off(a), End(b), Off(c)) -> raise BadEvalXor
		| (Off(a), End(b), On(c))  -> raise BadEvalXor
		| (Off(a), End(b), End(c)) -> raise BadEvalXor
		| (On(a), Off(b), Off(c))  -> (Off(c),Off(SYS_RETRACT))
		| (On(a), Off(b), On(c))   -> ((changeOnState (On(a)) (On(c))), Off(SYS_RETRACT))
		| (On(a), Off(b), End(c))  -> (End(c), (if (c=RETRACT) then On(READY) else End(RETRACT))) (**TODO: Change on spreadsheet)**)
		| (On(a), On(b), Off(c))   -> raise BadEvalXor
		| (On(a), On(b), On(c))    -> raise BadEvalXor
		| (On(a), On(b), End(c))   -> raise BadEvalXor
		| (On(a), End(b), Off(c))  -> raise BadEvalXor
		| (On(a), End(b), On(c))   -> raise BadEvalXor
		| (On(a), End(b), End(c))  -> raise BadEvalXor
		| (End(a), Off(b), Off(c)) -> (End(a), (if (a=RETRACT) then Off(c) else End(RETRACT))) (**TODO: Change on spreadsheet**)
		| (End(a), Off(b), On(c))  -> (End(a), (if (a=RETRACT) then On(c) else End(RETRACT))) (**TODO: Change on spreadsheet**)
		| (End(a), Off(b), End(c)) -> (End(a), (if (a=RETRACT) then End(c) else End(RETRACT))) (**TODO: Change on spreadsheet**)
		| (End(a), On(b), Off(c))  -> (End(a), (if (a=RETRACT) then Off(c) else raise BadEvalXor)) (**TODO: Change on spreadsheet**)
		| (End(a), On(b), On(c))   -> (End(a), (if (a=RETRACT) then On(c) else raise BadEvalXor)) (**TODO: Change on spreadsheet**)
		| (End(a), On(b), End(c))  -> (End(a), (if (a=RETRACT) then End(c) else raise BadEvalXor)) (**TODO: Change on spreadsheet**)
		| (End(a), End(b), Off(c)) -> (End(a), End(b)) (**TODO: Change on spreadsheet**)
		| (End(a), End(b), On(c))  -> (End(a), End(b)) (**TODO: Change on spreadsheet**)
		| (End(a), End(b), End(c)) -> (End(a), End(b));; (**TODO: Change on spreadsheet**)

exception BadEvalParallel;;

(** With parallel, if something is on, or is attempted to be turned on, and that on state (whether current or next) is
going to be On(RUN)...the other item needs to have On(FORCE_RUN) applied to it if it isn't already in On(RUN) itself**)

let rec correctForForceRun e1 e2 =
	match(e1, e2) with
		| (On(RUN), On(RUN))             -> (e1, e2)
		| (On(RUN), On(FORCE_RUN))       -> (e1, e2)
		| (On(RUN), On(READY))           -> (e1, On(FORCE_RUN))
		| (On(FORCE_RUN), On(RUN))       -> (e1, e2)
		| (On(FORCE_RUN), On(FORCE_RUN)) -> (e1,e2)
		| (On(FORCE_RUN), On(READY))     -> (e1, On(FORCE_RUN))
		| (On(READY), On(RUN))           -> (On(FORCE_RUN), e2)
		| (On(READY), On(FORCE_RUN))     -> (On(FORCE_RUN), e2)
		| _                              -> (e1,e2)

let rec nextParallelBasic e1 e2 app = 
		match (e1, e2, app) with
		| (Off(a), Off(b), Off(c)) -> (Off(c), Off(c))
		| (Off(a), Off(b), On(c))  -> (On(c), On(c))
		| (Off(a), Off(b), End(c)) -> (End(c), End(c))
		| (Off(a), On(b), Off(c))  -> (Off(c), Off(c))
		| (Off(a), On(b), On(c))   -> (On(c), (changeOnState (On(b)) (On(c)))) 
		| (Off(a), On(b), End(c))  -> (End(c), End(c))
		| (Off(a), End(b), Off(c)) -> (Off(c), End(b))
		| (Off(a), End(b), On(c))  -> (On(c), End(b))
		| (Off(a), End(b), End(c)) -> (End(c), End(b))
		| (On(a), Off(b), Off(c))  -> (Off(c), Off(c))
		| (On(a), Off(b), On(c))   -> ((changeOnState (On(a)) (On(c))), On(c))
		| (On(a), Off(b), End(c))  -> (End(c), End(c))
		| (On(a), On(b), Off(c))   -> (Off(c), Off(c))
		| (On(a), On(b), On(c))    -> ((changeOnState (On(a)) (On(c))), (changeOnState (On(b)) (On(c))))
		| (On(a), On(b), End(c))   -> (End(c), End(c))
		| (On(a), End(b), Off(c))  -> (Off(c), Off(c))
		| (On(a), End(b), On(c))   -> ((changeOnState (On(a)) (On(c))), End(b))
		| (On(a), End(b), End(c))  -> (End(c), End(b))
		| (End(a), Off(b), Off(c)) -> (End(a), Off(c))
		| (End(a), Off(b), On(c))  -> (End(a), On(c))
		| (End(a), Off(b), End(c)) -> (End(a), End(c))
		| (End(a), On(b), Off(c))  -> (End(a), Off(c))
		| (End(a), On(b), On(c))   -> (End(a), (changeOnState (On(b)) (On(c))))
		| (End(a), On(b), End(c))  -> (End(a), End(c))
		| (End(a), End(b), Off(c)) -> (End(a), End(b))
		| (End(a), End(b), On(c))  -> (End(a), End(b))
		| (End(a), End(b), End(c)) -> (End(a), End(b));;

let rec nextParallel e1 e2 app = (let (v1,v2) = (nextParallelBasic e1 e2 app) in (correctForForceRun v1 v2));;

exception Unimplemented;;

let rec eval e app =
    match (e, app) with
			| (PlanInst(n,On(s), NIL, t, cs, ih, ch), v) -> PlanInst(n, (changeOnState (On(s)) (v)), NIL, t, cs, ih, ch)
			| (PlanInst(n,s,NIL, t, cs, ih, ch), v)      -> PlanInst(n, v, NIL, t, cs, ih, ch)            
			| (PlanInst(n,s,Do(e), t, cs, ih, ch), v)    -> let nextExp = eval e v in
                                                            let nextState = current nextExp in
                                                                PlanInst(n, nextState, Do(nextExp), t, cs, ih, ch)
			| (Op(SERIAL,e1,e2), app)            -> let (v1,v2) = (nextSerial (current e1) (current e2) (app)) in Op(SERIAL, (eval e1 v1), (eval e2 v2))
			| (Op(GROUP,e1,e2), app)             -> let (v1,v2) = (nextGroup (current e1) (current e2) (app)) in Op(GROUP, (eval e1 v1), (eval e2 v2))
			| (Op(XOR,e1,e2), app)               -> let (v1,v2) = (nextXor (current e1) (current e2) (app)) in Op(XOR, (eval e1 v1), (eval e2 v2))                
			| (Op(PARALLEL, e1, e2), app)        -> let (v1,v2) = (nextParallel (current e1) (current e2) (app)) in Op(PARALLEL, (eval e1 v1), (eval e2 v2))
            | (IfThenElse(cond, e1, e2), app)    -> if(eval_mpflBool cond) then
                                                        let nextExp1 = eval e1 app in
                                                        let nextExp2 = eval e2 (Off(SYS_RETRACT)) in
                                                        IfThenElse(cond, nextExp1, nextExp2)
                                                    else
                                                        let nextExp1 = eval e1 (Off(SYS_RETRACT)) in
                                                        let nextExp2 = eval e2 app in
                                                        IfThenElse(cond, nextExp1, nextExp2);;