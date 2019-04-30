open Bap.Std
open Bap_plugins.Std
open Core_kernel
open Monads.Std

module type Ctxt = sig
  type 'a t
  val read : var -> word t
  val load : addr -> word t

  val saved : var -> word -> unit t
  val stored : addr -> word -> unit t
  val jumped : addr -> unit t
  val undefined : string -> unit t
  val unknown : string -> typ -> word t
  val interrupted : int -> unit t
  include Monad.S with type 'a t := 'a t
end

module Interpreter(Ctxt : Ctxt) = struct
  open Ctxt.Syntax

  let subst v r =
    (object(self)
      inherit Exp.mapper
      method! map_var v' =
        if Var.equal v v' then r else (Var v')
      method! map_let v' ~exp:x ~body:y =
        if Var.equal v v' then Let(v',x,y)
        else Let(v',self#map_exp x, self#map_exp y)
    end)#map_exp

  let rec eval : bil -> unit Ctxt.t =
    Ctxt.List.iter  ~f:(function
        | Bil.Move (v,x) -> exp x >>= Ctxt.saved v
        | Jmp x -> exp x >>= Ctxt.jumped
        | Special s -> Ctxt.undefined s
        | While (cnd, prog) -> loop cnd prog
        | If (cnd, yes, nay) ->
          exp cnd >>= fun r ->
          if Word.is_zero r then eval nay else eval yes
        | CpuExn n  -> Ctxt.interrupted n)
  and loop cnd body =
    exp cnd >>= fun r ->
    if Word.is_zero r then Ctxt.return ()
    else eval body >>= fun () -> loop cnd body
  and exp : exp -> word Ctxt.t = function
    | Load (m, a, ed, sz) ->
      exp m >>= fun _ ->
      exp a >>= fun a ->
      Seq.range 0 (Size.in_bytes sz) |>
      Ctxt.Seq.map ~f:(fun off -> Ctxt.load (Addr.nsucc a off)) >>|
      Seq.reduce_exn ~f:(fun x y -> match ed with
          | BigEndian -> Word.concat x y
          | LittleEndian -> Word.concat y x)
    | Store (m, a, v, ed, sz) ->
      exp m >>= fun m ->
      exp a >>= fun a ->
      exp v >>= fun v ->
      Seq.zip
        (Word.enum_bytes v ed)
        (Seq.range 0 (Size.in_bytes sz)) |>
      Ctxt.Seq.iter ~f:(fun (byte,off) ->
          Ctxt.stored (Addr.nsucc a off) byte) >>= fun () ->
      Ctxt.return m
    | BinOp (op, x, y) ->
      exp x >>= fun x ->
      exp y >>| fun y ->
      Bil.Apply.binop op x y
    | UnOp (op, x) -> exp x >>| Bil.Apply.unop op
    | Var v -> Ctxt.read v
    | Int w -> Ctxt.return w
    | Cast (c,w,x) -> exp x >>| Bil.Apply.cast c w
    | Let (v, x, y) -> exp x >>= fun r -> exp (subst v (Int r) y)
    | Unknown (s,t) -> Ctxt.unknown s t
    | Ite (cnd, yes, nay) ->
      exp cnd >>= fun r ->
      if Word.is_zero r then exp nay else exp yes
    | Extract (hi, lo, x) ->
      exp x >>| fun x -> Word.extract_exn ~hi ~lo x
    | Concat (x, y) ->
      exp x >>= fun x ->
      exp y >>| fun y ->
      Word.concat x y
end

module Ctxt = struct
  type ref =
    | Reg of var
    | Ptr of addr
  [@@deriving compare, sexp_of]

  module Ref = struct
    type t = ref
    include Base.Comparable.Make(struct
        type t =  ref [@@deriving compare, sexp_of]
      end)
  end

  type state = {
    ctxt : word Map.M(Ref).t;
    input : int Map.M(Ref).t;
    state : word Map.M(Int).t;
    space : Word.Set.t;
  }

  module Ctxt = Monad.State.Make(struct type t = state end)(Monad.Ident)
  open Ctxt.Syntax

  let cast m x = Word.extract_exn ~hi:(m-1) x

  let deref m v =
    Ctxt.get () >>= fun s ->
    match Map.find s.ctxt v with
    | Some r -> Ctxt.return r
    | None -> match Map.find s.input v with
      | Some p -> Ctxt.return @@ Map.find_exn s.state p
      | None ->
        let r = cast m (Set.min_elt_exn s.space) in
        let p = Map.length s.input in
        let input = Map.add_exn s.input v p in
        let state = Map.add_exn s.state p r in
        Ctxt.put {s with state; input} >>| fun () ->
        r


  let read v = match Var.typ v with
    | Type.Imm m -> deref m (Reg v)
    | _ -> Ctxt.return Word.b0
  let load x = deref 8 (Ptr x)

  let saved v x = Ctxt.update @@ fun s -> {
      s with ctxt = Map.set s.ctxt (Reg v) x
    }

  let stored a x = Ctxt.update @@ fun s -> {
      s with ctxt = Map.set s.ctxt (Ptr a) x
    }

  let interrupted n =
    let v = Var.create "CPUEXN" (Type.Imm 8) in
    saved v (Word.of_int ~width:8 n)

  let jumped dst =
    let pc = Var.create "PC" (Type.Imm (Word.bitwidth dst)) in
    saved pc dst

  let unknown _ t = Ctxt.return @@ match t with
    | Type.Imm x -> Word.zero x
    |  _ -> Word.b1

  let undefined s =
    let v = Var.create s bool_t in
    saved v Word.b1


  let diff {ctxt=m1} {ctxt=m2} =
    Map.symmetric_diff m1 m2 ~data_equal:Word.equal |>
    Seq.filter ~f:(fun (k,r) -> match k with
        | Reg v -> Var.is_physical v
        | Ptr _ -> true)

  let differs diff = not (Seq.is_empty diff)

  let next_state states x =
    let x = Word.succ x in
    match Seq.hd @@ Set.to_sequence ~greater_or_equal_to:x states with
    | None ->
      Set.min_elt_exn states
    | Some r ->
      Word.extract_exn ~hi:(Word.bitwidth x - 1) r

  let empty space = {
    ctxt  = Map.empty (module Ref);
    input = Map.empty (module Ref);
    state = Map.empty (module Int);
    space;
  }

  let pp_ref ppf = function
    | Reg v -> Format.fprintf ppf "%a" Var.pp v
    | Ptr p -> Format.fprintf ppf "%a" Addr.pp p

  let pp_inputs ppf state =
    Format.pp_print_list
      ~pp_sep:(fun ppf () -> Format.pp_print_string ppf ", ")
      Word.pp ppf
      (Map.data state)

  let succ s =
    let state = Map.empty (module Int) in
    Map.to_sequence s.state |>
    Seq.fold ~init:(state,true) ~f:(fun (word,incr) (p,x) ->
        if incr then
          let next = next_state s.space x in
          (Map.set word p next, Word.(next <= x))
        else (Map.set word p x,false)) |> function
    | (_,true) -> None
    | (state,false) -> Some {
        (empty s.space)
        with state;
             input = s.input
      }

  include Monad.State.T1(struct type t = state end)(Monad.Ident)
  include Ctxt
end

module Interperter = Interpreter(Ctxt)

let states space bil =
  let space = Set.of_list (module Word) space in
  let run ctxt = snd @@ Ctxt.run (Interperter.eval bil) ctxt in
  Seq.unfold ~init:(Some (Ctxt.empty space)) ~f:(function
      | None -> None
      | Some ctxt ->
        let ctxt = run ctxt in
        Some (ctxt,Ctxt.succ ctxt))

module Dis = Disasm_expert

let space = [
  Word.zero 128;
  Word.ones 128;
  Word.one 128;
  Word.of_int64 ~width:128 0xDEADBEEFL;
]

let run arch bytes =
  let bytes = Bigstring.of_string bytes in
  let endian = Arch.endian arch in
  let width = Size.in_bits (Arch.addr_size arch) in
  let entry = Addr.of_int 0x400000 ~width in
  let mem = ok_exn (Memory.create endian entry bytes) in
  Disasm_expert.Linear.With_exn.sweep arch mem |>
  List.concat_map ~f:(function
      |  (_, Some insn) ->
        let bil = Insn.bil insn in
        Format.printf "%a@\n" Bil.pp bil;
        bil

      | _ -> []) |>
  states space

let print_state () =
  Array.iter (Array.subo ~pos:2 Sys.argv) ~f:(fun input ->
      run `x86_64 (Scanf.unescaped input) |>
      Seq.iteri ~f:(fun i {Ctxt.ctxt; state} ->
          Format.printf "State %d: [%a]@\n" i Ctxt.pp_inputs state;
          Map.iteri ctxt ~f:(fun ~key ~data ->
              Format.printf "    %a => %a@\n"
                Ctxt.pp_ref key
                Word.pp data)))

let diff_states () =
  let base = run `x86_64 (Scanf.unescaped Sys.argv.(2)) in
  Array.iter (Array.subo ~pos:2 Sys.argv) ~f:(fun input ->
      let test = run `x86_64 (Scanf.unescaped input) in
      Seq.zip base test |>
      Seq.iteri ~f:(fun i (base,test) ->
          let diff = Ctxt.diff base test in
          if Ctxt.differs diff then begin
            Format.printf "State %d differs:\n" i;
            Seq.iter diff ~f:(fun (k,d) -> match d with
                | `Left x ->
                  Format.printf "  < %a = %a@\n"
                    Ctxt.pp_ref k Word.pp x
                | `Right x ->
                  Format.printf "  > %a = %a@\n"
                    Ctxt.pp_ref k Word.pp x
                | `Unequal (x,y) ->
                  Format.printf "  | %a = %a <> %a"
                    Ctxt.pp_ref k Word.pp x Word.pp y)
          end else
            Format.printf "State %d matches!\n%!" i))

let print_help () =
  Format.eprintf "Usage: ./randeval {eval|diff} bytes...\n%!"


let () = Plugins.run ()


let () =
  if Array.length Sys.argv < 3 then print_help ()
  else match Sys.argv.(1) with
    | "eval" -> print_state ()
    | "diff" -> diff_states ()
    | _ -> print_help ()