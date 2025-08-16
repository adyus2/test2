open Ast

(* 寄存器分类 *)
type reg_type = 
  | CallerSaved   (* 调用者保存寄存器 *)
  | CalleeSaved   (* 被调用者保存寄存器 *)
  | TempReg       (* 临时寄存器 *)

(* 上下文类型 *)
type context = {
    current_func: string;
    local_offset: int;
    frame_size: int;
    var_offset: (string * int) list list;
    next_temp: int;
    label_counter: int;
    loop_stack: (string * string) list;
    saved_regs: string list;
    reg_map: (string * reg_type) list;
    param_count: int;
    temp_regs_used: int;
    saved_area_size: int;
    max_local_offset: int; (* 新增字段 *)
}

(* 创建新上下文 *)
let create_context func_name =
    let reg_map = [
        ("a0", CallerSaved); ("a1", CallerSaved); ("a2", CallerSaved); ("a3", CallerSaved);
        ("a4", CallerSaved); ("a5", CallerSaved); ("a6", CallerSaved); ("a7", CallerSaved);
        ("s0", CalleeSaved); ("s1", CalleeSaved); ("s2", CalleeSaved); ("s3", CalleeSaved);
        ("s4", CalleeSaved); ("s5", CalleeSaved); ("s6", CalleeSaved); ("s7", CalleeSaved);
        ("s8", CalleeSaved); ("s9", CalleeSaved); ("s10", CalleeSaved); ("s11", CalleeSaved);
        ("t0", TempReg); ("t1", TempReg); ("t2", TempReg); ("t3", TempReg);
        ("t4", TempReg); ("t5", TempReg); ("t6", TempReg)
    ] in
    (* 初始保存区域大小：RA(4字节) + 所有被调用者保存寄存器(12*4=48字节) *)
    { current_func = func_name;
      local_offset = 0;
      frame_size = 0;
      var_offset = [[]];  (* 初始化为一个作用域（空列表） *)
      next_temp = 0;
      label_counter = 0;
      loop_stack = [];
      saved_regs = ["s0"; "s1"; "s2"; "s3"; "s4"; "s5"; "s6"; "s7"; "s8"; "s9"; "s10"; "s11"];
      reg_map = reg_map;
      param_count = 0;
      temp_regs_used = 0;
      max_local_offset = 0; 
      saved_area_size = 52 } (* 4(ra) + 12*4(regs) = 52 *)

(* 栈对齐常量 *)
let stack_align = 16

(* 获取唯一标签 - 使用函数名作为前缀 *)
let fresh_label ctx prefix =
    let n = ctx.label_counter in
    { ctx with label_counter = n + 1 }, 
    Printf.sprintf ".L%s_%s%d" ctx.current_func prefix n  (* 添加函数名前缀 *)

(* 获取变量偏移量 - 支持嵌套作用域查找 *)
let get_var_offset ctx name =
    let rec search = function
        | [] -> None
        | scope :: rest -> 
            try Some (List.assoc name scope)
            with Not_found -> search rest
    in
    match search ctx.var_offset with
    | Some offset -> offset
    | None -> failwith (Printf.sprintf "Undefined variable: %s" name)

(* 添加变量到当前作用域 *)
let add_var ctx name size =
  match ctx.var_offset with
  | current_scope :: rest_scopes ->
      let offset = ctx.saved_area_size + ctx.local_offset in
      let new_scope = (name, offset) :: current_scope in
      { ctx with 
          var_offset = new_scope :: rest_scopes;
          local_offset = ctx.local_offset + size;
          (* 更新最大局部偏移量 *)
          max_local_offset = max ctx.max_local_offset (ctx.local_offset + size)
      }
  | [] -> failwith "No active scope"

(* 分配临时寄存器 - 改进版本，优先使用t寄存器 *)
let alloc_temp_reg ctx =
    (* 优先使用t寄存器，然后使用s寄存器 *)
    let temp_regs = ["t0"; "t1"; "t2"; "t3"; "t4"; "t5"; "t6"] in
    let saved_regs_list = ["s0"; "s1"; "s2"; "s3"; "s4"; "s5"; "s6"; "s7"; "s8"; "s9"; "s10"; "s11"] in
    let all_regs = temp_regs @ saved_regs_list in
    
    if ctx.temp_regs_used >= List.length all_regs then (
        (* 当寄存器不够用时，溢出到栈上 *)
        let spill_offset = ctx.saved_area_size + ctx.max_local_offset + (ctx.temp_regs_used - List.length all_regs + 1) * 4 in
        let spill_reg = Printf.sprintf "SPILL_%d" spill_offset in
        { ctx with temp_regs_used = ctx.temp_regs_used + 1 }, spill_reg
    ) else (
        let reg = List.nth all_regs ctx.temp_regs_used in
        { ctx with temp_regs_used = ctx.temp_regs_used + 1 }, reg
    )

(* 释放临时寄存器 *)
let free_temp_reg ctx =
    if ctx.temp_regs_used > 0 then
        { ctx with temp_regs_used = ctx.temp_regs_used - 1 }
    else ctx

(* 计算栈对齐 *)
let align_stack size align =
    let remainder = size mod align in
    if remainder = 0 then size else size + (align - remainder)

(* 函数序言生成 *)
let gen_prologue ctx func =
    (* 预估需要的局部变量空间 - 这个需要根据函数复杂度调整 *)
    let estimated_locals = 
        match func.name with
        | "main" -> 500  (* main函数需要更多空间 *)
        | _ -> 200        (* 其他函数也增加空间 *)
    in
    (* 总栈大小 = 保存区域 + 预估局部变量空间 *)
    let total_size = align_stack (ctx.saved_area_size + estimated_locals) stack_align in
    (* 生成保存寄存器的汇编代码 *)
    let save_regs_asm = 
        let save_instrs = 
            List.mapi (fun i reg -> 
                Printf.sprintf "    sw %s, %d(sp)" reg (i * 4)
            ) ctx.saved_regs
            @ [Printf.sprintf "    sw ra, %d(sp)" (List.length ctx.saved_regs * 4)]
        in
        String.concat "\n" save_instrs
    in
    let asm = Printf.sprintf "
    .globl %s
%s:
    addi sp, sp, -%d
%s
" func.name func.name total_size save_regs_asm in
    (asm, { ctx with frame_size = total_size })

(* 函数结语生成 *)
let gen_epilogue ctx =
    (* 生成恢复寄存器的汇编代码 - 逆序恢复：先恢复ra，然后s11-s0 *)
    let restore_regs_asm = 
        (* 关键修复：按入栈顺序的逆序恢复 *)
        let restore_list = 
            ["ra"] @ (List.rev ctx.saved_regs) (* 恢复顺序：ra, s11, s10, ..., s0 *)
        in
        (* 关键修复：偏移量计算与保存时完全匹配 *)
        List.mapi (fun i reg ->
            let offset = (List.length ctx.saved_regs * 4) - (i * 4) in
            Printf.sprintf " \n   lw %s, %d(sp)" reg offset
        ) restore_list
        |> String.concat "\n"
    in
    
    Printf.sprintf "
%s
    addi sp, sp, %d
    ret
" restore_regs_asm ctx.frame_size

(* 检查是否是溢出寄存器 *)
let is_spill_reg reg = 
    String.length reg > 5 && String.sub reg 0 5 = "SPILL"

(* 获取溢出寄存器的栈偏移量 *)
let get_spill_offset reg =
    if is_spill_reg reg then
        int_of_string (String.sub reg 6 (String.length reg - 6))
    else
        failwith "Not a spill register"

(* 生成加载溢出寄存器的代码 *)
let gen_load_spill reg temp_reg =
    if is_spill_reg reg then
        let offset = get_spill_offset reg in
        Printf.sprintf "    lw %s, %d(sp)" temp_reg offset
    else ""

(* 生成存储到溢出寄存器的代码 *)
let gen_store_spill reg temp_reg =
    if is_spill_reg reg then
        let offset = get_spill_offset reg in
        Printf.sprintf "    sw %s, %d(sp)" temp_reg offset
    else ""

(* 表达式代码生成 - 改进版本 *)
let rec gen_expr ctx expr =
    match expr with
    | IntLit n -> 
        let (ctx, reg) = alloc_temp_reg ctx in
        if is_spill_reg reg then
            let temp_asm = Printf.sprintf "    li t0, %d\n%s" n (gen_store_spill reg "t0") in
            (ctx, temp_asm, reg)
        else
            (ctx, Printf.sprintf "    li %s, %d" reg n, reg)
    | Var name ->
        let offset = get_var_offset ctx name in
        let (ctx, reg) = alloc_temp_reg ctx in
        if is_spill_reg reg then
            let temp_asm = Printf.sprintf "    lw t0, %d(sp)\n%s" offset (gen_store_spill reg "t0") in
            (ctx, temp_asm, reg)
        else
            (ctx, Printf.sprintf "    lw %s, %d(sp)" reg offset, reg)
    | BinOp (e1, op, e2) ->
        let (ctx, asm1, reg1) = gen_expr ctx e1 in
        let (ctx, asm2, reg2) = gen_expr ctx e2 in
        
        (* 处理溢出寄存器 *)
        let load1 = gen_load_spill reg1 "t0" in
        let load2 = gen_load_spill reg2 "t1" in
        let actual_reg1 = if is_spill_reg reg1 then "t0" else reg1 in
        let actual_reg2 = if is_spill_reg reg2 then "t1" else reg2 in
        
        (* 重用第一个寄存器作为目标寄存器，减少寄存器使用 *)
        let reg_dest = reg1 in
        let actual_reg_dest = actual_reg1 in
        
        let instr = match op with
            | Add -> Printf.sprintf "    add %s, %s, %s" actual_reg_dest actual_reg1 actual_reg2
            | Sub -> Printf.sprintf "    sub %s, %s, %s" actual_reg_dest actual_reg1 actual_reg2
            | Mul -> Printf.sprintf "    mul %s, %s, %s" actual_reg_dest actual_reg1 actual_reg2
            | Div -> Printf.sprintf "    div %s, %s, %s" actual_reg_dest actual_reg1 actual_reg2
            | Mod -> Printf.sprintf "    rem %s, %s, %s" actual_reg_dest actual_reg1 actual_reg2
            | Lt  -> Printf.sprintf "    slt %s, %s, %s" actual_reg_dest actual_reg1 actual_reg2
            | Le  -> Printf.sprintf "    slt %s, %s, %s\n    xori %s, %s, 1" actual_reg_dest actual_reg2 actual_reg1 actual_reg_dest actual_reg_dest
            | Gt  -> Printf.sprintf "    slt %s, %s, %s" actual_reg_dest actual_reg2 actual_reg1
            | Ge  -> Printf.sprintf "    slt %s, %s, %s\n    xori %s, %s, 1" actual_reg_dest actual_reg1 actual_reg2 actual_reg_dest actual_reg_dest
            | Eq  -> Printf.sprintf "    sub %s, %s, %s\n    seqz %s, %s" actual_reg_dest actual_reg1 actual_reg2 actual_reg_dest actual_reg_dest
            | Ne  -> Printf.sprintf "    sub %s, %s, %s\n    snez %s, %s" actual_reg_dest actual_reg1 actual_reg2 actual_reg_dest actual_reg_dest
            | And -> Printf.sprintf "    and %s, %s, %s" actual_reg_dest actual_reg1 actual_reg2
            | Or  -> Printf.sprintf "    or %s, %s, %s" actual_reg_dest actual_reg1 actual_reg2
        in
        
        let store_result = gen_store_spill reg_dest actual_reg_dest in
        
        (* 只释放第二个寄存器，第一个寄存器被重用为结果寄存器 *)
        let ctx = free_temp_reg ctx in
        let full_asm = asm1 ^ "\n" ^ asm2 ^ "\n" ^ load1 ^ "\n" ^ load2 ^ "\n" ^ instr ^ "\n" ^ store_result in
        (ctx, full_asm, reg_dest)
        
    | UnOp (op, e) ->
        let (ctx, asm, reg) = gen_expr ctx e in
        let load_asm = gen_load_spill reg "t0" in
        let actual_reg = if is_spill_reg reg then "t0" else reg in
        let instr = match op with
        | UPlus  -> Printf.sprintf "    mv %s, %s" actual_reg actual_reg  (* 空操作 *)
        | UMinus -> Printf.sprintf "    neg %s, %s" actual_reg actual_reg
        | Not    -> Printf.sprintf "    seqz %s, %s" actual_reg actual_reg
        in
        let store_asm = gen_store_spill reg actual_reg in
        (ctx, asm ^ "\n" ^ load_asm ^ "\n" ^ instr ^ "\n" ^ store_asm, reg)

    | FuncCall (name, args) ->
          (* 先计算所有参数表达式，不调整栈指针 *)
          let (ctx, arg_asm, arg_regs) = gen_args ctx args in
          
          (* 计算额外参数数量 *)
          let n_extra = max (List.length args - 8) 0 in
          let temp_space = 28 + n_extra * 4 in
          let aligned_temp_space = align_stack temp_space stack_align in
          
          (* 调整栈指针 *)
          let stack_adj_asm = 
            if aligned_temp_space > 0 then 
              Printf.sprintf "    addi sp, sp, -%d\n" aligned_temp_space
            else "" in
          
          (* 保存临时寄存器 *)
          let save_temps_asm = 
            List.init 7 (fun i -> 
              Printf.sprintf "    sw t%d, %d(sp)" i (i * 4))
            |> String.concat "\n" in
          
          (* 移动参数到正确位置 *)
          let move_args_asm = 
            let rec move_args regs index asm =
              match regs with
              | [] -> asm
              | reg::rest when index < 8 ->
                  let target = Printf.sprintf "a%d" index in
                  let load_src = gen_load_spill reg "t0" in
                  let actual_src = if is_spill_reg reg then "t0" else reg in
                  let move_instr = 
                    if actual_src = target then ""
                    else Printf.sprintf "    mv %s, %s\n" target actual_src
                  in
                  move_args rest (index+1) (asm ^ load_src ^ "\n" ^ move_instr)
              | reg::rest ->
                  let stack_offset = 28 + (index - 8) * 4 in
                  let load_src = gen_load_spill reg "t0" in
                  let actual_src = if is_spill_reg reg then "t0" else reg in
                  let store_instr = Printf.sprintf "    sw %s, %d(sp)\n" actual_src stack_offset in
                  move_args rest (index+1) (asm ^ load_src ^ "\n" ^ store_instr)
            in
            move_args arg_regs 0 ""
          in
          
          (* 函数调用 *)
          let call_asm = Printf.sprintf "    call %s\n" name in
          
          (* 恢复临时寄存器 *)
          let restore_temps_asm = 
            List.init 7 (fun i -> 
              Printf.sprintf "    lw t%d, %d(sp)" i (i * 4))
            |> String.concat "\n" in
          
          (* 恢复栈指针 *)
          let restore_stack_asm = 
            if aligned_temp_space > 0 then 
              Printf.sprintf "    addi sp, sp, %d" aligned_temp_space
            else "" in
          
          (* 将返回值移动到目标寄存器 *)
          let (ctx, reg_dest) = alloc_temp_reg ctx in
          let move_result = 
            if is_spill_reg reg_dest then
              Printf.sprintf "    mv t0, a0\n%s" (gen_store_spill reg_dest "t0")
            else
              Printf.sprintf "    mv %s, a0" reg_dest
          in
          
          (* 组合汇编代码 *)
          let asm = arg_asm ^ stack_adj_asm ^ save_temps_asm ^ "\n" ^ 
                    move_args_asm ^ call_asm ^ "\n" ^ 
                    restore_temps_asm ^ "\n" ^ restore_stack_asm ^ "\n" ^ 
                    move_result in
          
          let ctx = List.fold_left (fun ctx _ -> free_temp_reg ctx) ctx arg_regs in
            (ctx, asm, reg_dest)

(* 生成参数代码 - 返回参数寄存器列表 *)
and gen_args ctx args =
  let rec process_args ctx asm regs count = function
    | [] -> (ctx, asm, List.rev regs)
    | arg::rest ->
        let (ctx, arg_asm, reg) = gen_expr ctx arg in
        let new_asm = asm ^ arg_asm in
        process_args ctx new_asm (reg::regs) (count+1) rest
  in
  process_args ctx "" [] 0 args

(* 处理语句列表的辅助函数 *)
let rec gen_stmts ctx stmts =
    List.fold_left (fun (ctx, asm) stmt ->
        let (ctx', stmt_asm) = gen_stmt ctx stmt in
        (ctx', asm ^ "\n" ^ stmt_asm)
    ) (ctx, "") stmts

(* 语句代码生成 *)
and gen_stmt ctx stmt =
    match stmt with
    | Block stmts ->
        (* 进入新作用域：压入一个新的空作用域 *)
        let new_ctx = { ctx with var_offset = [] :: ctx.var_offset } in
        let (ctx_after, asm) = gen_stmts new_ctx stmts in
        (* 离开作用域：弹出当前作用域 *)
        let popped_ctx = 
            match ctx_after.var_offset with
            | _ :: outer_scopes -> { ctx_after with var_offset = outer_scopes }
            | [] -> failwith "Scope stack underflow"
        in
        (popped_ctx, asm)
    
    | VarDecl (name, expr) ->
        let (ctx, expr_asm, reg) = gen_expr ctx expr in
        let ctx = add_var ctx name 4 in
        let offset = get_var_offset ctx name in
        let load_asm = gen_load_spill reg "t0" in
        let actual_reg = if is_spill_reg reg then "t0" else reg in
        let asm = expr_asm ^ "\n" ^ load_asm ^ "\n" ^ Printf.sprintf "    sw %s, %d(sp)" actual_reg offset in
        (free_temp_reg ctx, asm)
    
    | VarAssign (name, expr) ->
        let offset = get_var_offset ctx name in
        let (ctx, expr_asm, reg) = gen_expr ctx expr in
        let load_asm = gen_load_spill reg "t0" in
        let actual_reg = if is_spill_reg reg then "t0" else reg in
        let asm = expr_asm ^ "\n" ^ load_asm ^ "\n" ^ Printf.sprintf "    sw %s, %d(sp)" actual_reg offset in
        (free_temp_reg ctx, asm)
    
     | If (cond, then_stmt, else_stmt) ->
        let (ctx, cond_asm, cond_reg) = gen_expr ctx cond in
        let (ctx, then_label) = fresh_label ctx "if_then" in
        let (ctx, else_label) = fresh_label ctx "if_else" in
        let (ctx, end_label) = fresh_label ctx "if_end" in
        
        let load_cond = gen_load_spill cond_reg "t0" in
        let actual_cond_reg = if is_spill_reg cond_reg then "t0" else cond_reg in
        
        let (ctx, then_asm) = gen_stmt ctx then_stmt in
        let (ctx, else_asm) = match else_stmt with
            | Some s -> gen_stmt ctx s
            | None -> (ctx, "") in
        
        let asm = cond_asm ^ "\n" ^ load_cond ^
                Printf.sprintf "\n    beqz %s, %s" actual_cond_reg else_label ^
                Printf.sprintf "\n    j %s" then_label ^
                Printf.sprintf "\n%s:" else_label ^
                else_asm ^
                Printf.sprintf "\n    j %s" end_label ^
                Printf.sprintf "\n%s:" then_label ^
                then_asm ^
                Printf.sprintf "\n    j %s" end_label ^
                Printf.sprintf "\n%s:" end_label in
        (free_temp_reg ctx, asm)
    
    | While (cond, body) ->
        let (ctx, begin_label) = fresh_label ctx "loop_begin" in
        let (ctx, end_label) = fresh_label ctx "loop_end" in
        let (ctx, cond_asm, cond_reg) = gen_expr ctx cond in
        
        let load_cond = gen_load_spill cond_reg "t0" in
        let actual_cond_reg = if is_spill_reg cond_reg then "t0" else cond_reg in
        
        let loop_ctx = { ctx with 
            loop_stack = (begin_label, end_label) :: ctx.loop_stack } in
        let (ctx_after_body, body_asm) = gen_stmt loop_ctx body in
        
        (* 仅弹出循环栈，保留其他字段 *)
        let ctx_after_loop = { ctx_after_body with 
            loop_stack = List.tl ctx_after_body.loop_stack } in
        
        let asm = Printf.sprintf "%s:" begin_label ^
                cond_asm ^ "\n" ^ load_cond ^
                Printf.sprintf "\n    beqz %s, %s" actual_cond_reg end_label ^
                body_asm ^
                Printf.sprintf "\n    j %s" begin_label ^
                Printf.sprintf "\n%s:" end_label in
        (free_temp_reg ctx_after_loop, asm)
    
    | Break ->
        (match ctx.loop_stack with
        | (_, end_label)::_ -> 
            (ctx, Printf.sprintf "    j %s" end_label)
        | [] -> failwith "break outside loop")
    
    | Continue ->
        (match ctx.loop_stack with
        | (begin_label, _)::_ -> 
            (ctx, Printf.sprintf "    j %s" begin_label)
        | [] -> failwith "continue outside loop")
    
    | Return expr_opt ->
        let (ctx, expr_asm, _reg) = 
            match expr_opt with
            | Some expr -> 
                let (ctx, asm, r) = gen_expr ctx expr in
                let load_asm = gen_load_spill r "t0" in
                let actual_reg = if is_spill_reg r then "t0" else r in
                if actual_reg = "a0" then (ctx, asm ^ "\n" ^ load_asm, "a0")
                else (ctx, asm ^ "\n" ^ load_asm ^ Printf.sprintf "\n    mv a0, %s" actual_reg, "a0")
            | None -> (ctx, "", "a0")
        in
        (* 关键修复：在返回语句后直接跳转到函数结尾 *)
        let epilogue_asm = gen_epilogue ctx in
        (free_temp_reg ctx, expr_asm ^ "\n" ^ epilogue_asm)
    | EmptyStmt -> (ctx, "")
    | ExprStmt e -> 
        let (ctx, asm, _) = gen_expr ctx e in 
        (free_temp_reg ctx, asm)

(* 函数代码生成 *)
let gen_function func =
    let ctx = create_context func.name in
    (* 处理参数 *)
    let ctx =
        List.fold_left (fun ctx param ->
            let ctx = add_var ctx param 4 in
            { ctx with param_count = ctx.param_count + 1 }
        ) ctx func.params
    in
    
    (* 计算实际需要的栈帧大小(在知道所有变量后) *)
    let total_local_size = ctx.max_local_offset in
    let total_size = align_stack (ctx.saved_area_size + total_local_size) stack_align in
    let ctx = { ctx with frame_size = total_size } in
    
    (* 生成序言(使用实际大小) *)
    let prologue_asm =
        let save_regs_asm =
            let reg_saves = List.mapi (fun i reg ->
                Printf.sprintf "    sw %s, %d(sp)" reg (i * 4)
            ) ctx.saved_regs in
            let ra_save = Printf.sprintf "    sw ra, %d(sp)" (List.length ctx.saved_regs * 4) in
            String.concat "\n" (reg_saves @ [ra_save])
        in
        Printf.sprintf ".globl %s\n%s:\n    addi sp, sp, -%d\n%s"
            func.name func.name total_size save_regs_asm
    in
    
    (* 保存参数到局部变量区 - 关键修复:栈传递参数偏移量 *)
    let save_params_asm =
        let rec gen_save params index asm =
            match params with
            | [] -> asm
            | param::rest ->
                let offset = get_var_offset ctx param in
                if index < 8 then (
                    (* 寄存器参数 *)
                    let reg = Printf.sprintf "a%d" index in
                    gen_save rest (index + 1)
                        (asm ^ Printf.sprintf "    sw %s, %d(sp)\n" reg offset)
                ) else (
                    (* 关键修复:栈传递参数在调用者栈帧中，相对于当前sp的偏移为：(index-8)*4 *)
                    let stack_offset = (index - 8) * 4 in
                    let load_asm = Printf.sprintf "    lw t0, %d(sp)\n" stack_offset in
                    let store_asm = Printf.sprintf "    sw t0, %d(sp)" offset in
                    gen_save rest (index + 1)
                        (asm ^ load_asm ^ store_asm ^ "\n")
                )
        in
        gen_save func.params 0 ""
    in
    
    (* 生成函数体:直接处理语句列表(不额外添加作用域) *)
    let (_, body_asm) =
        match func.body with
        | Block stmts -> gen_stmts ctx stmts
        | _ -> gen_stmt ctx func.body
    in
    
    (* 生成函数结语 *)
    let epilogue_asm = gen_epilogue ctx in
    
    prologue_asm ^ "\n" ^ save_params_asm ^ body_asm ^ epilogue_asm
    
(* 编译单元代码生成 *)
let compile cu =
    let main_exists = ref false in
    let funcs_asm = List.map (fun func ->
        if func.name = "main" then main_exists := true;
        gen_function func
    ) cu in
    
    if not !main_exists then
        failwith "Missing main function";
    
    String.concat "\n\n" funcs_asm