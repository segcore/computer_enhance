binop_to_string :: (binop: int) -> string {
    operation: string;
    if binop == {
        case 0b000; operation = "add";
        case 0b101; operation = "sub";
        case 0b111; operation = "cmp";
        case; log("unknown operation with code %\n", binop);
    }
    return operation;
}

register_name :: (reg: int, wide: bool) -> string {
    if wide == false {
        if reg == {
            case 0b000; return "al";
            case 0b001; return "cl";
            case 0b010; return "dl";
            case 0b011; return "bl";
            case 0b100; return "ah";
            case 0b101; return "ch";
            case 0b110; return "dh";
            case 0b111; return "bh";
        }
    } else {
        if reg == {
            case 0b000; return "ax";
            case 0b001; return "cx";
            case 0b010; return "dx";
            case 0b011; return "bx";
            case 0b100; return "sp";
            case 0b101; return "bp";
            case 0b110; return "si";
            case 0b111; return "di";
        }
    }
    assert(false, "Invalid register");
    return "???";
}

effective_address :: (rm: int) -> string {
    if rm == {
        case 0b000; return "bx + si";
        case 0b001; return "bx + di";
        case 0b010; return "bp + si";
        case 0b011; return "bp + di";
        case 0b100; return "si";
        case 0b101; return "di";
        case 0b110; return "bp";
        case 0b111; return "bx";
    }
    assert(false, "Invalid rm value");
    return "???";
}

print_effective_address :: (sb: *String_Builder, rm: int, displacement: int) {
    append(sb, "[");
    append(sb, effective_address(rm));
    if displacement {
        append(sb, ifx displacement < 0 " - " else " + ");
        print_integer(sb, formatInt(abs(displacement)));
    }
    append(sb, "]");
}

print_register_or_memory :: (sb: *String_Builder, rm: int, mod: int, wide: bool, displacement: int) {
    if mod == 0b11 {
        append(sb, register_name(rm, wide));
    } else if mod == 0 && rm == 0b110 {
        append(sb, "[");
        print_integer(sb, formatInt(displacement));
        append(sb, "]");
    } else {
        print_effective_address(sb, rm, displacement);
    }
}

print_reg_memory_args :: (sb: *String_Builder, d: bool, w: bool, mod: int, reg: int, rm: int, displacement: int) {
    if d {
        print(sb, register_name(reg, w));
        print(sb, ", ");
        print_register_or_memory(sb, rm, mod, w, displacement);
    } else {
        print_register_or_memory(sb, rm, mod, w, displacement);
        print(sb, ", ");
        print(sb, register_name(reg, w));
    }
}

sign_extend_s8 :: (value: *int) {
    x := cast,no_check(s8, value.*);
    value.* = x; // conversion back to int should sign extend for us
}

pull_displacement :: (instructions: *[]u8, mod: int, rm: int) -> displacement: int {
    if mod == {
    case 0b00;
        if rm == 0b110 {
            return next2(instructions);
        }
        return 0;
    case 0b01;
        displacement: int = next(instructions, true);
        return displacement;
    case 0b10;
        displacement: int = next2(instructions);
        return displacement;
    case 0b11;
        return 0;
    }
    assert(false, "Invalid mod");
    return 0;
}

advance :: (view: *[]u8, count := 1) {
    view.* = array_view(view.*, count);
}

next :: (view: *[]u8, signed: bool = false) -> int {
    value : int = view.*[0];
    advance(view, 1);
    if signed sign_extend_s8(*value);
    return value;
}

next2 :: (view: *[]u8) -> int {
    d1 := next(view);
    d2 := next(view);
    return d1 + (d2 << 8);
}

decode_op :: (instructions: *[]u8) -> ok: bool {
    if instructions.count == 0 return false;

    byte := next(instructions);
    o4 := byte >> 4;
    o6 := byte >> 2;
    o7 := byte >> 1;
    o8 := byte;
    sb: String_Builder;
    defer {
        assert(builder_string_length(*sb) == 0, "Returned without using up the string builder!");
    }

    log("; opcode = %\n", formatInt(byte, 2, 8));

    // ----- MOVs -----
    if o4 == 0b1011 { // MOV: immediate to register
        w := cast(bool, (byte >> 3) & 1);
        reg := byte & 0b111;
        d1 : int = next(instructions);
        d2 : int = ifx w then next(instructions);
        value := d1 + (d2 << 8);

        print("mov %, %\n", register_name(reg, w), value);
        return true;
    }

    if o6 == 0b100010 { // MOV: Immediate to register/memory
        d := cast(bool, (byte >> 1) & 1); // Direction; 1=destination is the REG field.
        w := cast(bool, byte & 1); // Wide/word if true. Otherwise a byte.

        b2 := next(instructions);
        rm := b2 & 0b111;
        reg := (b2 >> 3) & 0b111;
        mod := (b2 >> 6);

        displacement := pull_displacement(instructions, mod, rm);
        if mod == {
        case 0b00; // Memory mode, no displacement* (unless rm==0b110)
            if rm == 0b110 { // DIRECT ADDRESS
                direct_address := displacement;
                if d {
                    print("mov %1, [%2]\n", register_name(reg, w), direct_address);
                } else {
                    print("mov [%2], %1\n", register_name(reg, w), direct_address);
                }
                return true;
            }
            #through;
        case 0b01; #through; // 8 bit displacement
        case 0b10; // 16 bit displacement
            print(*sb, "mov ");
            print_reg_memory_args(*sb, d, w, mod, reg, rm, displacement);
            print("%\n", builder_to_string(*sb));
            return true;
        case 0b11; // Register mode, no displacement
            dest, src := ifx d then reg else rm, ifx !d then reg else rm;
            print("mov %, %\n", register_name(dest, w), register_name(src, w));
            return true;
        }
    }
    if o7 == 0b1100011 { // MOV: Immediate to register/memory
        w := cast(bool, byte & 1);
        b2 := next(instructions);

        rm := (b2 & 0b111);
        subcode := (b2 >> 3) & 0b111;
        mod := (b2 >> 6) & 0b11;

        if subcode == 0b000 {
            displacement := pull_displacement(instructions, mod, rm);
            immediate: int = ifx w then next2(instructions) else next(instructions, true);

            size := ifx w then "word" else "byte";
            if displacement {
                sign := ifx displacement < 0 "-" else "+";
                print("mov [% % %], % %\n", effective_address(rm), sign, abs(displacement), size, immediate);
            } else {
                print("mov [%], % %\n", effective_address(rm), size, immediate);
            }
            return true;
        } else {
            log("Unknown subcode %\n", subcode);
        }
    }
    if o7 == 0b1010000 { // MOV: Memory to accumulator
        w := cast(bool, byte & 1);
        memory := next2(instructions);
        destination := ifx w "ax" else "al";
        print("mov %, [%]\n", destination, memory);
        return true;
    }
    if o7 == 0b1010001 { // MOV: Accumulator to memory
        w := cast(bool, byte & 1);
        memory := next2(instructions);
        destination := ifx w "ax" else "al";
        print("mov [%], %\n", memory, destination);
        return true;
    }


    // ---------------
    // ----- BINOP: Add/Sub/Cmp -----
    // ---------------
    if o6 == 0b000000 || o6 == 0b001010 || o6 == 0b001110 { // BINOP: Reg/memory with register (either direction)
        d := cast(bool, (byte >> 1) & 1);
        w := cast(bool, byte & 1);

        b2 := next(instructions);
        rm := b2 & 0b111;
        reg := (b2 >> 3) & 0b111;
        mod := (b2 >> 6);
        displacement := pull_displacement(instructions, mod, rm);

        operation := binop_to_string((o6 >> 1) & 0b111);
        print(*sb, "% ", operation);
        print_reg_memory_args(*sb, d, w, mod, reg, rm, displacement);
        print("%\n", builder_to_string(*sb));
        return true;
    }
    if o6 == 0b1000_00 { // BINOP: Immediate to register or memory
        s := cast(bool, (byte >> 1) & 1);
        w := cast(bool, byte & 1);

        b2 := next(instructions);
        rm := b2 & 0b111;
        binop := (b2 >> 3) & 0b111;
        mod := (b2 >> 6);

        operation := binop_to_string(binop);
        if operation {
            displacement := pull_displacement(instructions, mod, rm);
            immediate: int;
           if !s && w {
               immediate = next2(instructions);
           } else {
               immediate = next(instructions, true);
           }

            print(*sb, "% ", operation);
            // may need 'byte' or 'word'
            print_register_or_memory(*sb, rm, mod, w, displacement);
            append(*sb, ", ");
            print_integer(*sb, formatInt(immediate));
            print("%\n", builder_to_string(*sb));
            return true;
        }
    }
    if o7 == 0b0000_010 || o7 == 0b0010110 { // : Immediate to accumulator
        w := cast(bool, byte & 1);
        value := ifx w next2(instructions) else next(instructions, true);
        register := ifx w "ax" else "al";
        operation := binop_to_string((o7 >> 2) & 0b111);
        print("% %, %\n", operation, register, value);
        return true;
    }
    if o7 == 0b0011110 { // CMP: immediate with accumulator
        w := cast(bool, byte & 1);
        value := ifx w next2(instructions) else next(instructions, true);
        register := ifx w "ax" else "al";
        print("cmp %, %\n", register, value);
        return true;
    }


    // ---------------
    // ---- Jumps ----
    // ---------------
    jumps :: struct { code: int; name: string; }.[
        .{ 0b01110100, "jz" },
        .{ 0b01111100, "jl" },
        .{ 0b01111110, "jle" },
        .{ 0b01110010, "jb" },
        .{ 0b01110110, "jbe" },
        .{ 0b01111010, "jp" },
        .{ 0b01110000, "jo" },
        .{ 0b01111000, "js" },
        .{ 0b01110101, "jnz" },
        .{ 0b01111101, "jge" },
        .{ 0b01111111, "jg" },
        .{ 0b01110011, "jae" },
        .{ 0b01110111, "ja" },
        .{ 0b01111011, "jpo" },
        .{ 0b01110001, "jno" },
        .{ 0b01111001, "jns" },
        .{ 0b11100010, "loop" },
        .{ 0b11100001, "loope" },
        .{ 0b11100000, "loopne" },
        .{ 0b11100011, "jcxz" },
    ];
    for j: jumps {
        if o8 == j.code {
            value := next(instructions, true);
            print("% label_%\n", j.name, value);
            return true;
        }
    }


    // ---------------
    // ---------------

    print("Unknown op code with 0b%\n", formatInt(byte, base=2, minimum_digits=8));
    return false;
}

stderr_logger :: (message: string, data: *void, info: Log_Info) {
    if !message then return;
    to_standard_error := true;
    if message[message.count-1] != #char "\n" {
        write_strings(message, "\n", to_standard_error = to_standard_error);
    } else {
        write_string(message, to_standard_error = to_standard_error);
    }
}

main :: () {
    context.logger = stderr_logger;
    args := get_command_line_arguments();
    in: string;

    if args.count >= 2 {
        in = args[1];
    } else {
        print("Please provide a file to decode\n");
        // in = "./listing_0041_add_sub_cmp_jnz";
        exit(1);
    }

    contents : []u8 = xx read_entire_file(in);
    if !contents {
        print("Empty or missing file '%'. Exiting.\n", in);
        exit(2);
    }

    print("bits 16\n");
    remaining := contents;
    while decode_op(*remaining) {}

    if remaining {
        print("--- error, % bytes not decoded\n", remaining.count);
        exit(1);
    }
}

#import "Basic";
#import "File";
#import "Math";
