
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

    if o4 == {
        case 0b1011; // immediate to register
            w := cast(bool, (byte >> 3) & 1);
            reg := byte & 0b111;
            d1 : int = next(instructions);
            d2 : int = ifx w then next(instructions);
            value := d1 + (d2 << 8);

            print("mov %, %\n", register_name(reg, w), value);
            return true;
    }

    if o6 == {
    case 0b100010; // MOV: Immediate to register/memory
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
            sign := ifx displacement < 0 "-" else "+";
            if d {
                if displacement {
                    print("mov %1, [%2 %4 %3]\n", register_name(reg, w), effective_address(rm), abs(displacement), sign);
                } else {
                    print("mov %1, [%2]\n", register_name(reg, w), effective_address(rm));
                }
            } else {
                if displacement {
                    print("mov [%2 %4 %3], %1\n", register_name(reg, w), effective_address(rm), abs(displacement), sign);
                } else {
                    print("mov [%2], %1\n", register_name(reg, w), effective_address(rm));
                }
            }
            return true;
        case 0b11; // Register mode, no displacement
            dest, src := ifx d then reg else rm, ifx !d then reg else rm;
            print("mov %, %\n", register_name(dest, w), register_name(src, w));
            return true;
        }
    }

    if o7 == {
    case 0b1100011; // MOV: Immediate to register/memory
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
    case 0b1010000; // MOV: Memory to accumulator
        w := cast(bool, byte & 1);
        memory := next2(instructions);
        destination := ifx w "ax" else "al";
        print("mov %, [%]\n", destination, memory);
        return true;
    case 0b1010001; // MOV: Accumulator to memory
        w := cast(bool, byte & 1);
        memory := next2(instructions);
        destination := ifx w "ax" else "al";
        print("mov [%], %\n", memory, destination);
        return true;
    }

    print("Unknown op code with 0b%\n", formatInt(byte, base=2, minimum_digits=8));

    return false;
}

main :: () {
    args := get_command_line_arguments();
    in: string;

    if args.count >= 2 {
        in = args[1];
    } else {
        print("Please provide a file to decode\n");
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
