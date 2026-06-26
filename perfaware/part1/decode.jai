
register_name :: (reg: u8, wide: bool) -> string {
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

effective_address :: (rm: u8) -> string {
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

advance :: (view: *[]u8, count := 1) {
    view.* = array_view(view.*, count);
}

next :: (view: *[]u8) -> u8 {
    value := view.*[0];
    advance(view, 1);
    return value;
}

decode_op :: (instructions: *[]u8) -> ok: bool {
    if instructions.count == 0 return false;

    byte := next(instructions);
    o4 := byte >> 4;
    o6 := byte >> 2;

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
    case 0b100010; // Immediate to register/memory
        d := cast(bool, (byte >> 1) & 1); // Direction; 1=destination is the REG field.
        w := cast(bool, byte & 1); // Wide/word if true. Otherwise a byte.

        b2 := next(instructions);
        rm := b2 & 0b111;
        reg := (b2 >> 3) & 0b111;
        mod := (b2 >> 6);

        displacement, d1, d2: int;
        if mod == {
            case 0b00; // Memory mode, no displacement* (unless rm==0b110)
                if rm == 0b110 { // DIRECT ADDRESS
                    d1 = next(instructions);
                    d2 = next(instructions);
                    direct_address := d1 + (d2 << 8);
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
                if mod >= 0b01 d1 = next(instructions);
                if mod >= 0b10 d2 = next(instructions);
                displacement = d1 + (d2 << 8);
                if d {
                    if displacement {
                        print("mov %1, [%2 + %3]\n", register_name(reg, w), effective_address(rm), displacement);
                    } else {
                        print("mov %1, [%2]\n", register_name(reg, w), effective_address(rm));
                    }
                } else {
                    if displacement {
                        print("mov [%2 + %3], %1\n", register_name(reg, w), effective_address(rm), displacement);
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
