LatLon :: struct {
    x: float64;
    y: float64;
};

Pair :: struct {
    a: LatLon;
    b: LatLon;
};


main :: () {
    args := get_command_line_arguments();

    input_file: string = "./data_10.json";
    if args.count > 1 {
        input_file = args[1];
    }

    input, ok := read_entire_file(input_file);
    if !ok {
        log_error("Failed to read input file '%'.\n", input_file);
        exit(1);
    }

    pairs:, ok = parse(input);
    defer array_free(pairs);
    if !ok {
        log_error("Failed to parse input from '%'", input_file);
        exit(1);
    }

    print("Found pairs: %\n", pairs.count);
}

// Remember to free the result!
parse :: (data: string) -> []Pair, ok: bool {
    results: [..]Pair;

    j: Lexer = json_init(data);
    defer json_free(*j);

    state: enum {
        NONE;
        POSITIONS;
        PAIR;
    } = .NONE;

    while true {
        t := json_next(*j);
        if t.type == .NONE break;
        else if t.type == .KEY && state == .NONE && t.str == "positions" {
            state = .POSITIONS;
        } else if t.type == .OBJECT_BEGIN && state == .POSITIONS {
            state = .PAIR;
            array_add(*results);
        } else if t.type == .OBJECT_END && state == .PAIR {
            state = .POSITIONS;
        } else if t.type == .NUMBER && state == .PAIR {
            key_token := j.stack[j.stack.count - 2];
            if key_token.str == {
            case "x1"; peek_pointer(results).a.x = t.num;
            case "x2"; peek_pointer(results).b.x = t.num;
            case "y1"; peek_pointer(results).a.y = t.num;
            case "y2"; peek_pointer(results).b.y = t.num;
            }
        }
    }

    return results, true;
}

TokenType :: enum {
    NONE; // also EOF
    OBJECT_BEGIN;
    OBJECT_END;
    ARRAY_BEGIN;
    ARRAY_END;
    KEY;
    NUMBER;
    STRING;
}

Token :: struct {
    type: TokenType;
    str: string;
    num: float64;
}

Lexer :: struct {
    stack: [..]Token;
    line: int;
    remaining: string;
    state: LexerState;
}

LexerState :: enum {
    parse_value;
    parse_key;
    parse_key_separator;
    parse_key_value;
    parse_array_value;
    value_done;
}


json_init :: (data: string) -> Lexer {
    j: Lexer;
    j.remaining = data;
    return j;
}

json_free :: (j: *Lexer) {
    array_reset(*j.stack);
}

json_next :: (j: *Lexer) -> Token {
    while true {
        j.remaining = skip_whitespace(j.remaining);
        if !j.remaining return .{};

        if #complete j.state == {
        case .parse_key_value; #through;
        case .parse_array_value; #through;
        case .parse_value;
            c := j.remaining[0];
            if c == {
            case "{";
                advance(*j.remaining);
                array_add(*j.stack, .{type=.OBJECT_BEGIN});
                j.state = .parse_key;
                return peek(j.stack);
            case "}";
                advance(*j.remaining);
                if peek_or_default(j.stack).type != .OBJECT_BEGIN {
                    log_error("Close of object while not at an object end");
                    return .{};
                }
                t := peek(j.stack);
                t.type = .OBJECT_END;
                j.state = .value_done;
                return t;
            case "[";
                advance(*j.remaining);
                array_add(*j.stack, .{type=.ARRAY_BEGIN});
                j.state = .parse_array_value;
                return peek(j.stack);
            case "]";
                advance(*j.remaining);
                if peek_or_default(j.stack).type != .ARRAY_BEGIN {
                    log_error("Close of array while not at an array end");
                    return .{};
                }
                t := peek(j.stack);
                t.type = .ARRAY_END;
                j.state = .value_done;
                return t;
            case;
                len := 0;
                while len < j.remaining.count {
                    c = j.remaining[len];
                    if is_digit(c) || c == "-" || c == "+" || c == "." || c == "e" || c == "E" {
                        len += 1;
                    } else break;
                }
                if len > 0 {
                    digits := slice(j.remaining, 0, len);
                    advance(*j.remaining, len);
                    value, ok := parse_float(*digits);
                    if !ok {
                        log_error("Failed to parse float at %", slice(j.remaining, 0, 20));
                        return .{};
                    }
                    j.state = .value_done;
                    array_add(*j.stack, .{type=.NUMBER, num=value});
                    return peek(j.stack);
                }

                // Reaching here means unknown. so error
                log_error("Unable to decode JSON value: %\n", slice(j.remaining, 0, 20));
                return .{};
            }
        case .parse_key;
            str, ok := parse_string(*j.remaining);
            if !ok {
                return .{};
            }
            array_add(*j.stack, .{type=.KEY, str=str});
            j.state = .parse_key_separator;
            return peek(j.stack);
        case .parse_key_separator;
            if j.remaining[0] != ":" {
                log_error("No : separating the key from value at %", slice(j.remaining, 0, 20));
            }
            advance(*j.remaining);
            j.state = .parse_key_value;
        case .value_done;
            assert(j.stack.count > 0);
            j.stack.count -= 1;
            if j.stack && peek(j.stack).type == .KEY {
                j.stack.count -= 1;
            }

            t := peek_or_default(j.stack);
            if j.remaining[0] == #char "," {
                if t.type == .OBJECT_BEGIN {
                    advance(*j.remaining);
                    j.state = .parse_key;
                } else if t.type == .ARRAY_BEGIN {
                    advance(*j.remaining);
                    j.state = .parse_array_value;
                } else {
                    log_error(", not in array or object at %", slice(j.remaining, 0, 20));
                    return .{};
                }
            } else if j.remaining[0] == #char "}" {
                if t.type == .OBJECT_BEGIN {
                    advance(*j.remaining);
                    t.type = .OBJECT_END;
                    j.state = .value_done;
                    return t;
                } else {
                    log_error("Closing brace not in an object at %", slice(j.remaining, 0, 20));
                    return .{};
                }
            } else if j.remaining[0] == #char "]" {
                if t.type == .ARRAY_BEGIN {
                    advance(*j.remaining);
                    t.type = .ARRAY_END;
                    j.state = .value_done;
                    return t;
                } else {
                    log_error("Closing brace not in an object at %", slice(j.remaining, 0, 20));
                    return .{};
                }
            } else {
                log_error("Unexpected token starting at %", slice(j.remaining, 0, 20));
                return .{};
            }
        }
    }
    assert(false, "Bad fallthrough");
    return .{};
}

parse_string :: (str: *string) -> string, ok: bool {
    assert(str.*[0] == #char "\"", tprint("str should start with a \" but it is: %", slice(str.*, 0, 10)));
    i := 1;
    while true {
        if i >= str.count {
            log_error("No end-of-string found\n");
            return "", false;
        }
        c := str.*[i];
        if c == "\\" {
            i += 2;
            continue;
        }
        i += 1;
        if c == "\"" {
            value := slice(str.*, 1, i - 2);
            advance(str, i);
            return value, true;
        }
    }
    assert(false, "bad fallthrough");
    return "", false;
}

#scope_file

skip_whitespace :: (_s: string) -> string {
    s := _s;
    while s.count && is_space(s.data[0]) {
        s.data += 1;
        s.count -= 1;
    }
    return s;
}

peek_or_default :: (arr: []$T) -> T {
    if arr {
        return arr[arr.count - 1];
    }
    default: T;
    return default;
}

#import "Basic";
#import "File";
#import "String";
