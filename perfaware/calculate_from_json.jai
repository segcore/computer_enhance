// ---------------- Reference calculations
RADIUS :: 6372.8;
Square :: (A: float64) -> float64 {
    Result := A * A;
    return Result;
}

RadiansFromDegrees :: (Degrees: float64) -> float64 {
    Result := 0.01745329251994329577 * Degrees;
    return Result;
}

// NOTE(casey): EarthRadius is generally expected to be 6372.8
ReferenceHaversine :: (X0: float64, Y0: float64, X1: float64, Y1: float64, EarthRadius: float64) -> float64 {
    /* NOTE(casey): This is not meant to be a "good" way to calculate the Haversine distance.
       Instead, it attempts to follow, as closely as possible, the formula used in the real-world
       question on which these homework exercises are loosely based.
    */

    lat1 := Y0;
    lat2 := Y1;
    lon1 := X0;
    lon2 := X1;

    dLat := RadiansFromDegrees(lat2 - lat1);
    dLon := RadiansFromDegrees(lon2 - lon1);
    lat1 = RadiansFromDegrees(lat1);
    lat2 = RadiansFromDegrees(lat2);

    a := Square(sin(dLat/2.0)) + cos(lat1)*cos(lat2)*Square(sin(dLon/2));
    c := 2.0*asin(sqrt(a));

    Result := EarthRadius * c;

    return Result;
}
// ------------------------

LatLon :: struct {
    x: float64;
    y: float64;
};

Pair :: struct {
    a: LatLon;
    b: LatLon;
};

main :: () {
    Profile.estimate_cpu_frequency();
    Profile.begin();

    PROF: Profile.Region;

    PROF = Profile.block_begin("Args");
    args := get_command_line_arguments();
    input_file: string = "./data_10.json";
    answers_file: string = "./data_10.answers";
    if args.count > 1 {
        input_file = args[1];
    }
    answers_file = tprint("%.answers", basename(input_file));
    Profile.block_end(PROF);

    PROF = Profile.block_begin("Read input");
    input, ok := read_entire_file(input_file);
    if !ok {
        log_error("Failed to read input file '%'.\n", input_file);
        exit(1);
    }
    Profile.block_end(PROF, input.count);

    pairs:, ok = parse(input);
    defer array_free(pairs);
    if !ok {
        log_error("Failed to parse input from '%'", input_file);
        exit(1);
    }

    PROF = Profile.block_begin("Read answers file");
    answers_data:, ok= read_entire_file(answers_file);
    if !ok {
        log("Failed to read answers file %", answers_file);
    }
    answers_f64: []float64;
    if answers_data {
        answers_f64.data = xx answers_data.data;
        answers_f64.count = answers_data.count / size_of(float64);
        if answers_f64.count != pairs.count {
            log_error("Answers has % values, but pairs has %\n", answers_f64.count, pairs.count);
        }
    }
    Profile.block_end(PROF);

    PROF = Profile.block_begin("Haversine");
    sum: float64 = 0;
    count := 0;
    success_count := 0;
    for pair : pairs {
        result := ReferenceHaversine(pair.a.x, pair.a.y, pair.b.x, pair.b.y, RADIUS);
        sum += result;
        count += 1;

        Profile.block("Answer calculation");
        if answers_f64.count > it_index {
            reference := answers_f64[it_index];
            if values_are_close(reference, result) {
                success_count += 1;
            } else {
                log_error("Reference does not match result for pair %. Should be %, but got %", it_index, reference, result);
            }
        }
    }
    Profile.block_end(PROF, pairs.count*size_of(type_of(pairs.data.*)));

    average := sum / count;

    PROF = Profile.block_begin("Free");
    array_reset(*pairs);
    Profile.block_end(PROF);

    PROF = Profile.block_begin("Printing");
    print("----- Calculations -----\n");
    print("Input file size: % bytes\n", input.count);
    print("Average: %\n", average);
    print("Correct: %/%\n", success_count, count);
    write_string("\n");

    print("----- Timing -----\n");
    print("Estimated CPU frequency: %Hz  (% MHz)\n", Profile.cpu_frequency, Profile.cpu_frequency/1000_000);
    Profile.block_end(PROF);

    Profile.end_and_print();
}

// Remember to free the result!
parse :: (data: string) -> []Pair, ok: bool {
    Profile.function(data.count);

    results: [..]Pair;

    j: Lexer = json_init(data);
    defer json_free(*j);

    parse_pair :: (j: *Lexer, pair: *Pair) {
        level := j.stack.count;
        t: Token;
        while json_fetch_next_at_depth(j, *t, level+1) {
            if t.type == .KEY {
                destination: *float64;
                if t.str == {
                case "x1"; destination = *pair.a.x;
                case "y1"; destination = *pair.a.y;
                case "x2"; destination = *pair.b.x;
                case "y2"; destination = *pair.b.y;
                }
                if destination {
                    key_value := json_next(j);
                    if key_value.type == .NUMBER {
                        destination.* = key_value.num;
                    }
                }
            }
        }
    }

    while true {
        t := json_next(*j);
        if t.type == .NONE break;
        if t.type == .ARRAY_BEGIN {
            while json_fetch_until(*j, .ARRAY_END, *t) {
                if t.type == .OBJECT_BEGIN {
                    parse_pair(*j, array_add(*results));
                }
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
            Profile.block("json_next.parse_value");
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

json_fetch_until :: (j: *Lexer, type: TokenType, out_token: *Token) -> bool {
    t := json_next(j);
    out_token.* = t;
    if t.type == .NONE return false;
    if t.type == type  return false;
    return true;
}

json_fetch_next_at_depth :: (j: *Lexer, out_token: *Token, level: int) -> bool {
    t := json_next(j);
    while j.stack.count >= level {
        if level == j.stack.count {
            out_token.* = t;
            return true;
        }
        t = json_next(j);
    }
    out_token.* = t;
    return false;
}

#scope_file

parse_string :: (str: *string) -> string, ok: bool {
    Profile.function();
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

// Sloppy_Math only supports float, not float64, so implement it here.
values_are_close :: (f0: float64, f1: float64, epsilon := 0.0005) -> bool {
    return abs(f0-f1) <= epsilon;
}

#import "Basic";
#import "File";
#import "String";
#import "Math";
#import "Print_Vars";
Profile :: #import,file "Profile.jai"()(enable=true);
