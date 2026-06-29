RADIUS :: 6372.8;

main :: () {
    context.logger = stderr_logger;
    args := get_command_line_arguments();

    number_of_points: int = 10;

    if args.count > 1 {
        number_of_points = string_to_int(args[1]);
    }

    seed_time := current_time_consensus();
    seed: u64 = seed_time.low ^ xx seed_time.high;
    random_seed(seed);

    regions := [4]float32.[
        generate_area(),
        generate_area(),
    ];

    sum : float64 = 0;
    count := 0;
    answers: []float64 = NewArray(number_of_points, float64);

    sb: String_Builder;
    print(*sb, "{\n");
    print(*sb, "  \"positions\": [\n");
    for 1..number_of_points {
        x1, y1, x2, y2 := generate_latlons(regions[0], regions[1]);
        print(*sb, "    { \"x1\": %, \"y1\": %, \"x2\": %, \"y2\": % }", x1, y1, x2, y2);
        if it == number_of_points {
            print(*sb, "\n");
        } else {
            print(*sb, ",\n");
        }

        answer := ReferenceHaversine(x1, y1, x2, y2, RADIUS);
        answers[it - 1] = answer;
        sum += answer;
        count += 1;
    }
    print(*sb, "  ]\n");
    print(*sb, "}\n");

    print("Writing %\n", tprint("data_%.*", count));
    ok :=  write_entire_file(tprint("data_%.json", count), builder_to_string(*sb));
    ok &&= write_entire_file(tprint("data_%.answers", count), answers.data, answers.count * size_of(float64));

    average := sum / count;
    log("Seed = %\n", seed);
    log("Count = %\n", count);
    log("Average = %\n", average);
}

generate_latlons :: (region1: [4]float32, region2: [4]float32)
                    -> x1: float32, y1: float32, x2: float32, y2: float32 {
    return  random_get_within_range(region1[0], region1[2]),
            random_get_within_range(region1[1], region1[3]),
            random_get_within_range(region2[0], region2[2]),
            random_get_within_range(region2[1], region2[3]);
}

generate_area :: () -> [4]float32 {
    x := random_get_within_range(-170, 170);
    y := random_get_within_range(-80, 80);
    x2 := x + random_get_within_range(5, 10);
    y2 := y + random_get_within_range(5, 10);
    return .[x, y, x2, y2];
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

// ---------------- Reference calculations

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

#import "Basic";
#import "File";
#import "Random";
#import "Math";
