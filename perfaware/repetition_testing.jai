#import "Basic";
#import "File";
#import "Machine_X64";
#import "POSIX";
#import "Math";
Profile :: #import,file "Profile.jai"()(enable=true);

Read_Parameters :: struct {
    buffer: []u8;
    file_name: string;
}

Tester_State :: enum {
    NONE;
    TESTING;
    DONE;
    ERROR;
}

Repetition_Tester :: struct {
    target_byte_count: s64;
    cpu_timer_frequency: u64;
    try_for_time: u64;
    tests_started_at: u64;

    state: Tester_State;
    do_print: bool;
    open_count: u64;
    close_count: u64;
    time_accumulated_on_this_test: u64;
    bytes_accumulated_on_this_test: s64;

    results: Repetition_Test_Results;
}

Repetition_Test_Results :: struct {
    test_count: u64;
    total_time: u64;
    max_time: u64;
    min_time: u64;
}

NewTestWave :: (tester: *Repetition_Tester, target_byte_count: s64, cpu_timer_freq: u64, secs_to_try: u64 = 5) {
    if tester.state == .NONE {
        // Initialise
        tester.state = .TESTING;
        tester.target_byte_count = target_byte_count;
        tester.cpu_timer_frequency = cpu_timer_freq;
        tester.do_print = true;
        tester.results.min_time = U64_MAX;
    } else if tester.state == .DONE {
        // Verify correct values, then re-initialise
        tester.state = .TESTING;
        if tester.target_byte_count != target_byte_count {
            Error(tester, "target byte count mismatch. Got %, expected %", tester.target_byte_count, target_byte_count);
        }
        assert(tester.cpu_timer_frequency == cpu_timer_freq, "CPU timer frequency changed from % to %", tester.cpu_timer_frequency, cpu_timer_freq);
    }

    tester.try_for_time = secs_to_try * cpu_timer_freq;
    tester.tests_started_at = rdtsc();
}

BeginTime :: (tester: *Repetition_Tester) {
    tester.open_count += 1;
    tester.time_accumulated_on_this_test -= rdtsc();
}
EndTime :: (tester: *Repetition_Tester) {
    tester.close_count += 1;
    tester.time_accumulated_on_this_test += rdtsc();
}
IsTesting :: (tester: *Repetition_Tester) -> bool {
    if tester.state == .TESTING {
        now := rdtsc();
        if tester.open_count {
            if tester.open_count != tester.close_count {
                Error(tester, "Opened % but closed % - bad.", tester.open_count, tester.close_count);
            }
            if tester.bytes_accumulated_on_this_test != tester.target_byte_count {
                Error(tester, "Bytes mismatch. Got %, expected %", tester.bytes_accumulated_on_this_test, tester.target_byte_count);
            }

            // Error may adjust the state since last check
            if tester.state == .TESTING {
                results := *tester.results;
                elapsed_time := tester.time_accumulated_on_this_test;
                results.test_count += 1;
                results.total_time += elapsed_time;
                if elapsed_time > results.max_time  results.max_time = elapsed_time;
                if elapsed_time < results.min_time {
                    results.min_time = elapsed_time;
                    // Reset timer to attempt to get new better times
                    tester.tests_started_at = now;

                    if tester.do_print {
                        PrintTime("Min", results.min_time, tester.cpu_timer_frequency, tester.bytes_accumulated_on_this_test);
                        print("                 \r");
                    }
                }

                tester.open_count = 0;
                tester.close_count = 0;
                tester.time_accumulated_on_this_test = 0;
                tester.bytes_accumulated_on_this_test = 0;

                if now - tester.tests_started_at > tester.try_for_time {
                    tester.state = .DONE;
                    print("                                                           \r");
                    PrintResults(tester.results, tester.cpu_timer_frequency, tester.target_byte_count);
                }
            }
        }
    }

    return tester.state == .TESTING;
}
CountBytes :: (tester: *Repetition_Tester, count: s64) {
    tester.bytes_accumulated_on_this_test += count;
}
Error :: (tester: *Repetition_Tester, format: string, args: ..Any) {
    log_error(format, ..args);
    tester.state = .ERROR;
} @PrintLike

PrintTime :: (label: string, tsc: u64, cpu_timer_freq: u64, byte_count: int, newline := false) {
    time_secs := tsc.(float64) / cpu_timer_freq;
    gbps := byte_count.(float64) / 1.0e9 / time_secs;
    print("%  %ms  (% Gb/s)", label, formatFloat(time_secs*1000, trailing_width=1, zero_removal=.NO), gbps);
    if newline write_string("\n");
}
PrintResults :: (results: Repetition_Test_Results, cpu_timer_frequency: u64, target_byte_count: int) {
    average_time := results.total_time / results.test_count;
    PrintTime("Min", results.min_time, cpu_timer_frequency, target_byte_count, newline=true);
    PrintTime("Avg", average_time,     cpu_timer_frequency, target_byte_count, newline=true);
    PrintTime("Max", results.max_time, cpu_timer_frequency, target_byte_count, newline=true);
}

read_with_posix_read :: (tester: *Repetition_Tester, params: Read_Parameters) {
    while IsTesting(tester) {
        assert(params.file_name.data[params.file_name.count] == 0, "File name is not null terminated!");
        fd := open(params.file_name.data, O_RDONLY);
        if fd >= 0 {
            buffer := params.buffer;

            remaining := buffer;
            while remaining {
                BeginTime(tester);
                result := read(fd, remaining.data, xx remaining.count);
                EndTime(tester);

                if result < 0 {
                    Error(tester, "posix read failed");
                    break;
                } else {
                    CountBytes(tester, result);
                    remaining = array_view(remaining, result);
                }
            }

            close(fd);
        } else {
            Error(tester, "fopen failed");
        }
    }
}
read_with_fread :: (tester: *Repetition_Tester, params: Read_Parameters) {
    while IsTesting(tester) {
        assert(params.file_name.data[params.file_name.count] == 0, "File name is not null terminated!");
        fp := fopen(params.file_name.data, "rb");
        if fp {
            buffer := params.buffer;

            BeginTime(tester);
            result := fread(buffer.data, xx buffer.count, 1, fp);
            EndTime(tester);

            if result == 1 {
                CountBytes(tester, buffer.count);
            } else {
                Error(tester, "fread failed");
            }

            fclose(fp);
        } else {
            Error(tester, "fopen failed");
        }
    }
}

TestFunction :: struct {
    name: string;
    function: (tester: *Repetition_Tester, params: Read_Parameters);
}
test_functions :: TestFunction.[
    .{ "posix_read", read_with_posix_read },
    .{ "fread", read_with_fread },
];

main :: () {
    args := get_command_line_arguments();
    if args.count < 2 {
        print("Please provide a file to read\n");
        // exit(1);
        args = .[args[0], "data_1000000.json"];
    }
    path := args[1];

    stats: stat_t;
    if stat(path.data, *stats) != 0 {
        print("stat failed on %\n", path);
        exit(1);
    }
    file_length := stats.st_size;

    params: Read_Parameters;
    params.buffer = NewArray(file_length, u8, initialized=false);
    params.file_name = path;

    cpu_timer_frequency := Profile.estimate_cpu_frequency();
    testers: [test_functions.count]Repetition_Tester;
    while true {
        for* tester: testers {
            test_function := test_functions[it_index];

            print("\n--- % ---\n", test_function.name);
            NewTestWave(tester, params.buffer.count, cpu_timer_frequency);
            test_function.function(tester, params);
        }
    }
}
