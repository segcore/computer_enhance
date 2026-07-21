#import "Basic";
#import "File";
#import "Machine_X64";
#import "POSIX";
#import "Math";
Profile :: #import,file "Profile.jai"()(enable=true);

AllocType :: enum {
    None;
    NewArray;
    NewArrayInitialised;
}

Read_Parameters :: struct {
    buffer: []u8;
    file_name: string;
    alloc_type: AllocType;
}

Tester_State :: enum {
    NONE;
    TESTING;
    DONE;
    ERROR;
}

Repetition_Tester :: struct {
    target_byte_count: u64;
    cpu_timer_frequency: u64;
    try_for_time: u64;
    tests_started_at: u64;

    state: Tester_State;
    do_print: bool;
    open_count: u64;
    close_count: u64;
    accumulated_on_this_test: Repetition_Value;

    results: Repetition_Test_Results;
}

Repetition_Value_Type :: enum {
    TestCount;
    CPUTimer;
    MemPageFaults;
    ByteCount;
}
ET :: Repetition_Value_Type;
ET_Max :: #run type_info(Repetition_Value_Type).names.count;

Repetition_Value :: struct {
    E: [ET_Max]u64;
}

Repetition_Test_Results :: struct {
    total: Repetition_Value;
    min: Repetition_Value;
    max: Repetition_Value;
}



ReadPageFaultCount :: () -> u64 {
    usage: rusage;
    if getrusage(RUSAGE.SELF, *usage) == -1 {
        log_error("Failed to read page fault count");
    }
    faults := usage.ru_minflt + usage.ru_majflt;
    return xx faults;
}


NewTestWave :: (tester: *Repetition_Tester, target_byte_count: u64, cpu_timer_freq: u64, secs_to_try: u64 = 5) {
    if tester.state == .NONE {
        // Initialise
        tester.state = .TESTING;
        tester.target_byte_count = target_byte_count;
        tester.cpu_timer_frequency = cpu_timer_freq;
        tester.do_print = true;
        tester.results.min.E[ET.CPUTimer] = U64_MAX;
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
    tester.accumulated_on_this_test.E[ET.CPUTimer] -= rdtsc();
    tester.accumulated_on_this_test.E[ET.MemPageFaults] -= ReadPageFaultCount();
}
EndTime :: (tester: *Repetition_Tester) {
    tester.close_count += 1;
    tester.accumulated_on_this_test.E[ET.CPUTimer] += rdtsc();
    tester.accumulated_on_this_test.E[ET.MemPageFaults] += ReadPageFaultCount();
}
IsTesting :: (tester: *Repetition_Tester) -> bool {
    if tester.state == .TESTING {
        now := rdtsc();
        if tester.open_count {
            if tester.open_count != tester.close_count {
                Error(tester, "Opened % but closed % - bad.", tester.open_count, tester.close_count);
            }

            accumulator := *tester.accumulated_on_this_test;
            if accumulator.E[ET.ByteCount] != tester.target_byte_count {
                Error(tester, "Bytes mismatch. Got %, expected %", accumulator.E[ET.ByteCount], tester.target_byte_count);
            }

            // Error may adjust the state since last check
            if tester.state == .TESTING {
                results := *tester.results;
                accumulator.E[ET.TestCount] = 1;
                for 0..ET_Max-1 {
                    results.total.E[it] += accumulator.E[it];
                }

                elapsed_time := accumulator.E[ET.CPUTimer];
                if elapsed_time > results.max.E[ET.CPUTimer] then results.max = accumulator.*;
                if elapsed_time < results.min.E[ET.CPUTimer] {
                    results.min = accumulator.*;
                    // Reset timer to attempt to get new better times
                    tester.tests_started_at = now;

                    if tester.do_print {
                        PrintValue("Min", results.min, tester.cpu_timer_frequency);
                        print("                                                             \r");
                    }
                }

                tester.open_count = 0;
                tester.close_count = 0;
                accumulator.* = .{};

                if now - tester.tests_started_at > tester.try_for_time {
                    tester.state = .DONE;
                    print("                                                                           \r");
                    PrintResults(tester.results, tester.cpu_timer_frequency, tester.target_byte_count);
                }
            }
        }
    }

    return tester.state == .TESTING;
}
CountBytes :: (tester: *Repetition_Tester, count: u64) {
    tester.accumulated_on_this_test.E[ET.ByteCount] += count;
}
Error :: (tester: *Repetition_Tester, format: string, args: ..Any) {
    log_error(format, ..args);
    tester.state = .ERROR;
} @PrintLike

PrintValue :: (label: string, value: Repetition_Value, cpu_timer_freq: u64, newline := false) {
    test_count : float64 = xx value.E[ET.TestCount];
    if test_count == 0 test_count = 1.0;

    E: [ET_Max]float64;
    for 0..ET_Max-1 {
        E[it] = value.E[it].(float64) / test_count;
    }

    write_string(label);

    cpu_time := E[ET.CPUTimer];
    if cpu_timer_freq {
        time_secs := cpu_time / cpu_timer_freq;
        print("  %ms", formatFloat(time_secs*1000, trailing_width=1, zero_removal=.NO));

        byte_count := E[ET.ByteCount];
        if byte_count {
            gbps := byte_count / 1.0e9 / time_secs;
            print("  (% Gb/s)", gbps);
        }
    }

    if E[ET.MemPageFaults] {
        print("  % faults (%k/fault)", E[ET.MemPageFaults], formatFloat(E[ET.ByteCount] / 1000 / E[ET.MemPageFaults], trailing_width=1));
    }

    if newline write_string("\n");
}

PrintResults :: (results: Repetition_Test_Results, cpu_timer_frequency: u64, target_byte_count: u64) {
    average_time := results.total.E[ET.CPUTimer].(float64) / results.total.E[ET.TestCount];
    PrintValue("Min", results.min, cpu_timer_frequency, newline=true);
    PrintValue("Max", results.max, cpu_timer_frequency, newline=true);
    PrintValue("Avg", results.total, cpu_timer_frequency, newline=true);
}


MaybeAllocate :: (params: Read_Parameters, buffer: *[]u8) {
    if #complete params.alloc_type == {
    case .None;
    case .NewArray;
        buffer.* = NewArray(buffer.count, u8, initialized=false);
    case .NewArrayInitialised;
        buffer.* = NewArray(buffer.count, u8, initialized=true);
    }
}

MaybeDeallocate :: (params: Read_Parameters, buffer: *[]u8) {
    if #complete params.alloc_type == {
    case .None;
    case .NewArray; #through;
    case .NewArrayInitialised;
        array_reset(buffer);
    }
}

read_with_posix_read :: (tester: *Repetition_Tester, params: Read_Parameters) {
    while IsTesting(tester) {
        assert(params.file_name.data[params.file_name.count] == 0, "File name is not null terminated!");
        fd := open(params.file_name.data, O_RDONLY);
        if fd >= 0 {
            buffer := params.buffer;
            MaybeAllocate(params, *buffer);

            remaining := buffer;
            while remaining {
                BeginTime(tester);
                result := read(fd, remaining.data, xx remaining.count);
                EndTime(tester);

                if result < 0 {
                    Error(tester, "posix read failed");
                    break;
                } else {
                    CountBytes(tester, xx result);
                    remaining = array_view(remaining, result);
                }
            }

            MaybeDeallocate(params, *buffer);
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
            MaybeAllocate(params, *buffer);

            BeginTime(tester);
            result := fread(buffer.data, xx buffer.count, 1, fp);
            EndTime(tester);

            if result == 1 {
                CountBytes(tester, xx buffer.count);
            } else {
                Error(tester, "fread failed");
            }

            MaybeDeallocate(params, *buffer);
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
    testers: [test_functions.count][#run type_info(AllocType).names.count]Repetition_Tester;
    while true {
        for* func_array, func_index: testers {
            for alloc_type: enum_values_as_s64(AllocType) {
                params.alloc_type = xx alloc_type;
                tester := *func_array.*[alloc_type];
                test_function := test_functions[func_index];

                print("\n--- %0 + %0 ---\n", cast(AllocType, alloc_type), test_function.name);
                NewTestWave(tester, xx params.buffer.count, cpu_timer_frequency);
                test_function.function(tester, params);
            }
        }
    }
}
