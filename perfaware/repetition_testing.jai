#import "Basic";
#import "File";
#import "POSIX";
Profile :: #import,file "Profile.jai"()(enable=true);
#import,file "RepetitionTesting.jai";

read_with_posix_read :: (tester: *Repetition_Tester, params: Read_Parameters) {
    while IsTesting(tester) {
        assert(params.file_name.data[params.file_name.count] == 0, "File name is not null terminated!");
        fd := open(params.file_name.data, O_RDONLY);
        if fd >= 0 {
            buffer := params.buffer;
            if MaybeAllocate(params, *buffer) {
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
            } else {
                Error(tester, "Cannot allocate with %", params.alloc_type);
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
            if MaybeAllocate(params, *buffer) {
                BeginTime(tester);
                result := fread(buffer.data, xx buffer.count, 1, fp);
                EndTime(tester);

                if result == 1 {
                    CountBytes(tester, xx buffer.count);
                } else {
                    Error(tester, "fread failed");
                }

                MaybeDeallocate(params, *buffer);
            } else {
                Error(tester, "Cannot allocate with %", params.alloc_type);
            }
            fclose(fp);
        } else {
            Error(tester, "fopen failed");
        }
    }
}

read_with_mmap :: (tester: *Repetition_Tester, params: Read_Parameters) {
    if params.alloc_type != .None then return;

    while IsTesting(tester) {
        assert(params.file_name.data[params.file_name.count] == 0, "File name is not null terminated!");
        fd := open(params.file_name.data, O_RDONLY);
        if fd >= 0 {
            pagesize := mmap_size(AllocType.Mmap_4kb);
            length := mmap_rounded(params.buffer.count, pagesize);
            BeginTime(tester);
            memory := mmap(null, length, PROT_READ, MAP_PRIVATE | MAP_POPULATE, fd, 0);
            EndTime(tester);
            if memory == MAP_FAILED {
                Error(tester, "mmap failed");
            } else {
                buffer := params.buffer;
                buffer.data = memory;
                buffer_u64 := []u64.{data = xx buffer.data, count = buffer.count/size_of(u64)};

                // Force faults
                // mlock() seems to fail (shrug)
                // stepsize := pagesize.(s64) / size_of(u64);
                stepsize := 64 / size_of(u64);
                count: s64 = buffer_u64.count/stepsize;
                mydata := NewArray(count, u64, initialized=true); // Pre-fault this array so timing below is unaffected
                defer array_free(mydata);
                BeginTime(tester);
                for 0..count-1 {
                    mydata[it] = buffer_u64[it*stepsize];
                }
                EndTime(tester);
                // print("Sample bytes: % bytes are %\n", mydata.count, mydata);

                CountBytes(tester, xx buffer.count);
            }
            close(fd);
        } else {
            Error(tester, "fopen failed");
        }
    }
}


write_to_all_bytes :: (tester: *Repetition_Tester, params: Read_Parameters) {
    MOVAllBytesASM(0, null);
    while IsTesting(tester) {
        buffer := params.buffer;

        BeginTime(tester);
        for 0..buffer.count-1 {
            buffer.data[it] = cast,no_check(u8) it;
        }
        EndTime(tester);
        CountBytes(tester, xx buffer.count);
    }
}

#run {
    C :: #import "Compiler";
    object_files: [..]string;

    // Build the .o files
    assemble :: (path: string) #expand {
        P :: #import "Process";
        result := P.run_command("nasm", "-f", "elf64", tprint("%.asm", path));
        if result.exit_code != 0 {
            log_error("Failed to compile assembly at '%'. %", path, result);
            C.compiler_report("Failed to compile required assembly file");
        }
        array_add(*object_files, tprint("%.o", path));
    }
    assemble("./part3/listing_0132_nop_loop");
    assemble("./part3/listing_0134_multinop_loops");

    C.set_build_options_dc(.{append_linker_arguments=object_files});
}

// 132
MOVAllBytesASM :: (count: u64, data: *void) #foreign;
NOPAllBytesASM :: (count: u64, data: *void) #foreign;
CMPAllBytesASM :: (count: u64, data: *void) #foreign;
DECAllBytesASM :: (count: u64, data: *void) #foreign;
TestAsmMOV :: #bake_arguments TestAsm(func=MOVAllBytesASM);
TestAsmNOP :: #bake_arguments TestAsm(func=NOPAllBytesASM);
TestAsmCMP :: #bake_arguments TestAsm(func=CMPAllBytesASM);
TestAsmDEC :: #bake_arguments TestAsm(func=DECAllBytesASM);

// 134
NOP3x1AllBytes :: (count: u64, data: *void) #foreign;
NOP1x3AllBytes :: (count: u64, data: *void) #foreign;
NOP1x9AllBytes :: (count: u64, data: *void) #foreign;
Test_NOP3x1 :: #bake_arguments TestAsm(func=NOP3x1AllBytes);
Test_NOP1x3 :: #bake_arguments TestAsm(func=NOP1x3AllBytes);
Test_NOP1x9 :: #bake_arguments TestAsm(func=NOP1x9AllBytes);

TestAsm :: (tester: *Repetition_Tester, params: Read_Parameters, $func: (u64, *void) #c_call) {
    while IsTesting(tester) {
        buffer := params.buffer;
        MaybeAllocate(params, *buffer);
        count : u64 = xx,no_check buffer.count;
        data := buffer.data;
        BeginTime(tester);
        func(count, data);
        EndTime(tester);
        CountBytes(tester, count);
        MaybeDeallocate(params, *buffer);
    }
}

TestFunction :: struct {
    name: string;
    function: (tester: *Repetition_Tester, params: Read_Parameters);
}
test_functions :: TestFunction.[
    .{ "Test_NOP1x3", Test_NOP1x3 },
    .{ "Test_NOP3x1", Test_NOP3x1 },
    .{ "Test_NOP1x9", Test_NOP1x9 },
    // .{ "TestAsmMOV", TestAsmMOV },
    // .{ "TestAsmNOP", TestAsmNOP },
    // .{ "TestAsmCMP", TestAsmCMP },
    // .{ "TestAsmDEC", TestAsmDEC },
    // .{ "write_to_all_bytes", write_to_all_bytes },
    // .{ "mmap(fd)", read_with_mmap },
    // .{ "posix_read", read_with_posix_read },
    // .{ "fread", read_with_fread },
];

main :: () {
    args := get_command_line_arguments();
    if args.count < 2 {
        print("Please provide a file to read\n");
        // exit(1);
        args = .[args[0], "data_10000000.json"];
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
    testers: [test_functions.count][AllocType._test_to_here]Repetition_Tester;
    while true {
        for* func_array, func_index: testers {
            for alloc_type: 0..AllocType._test_to_here-1 {
                params.alloc_type = xx alloc_type;
                tester := *func_array.*[alloc_type];
                test_function := test_functions[func_index];

                print("\n--- %0 + %0 ---\n", cast(AllocType, alloc_type), test_function.name);
                NewTestWave(tester, xx params.buffer.count, cpu_timer_frequency, 5);
                test_function.function(tester, params);
                reset_temporary_storage();
            }
        }
    }
}
