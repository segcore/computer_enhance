cpu_frequency: u64;

estimate_cpu_frequency :: () -> u64 {
    start := rdtsc();
    end: u64;
    time_begin := current_time_monotonic();
    time_should_end := time_begin + milliseconds_to_apollo(100);
    time_end: Apollo_Time;
    while time_end < time_should_end {
        time_end = current_time_monotonic();
        end = rdtsc();
    }
    diff_rdtsc := end - start;
    diff_time := to_float64_seconds(time_end - time_begin);
    cpu_frequency = cast(u64, diff_rdtsc / diff_time);
    return cpu_frequency;
}

profile_begin :: () {
    profile_globals.frame_start = rdtsc();
}

profile_end :: () {
    profile_globals.frame_end = rdtsc();
}

profile_function :: (loc := #caller_location) #expand {
    name :: #procedure_name();
    // The `defer must be directly within this macro to make it work
    profile_block_begin(name, loc);
    `defer {
        profile_block_end(name, loc);
    }
}

profile_scope :: (name: string, loc := #caller_location) #expand {
    profile_block_begin(name, loc);
    `defer {
        profile_block_end(name, loc);
    }
}

profile_block_begin :: ($name: string, loc := #caller_location) #expand {
    ID := #run,host -> int {
        Atomics :: #import "Atomics";
        ID := Atomics.atomic_add(*profile_unique_count, 1);
        // This might work with #no_reset, but that crashes at runtime...
        // _profiles[ID].ID = ID;
        // _profiles[ID].name = name;
        // _profiles[ID].loc = loc;
        assert(ID < Max_Profiles, "Max profiles exceeded, increase Max_Profiles constant");
        return ID;
    };
    info := *_profiles[ID];
    info.ID = ID;
    info.name = name;
    info.loc = loc;
    info.min_depth = ifx info.min_depth == 0 then context.profile_depth else min(info.min_depth, context.profile_depth);
    info.max_depth = max(info.max_depth, context.profile_depth);

    context.profile_dfs[context.profile_depth] = info;
    context.profile_depth += 1;
    if verbose {
        for 1..context.profile_depth  write_string("  ");
        print("--> enter '%'\n", name);
    }

    info.begin_tsc = rdtsc();
}

profile_block_end :: ($name: string = "", loc := #caller_location) #expand {
    end := rdtsc();
    context.profile_depth -= 1;
    assert(context.profile_depth >= 0, "More profile ENDs than begins. Location %", loc);
    info := context.profile_dfs[context.profile_depth];
    delta := end - info.begin_tsc;
    info.total_tsc += delta;

    if verbose {
        for 0..context.profile_depth  write_string("  ");
        print("<-- exit  '%' at %  (delta %)  % ms\n", info.name, end, delta, formatFloat(1000.0*delta.(float64)/cpu_frequency, trailing_width=2));
    }

    #if name assert(info.name == name, "Block name mismatch. Expected %, got %. Location % -> %", info.name, name, info.loc, loc);
}

profile_print :: (do_reset := true, require_depth_0 := true) {
    profile_end();
    profiles := array_copy(array_view(_profiles, 0, profile_unique_count));
    // sort by begin tsc
    (#import "Sort").quick_sort(profiles, p => p.begin_tsc);

    total_tsc_elapsed := ifx profile_globals.frame_start then profile_globals.frame_end - profile_globals.frame_start;
    for info: profiles {
        label := info.name;
        percent := 100.0 * (info.total_tsc.(float64) / total_tsc_elapsed.(float64));
        time_ms := 1000.0 * info.total_tsc.(float64) / cpu_frequency.(float64);
        SPACES :: "                  ";
        DEPTH_MARKERS :: ">>>>>>>>>>>>";
        depth_indent := slice(DEPTH_MARKERS, 0, info.max_depth);
        depth_padding := slice(SPACES, 0, 4 - depth_indent.count);
        name_padding := slice(SPACES, label.count, SPACES.count);
        print("%0%0%0%0 % (%\%) % ms\n", depth_indent, depth_padding, label, name_padding,
                formatInt(info.total_tsc, minimum_digits=12, padding=" "),
                formatFloat(percent, trailing_width=2, width=5, zero_removal=.NO),
                formatFloat(time_ms, trailing_width=2, width=8, zero_removal=.NO));
    }
    frame_time_ms := 1000.0 * total_tsc_elapsed.(float64) / cpu_frequency.(float64);
    print(" (total time: % ms)\n", formatFloat(frame_time_ms, trailing_width=2, zero_removal=.NO));

    if do_reset {
        for* info: profiles {
            info.total_tsc = 0;
            info.min_depth = 0;
            info.max_depth = 0;
        }
        profile_globals.frame_start = rdtsc();
        profile_globals.frame_end = profile_globals.frame_start;
    }

    if require_depth_0 assert(context.profile_depth == 0, "Printing when profile depth is not 0");
}


ProfileInfo :: struct {
    ID: int;
    name: string;
    loc: Source_Code_Location;

    begin_tsc: u64;
    total_tsc: u64;
    min_depth: int;
    max_depth: int;
}

profile_globals: struct {
    frame_start: u64;
    frame_end: u64;
};

Max_Depth :: 16;
Max_Profiles :: 1024;
#add_context profile_dfs: [Max_Depth]*ProfileInfo; // depth-first... search (bad name)
#add_context profile_depth: int;

#scope_file
#no_reset profile_unique_count: int;
_profiles: [1024]ProfileInfo;

verbose :: false; // debug this module
