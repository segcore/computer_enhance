#module_parameters()(enable := true, rdtsc: () -> u64 = (#import "System").read_cpu_counter);
#import "Basic";
verbose :: false; // debug this module

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


#if enable {
    MAX_PROFILES :: 4096;
    #add_context profiling: ProfileGlobals;
    #no_reset _profiles: [MAX_PROFILES]Anchor;
    #no_reset profile_unique_count: int = 1;

    #import "String";

    begin :: () {
        context.profiling.begin_tsc = rdtsc();
    }

    function :: (loc := #caller_location) #expand {
        name :: #procedure_name();
        // The `defer must be directly within this macro to make it work
        region := block_begin(name, loc);
        `defer {
            block_end(region, loc);
        }
    }

    block :: (name: string, loc := #caller_location) #expand {
        region := block_begin(name, loc);
        `defer {
            block_end(region, loc);
        }
    }

    block_begin :: ($name: string, loc : Source_Code_Location = #caller_location) -> Region #expand {
        ID :: #run,host -> int {
            Atomics :: #import "Atomics";
            ID := Atomics.atomic_add(*profile_unique_count, 1);
            // Ideally we'd set things here, but pointers are not remapped in #no_reset (yet?)
            // _profiles[ID].name = name;
            assert(ID < MAX_PROFILES, "Max profiles exceeded, increase Max_Profiles constant");
            return ID;
        };

        anchor := *_profiles[ID];
        anchor.name = name;
        region: Region;
        region.parent_index = context.profiling.active_index;
        region.anchor_index = ID;
        region.entry_tsc_including_children = anchor.tsc_including_children;
        region.start_tsc = rdtsc();
        context.profiling.active_index = ID;

        // object.hit_count += 1;
        #if verbose {
            context.profiling.depth += 1;
            for 1..context.profiling.depth  write_string("  ");
            print("--> enter '%'\n", name);

            #import "Print_Vars";
            print_vars(formatStruct(region, 0));
            print_vars(formatStruct(anchor.*, 0));
            print_vars(formatStruct(context.profiling, 0));
        }
        return region;
    }

    block_end :: (region: Region, loc := #caller_location) #expand {
        elapsed := rdtsc() - region.start_tsc;
        context.profiling.active_index = region.parent_index;

        parent := *_profiles[region.parent_index];
        anchor := *_profiles[region.anchor_index];

        parent.tsc_excluding_children -= elapsed;
        anchor.tsc_excluding_children += elapsed;
        anchor.tsc_including_children = region.entry_tsc_including_children + elapsed;
        anchor.hit_count += 1;

        #if verbose {
            context.profiling.depth -= 1;
            for 0..context.profiling.depth  write_string("  ");
            print("<-- exit  '%' (+%)  % ms\n", anchor.name, elapsed, format_tsc_millis(elapsed));
            print_vars(formatStruct(region, 0));
            print_vars(formatStruct(anchor.*, 0));
            print_vars(formatStruct(parent.*, 0));
            assert(context.profiling.depth >= 0, "Profiling depth has gone negative! There must be more block_ends than block_begins. Location %", anchor.name, loc);
        }
    }

    end_and_print :: () {
        end := rdtsc();

        // Print ordering
        profiles := array_copy(all_anchors());
        (#import "Sort").quick_sort(profiles, p => ~p.tsc_including_children);

        total_tsc_elapsed := end - context.profiling.begin_tsc;
        for* anchor: profiles {
            label := anchor.name;
            percent_child := 100.0 * (anchor.tsc_excluding_children.(float64) / total_tsc_elapsed.(float64));
            time_ms_child := 1000.0 * anchor.tsc_excluding_children.(float64) / cpu_frequency.(float64);
            percent_total := 100.0 * (anchor.tsc_including_children.(float64) / total_tsc_elapsed.(float64));
            time_ms_total := 1000.0 * anchor.tsc_including_children.(float64) / cpu_frequency.(float64);
            SPACES :: "                  ";
            name_padding := slice(SPACES, label.count, SPACES.count);
            print("%0%0 [%]  %\%/%\%  %/% ms\n", label, name_padding,
                    formatInt(anchor.hit_count, minimum_digits=7, padding=" "),
                    formatFloat(percent_child, trailing_width=2, width=5, zero_removal=.NO),
                    formatFloat(percent_total, trailing_width=2, width=5, zero_removal=.NO),
                    formatFloat(time_ms_child, trailing_width=2, width=8, zero_removal=.NO),
                    formatFloat(time_ms_total, trailing_width=2, width=8, zero_removal=.NO));
        }
        frame_time_ms := 1000.0 * total_tsc_elapsed.(float64) / cpu_frequency.(float64);
        print(" (total time: % ms)\n", formatFloat(frame_time_ms, trailing_width=2, zero_removal=.NO));

        reset();

        #if verbose {
            assert(context.profiling.depth == 0, "Printing when profile depth is not 0");
            context.profiling.depth = 0;
        }
    }

} else {

    // Module is disabled. Dummy everything.
    begin :: inline () {}
    function :: (loc := #caller_location) {}
    block :: (name: string, loc := #caller_location) {}
    block_begin :: inline ($name: string, loc : Source_Code_Location = #caller_location) -> Region { return .{}; }
    block_end :: (region: Region, loc := #caller_location) {}
    end_and_print :: inline () {}

}

reset :: () {
    for* anchor: all_anchors() {
        anchor.tsc_including_children = 0;
        anchor.tsc_excluding_children = 0;
        anchor.hit_count = 0;
    }
}

all_anchors :: inline () -> []Anchor {
    return array_view(_profiles, 1, profile_unique_count - 1);
}

Anchor :: struct {
    tsc_including_children: u64;
    tsc_excluding_children: u64;
    hit_count: int;

    name: string;
}

Region :: struct {
    entry_tsc_including_children: u64;
    start_tsc: u64;
    parent_index: s32;
    anchor_index: s32;
}

ProfileGlobals :: struct {
    begin_tsc: u64;
    active_index: s32;
    #if verbose {
        depth: s32;
    }
}
