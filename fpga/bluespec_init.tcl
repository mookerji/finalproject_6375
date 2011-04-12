
if { [info exists bscws] } {
    # Add the "Progarm FPGA" button
    register_tool_bar_item program {
        puts {Programming FPGA}
        ../tools/program_fpga
    } database_lightning.gif {program fpga}

    # Add the "Run Testbench" button
    register_tool_bar_item runbench {
        puts {Running test bench}
        ./tb ../data/qsort.smips.vmh
    } cog.gif {run testbench}
}

