
import ClientServer::*;
import FIFO::*;
import GetPut::*;
import DefaultValue::*;
import SceMi::*;
import Clocks::*;
import ResetXactor::*;

import Core::*;
import ProcTypes::*;
import Processor::*;
import DataCacheBlocking::*;
import InstCacheBlocking::*;

typedef Core DutInterface;

(* synthesize *)
module [Module] mkDutWrapper (DutInterface);
    Core core <- mkCore();

    // For tracing
    Reg#(int) cycle <- mkReg(0);
    rule printCycles (True);
        cycle <= cycle + 1;
        $fdisplay(stderr, " => Cycle = %d", cycle);
    endrule

    return core;
endmodule

module [SceMiModule] mkSceMiLayer();

    SceMiClockConfiguration conf = defaultValue;

    SceMiClockPortIfc clk_port <- mkSceMiClockPort(conf);
    DutInterface dut <- buildDutWithSoftReset(mkDutWrapper, clk_port);

    Empty mmem <- mkClientXactor(dut.mmem_client, clk_port);
    Empty tohost <- mkCPUToHostXactor(dut.tohost, clk_port);
    Empty stats <- mkCoreStatsXactor(dut.stats, clk_port);

    Empty shutdown <- mkShutdownXactor();
endmodule

module [SceMiModule] mkCPUToHostXactor#(CPUToHost tohost, SceMiClockPortIfc clk_port ) (Empty);

    // Access the controlled clock and reset
    Clock cclock = clk_port.cclock;
    Reset creset = clk_port.creset;

    // req belongs entirely to the controlled clock domain. We'll use the
    // clock domain crossings already implemented by the Bluespec people (in
    // the Put and Get transactors), because they know about such things
    // better than I do.
    FIFO#(int) req <- mkFIFO(clocked_by cclock, reset_by creset);

    Get#(Bit#(32)) resp = interface Get;
        method ActionValue#(Bit#(32)) get();
            req.deq();
            return tohost.cpuToHost(req.first());
        endmethod
    endinterface;

    Empty request <- mkPutXactor(toPut(req), clk_port);
    Empty response <- mkGetXactor(resp, clk_port);

endmodule

typedef enum {
    DCACHE_ACCESSES, DCACHE_MISSES, DCACHE_WRITEBACKS,
    ICACHE_ACCESSES, ICACHE_MISSES, ICACHE_EVICTIONS,
    PROC_INST, PROC_CYCLES
} StatID deriving(Bits, Eq);

module [SceMiModule] mkCoreStatsXactor#(CoreStats stats, SceMiClockPortIfc clk_port) (Empty);

    // Access the controlled clock and reset
    Clock cclock = clk_port.cclock;
    Reset creset = clk_port.creset;

    // Again, req and resp belong to the controlled clock domain.
    FIFO#(StatID) req <- mkFIFO(clocked_by cclock, reset_by creset);
    FIFO#(Stat) resp <- mkFIFO(clocked_by cclock, reset_by creset);

    rule handleRequest (True);
        case (req.first())
            DCACHE_ACCESSES: begin
                let x <- stats.dcache.num_accesses.get();
                resp.enq(x);
            end
            DCACHE_MISSES: begin
                let x <- stats.dcache.num_misses.get();
                resp.enq(x);
            end
            DCACHE_WRITEBACKS: begin
                let x <- stats.dcache.num_writebacks.get();
                resp.enq(x);
            end
            ICACHE_ACCESSES: begin
                let x <- stats.icache.num_accesses.get();
                resp.enq(x);
            end
            ICACHE_MISSES: begin
                let x <- stats.icache.num_misses.get();
                resp.enq(x);
            end
            ICACHE_EVICTIONS: begin
                let x <- stats.icache.num_evictions.get();
                resp.enq(x);
            end
            PROC_INST: begin
                let x <- stats.proc.num_inst.get();
                resp.enq(x);
            end
            PROC_CYCLES: begin
                let x <- stats.proc.num_cycles.get();
                resp.enq(x);
            end
        endcase
        req.deq();
    endrule

    Server#(StatID, Stat) server = interface Server;
        interface Get response = toGet(resp);
        interface Put request = toPut(req);
    endinterface;

    Empty xx <- mkServerXactor(server, clk_port);
endmodule

