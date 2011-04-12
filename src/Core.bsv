// The MIT License

// Copyright (c) 2009 Massachusetts Institute of Technology

// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:

// The above copyright notice and this permission notice shall be included in
// all copies or substantial portions of the Software.

// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
// THE SOFTWARE.

import Connectable::*;
import GetPut::*;
import ClientServer::*;

import DataCacheBlocking::*;
import InstCacheBlocking::*;
import Processor::*;
import MemArb::*;
import MemTypes::*;

interface CoreStats;
    interface DCacheStats dcache;
    interface ICacheStats icache;
    interface ProcStats proc;
endinterface

interface Core;

    // Interface from core to main memory
    interface Client#(MainMemReq,MainMemResp) mmem_client;

    // Statistics
    interface CoreStats stats;

    // CPU to Host
    interface CPUToHost tohost;

endinterface

(* synthesize *)
module mkCore(Core);

    // Instantiate the modules
    Proc proc <- mkProc();
    ICache#(InstReq,InstResp) icache <- mkInstCache();
    DCache#(DataReq,DataResp) dcache <- mkDataCache();
    MemArb marb <- mkMemArb();

    // Internal connections
    mkConnection( proc.statsEn_get,   icache.statsEn_put );
    mkConnection( proc.statsEn_get,   dcache.statsEn_put );
    mkConnection( proc.imem_client,   icache.proc_server );
    mkConnection( proc.dmem_client,   dcache.proc_server );
    mkConnection( icache.mmem_client, marb.cache0_server );
    mkConnection( dcache.mmem_client, marb.cache1_server );

    // Methods
    interface mmem_client = marb.mmem_client;

    interface CoreStats stats;
        interface dcache = dcache.stats;
        interface icache = icache.stats;
        interface proc = proc.stats;
    endinterface

    interface CPUToHost tohost = proc.tohost;

endmodule

