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
import RegFile::*;
import FIFO::*;
import FIFOF::*;
import SpecialFIFOs::*;
import RWire::*;

import MemTypes::*;
import ProcTypes::*;
import Trace::*;

interface ICacheStats;
    interface Get#(Stat) num_accesses;
    interface Get#(Stat) num_misses;
    interface Get#(Stat) num_evictions;
endinterface

interface ICache#( type req_t, type resp_t );

    // Interface from processor to cache
    interface Server#(req_t,resp_t) proc_server;

    // Interface from cache to main memory
    interface Client#(MainMemReq,MainMemResp) mmem_client;

    // Interface for enabling/disabling statistics
    interface Put#(Bool) statsEn_put;

    // Interface for collecting statistics
    interface ICacheStats stats;

endinterface

//----------------------------------------------------------------------
// Cache Types
//----------------------------------------------------------------------

typedef 10 CacheLineIndexSz;
typedef 20 CacheLineTagSz;
typedef 32 CacheLineSz;

typedef Bit#(CacheLineIndexSz) CacheLineIndex;
typedef Bit#(CacheLineTagSz)   CacheLineTag;
typedef Bit#(CacheLineSz)      CacheLine;

typedef enum 
{ 
    Init,
    Access, 
    Evict, 
    RefillReq, 
    RefillResp 
} 
CacheStage 
deriving (Eq,Bits);

//----------------------------------------------------------------------
// Helper functions
//----------------------------------------------------------------------

function Bit#(AddrSz) getAddr( InstReq req );

    Bit#(AddrSz) addr = ?;
    case ( req ) matches
        tagged LoadReq  .ld : addr = ld.addr;
        tagged StoreReq .st : addr = st.addr;
    endcase

    return addr;

endfunction

function CacheLineIndex getCacheLineIndex( InstReq req );
    Bit#(AddrSz) addr = getAddr(req);
    Bit#(CacheLineIndexSz) index = truncate( addr >> 2 );
    return index;
endfunction

function CacheLineTag getCacheLineTag( InstReq req );
    Bit#(AddrSz)         addr = getAddr(req);
    Bit#(CacheLineTagSz) tag  = truncate( addr >> fromInteger(valueOf(CacheLineIndexSz)) >> 2 );
    return tag;
endfunction

function Bit#(AddrSz) getCacheLineAddr( InstReq req );
    Bit#(AddrSz) addr = getAddr(req);
    return ((addr >> 2) << 2);
endfunction

//----------------------------------------------------------------------
// Main module
//----------------------------------------------------------------------

// The ram_style doc pragma keeps Xilinx from incorrectly inferring Block rams
// for the RegFiles.
(* doc = "synthesis attribute ram_style mkInstCache distributed;" *)
(* synthesize *)
module mkInstCache( ICache#(InstReq,InstResp) );

    //-----------------------------------------------------------
    // State

    Reg#(CacheStage) stage <- mkReg(Init);

    RegFile#(CacheLineIndex,Maybe#(CacheLineTag)) cacheTagRam  <- mkRegFileFull();
    RegFile#(CacheLineIndex,CacheLine)            cacheDataRam <- mkRegFileFull();

    FIFO#(InstReq)   reqQ  <- mkFIFO();
    FIFOF#(InstResp) respQ <- mkBypassFIFOF();

    FIFO#(MainMemReq)  mainMemReqQ  <- mkBypassFIFO();
    FIFO#(MainMemResp) mainMemRespQ <- mkFIFO();

    Reg#(CacheLineIndex) initCounter <- mkReg(1);

    // Statistics state

    Reg#(Bool)     statsEn        <- mkReg(False);

    Reg#(Stat) numAccesses <- mkReg(0);
    Reg#(Stat) numMisses <- mkReg(0);
    Reg#(Stat) numEvictions <- mkReg(0);

    //-----------------------------------------------------------
    // Name some wires

    let req              = reqQ.first();
    let reqIndex         = getCacheLineIndex(req);
    let reqTag           = getCacheLineTag(req);
    let reqCacheLineAddr = getCacheLineAddr(req);
    let refill           = mainMemRespQ.first();

    //-----------------------------------------------------------
    // Initialize

    rule init ( stage == Init );
        traceTiny("mkInstCacheBlocking", "stage","i");
        initCounter <= initCounter + 1;
        cacheTagRam.upd(initCounter,Invalid);
        if ( initCounter == 0 ) begin
            stage <= Access;
        end
    endrule

    //-----------------------------------------------------------
    // Cache access rule

    rule access ( (stage == Access) && respQ.notFull() );

        // Statistics

        if ( statsEn ) begin
            numAccesses <= numAccesses + 1;
        end

        // Check tag and valid bit to see if this is a hit or a miss

        Maybe#(CacheLineTag) cacheLineTag = cacheTagRam.sub(reqIndex);

        // Handle cache hits ...

        if ( isValid(cacheLineTag) && ( unJust(cacheLineTag) == reqTag ) )
        begin
            traceTiny("mkInstCacheBlocking", "hitMiss","h");
            reqQ.deq();

            case ( req ) matches
                tagged LoadReq .ld: respQ.enq( LoadResp { tag  : ld.tag, data : cacheDataRam.sub(reqIndex) } );
                tagged StoreReq .st: $display( " RTL-ERROR : %m : Stores are not allowed on the inst port!" );
            endcase
        end

        // Handle cache misses - since lines in instruction cache are
        // never dirty we can always immediately issue a refill request

        else 
        begin
            traceTiny("mkInstCacheBlocking", "hitMiss","m");
            if ( statsEn ) begin
                numMisses <= numMisses + 1;
                if ( isJust(cacheLineTag) ) begin
                    numEvictions <= numEvictions + 1;
                end
            end

            MainMemReq rfReq = LoadReq {
                tag  : 0,
                addr : reqCacheLineAddr
            };

            mainMemReqQ.enq(rfReq);
            stage <= RefillResp;    
        end

    endrule

    //-----------------------------------------------------------
    // Refill response rule

    rule refillResp ( stage == RefillResp );
        traceTiny("mkInstCacheBlocking", "stage","R");
        traceTiny("mkInstCacheBlocking", "refill",refill);

        // Write the new data into the cache and update the tag

        mainMemRespQ.deq();
        case ( mainMemRespQ.first() ) matches
            tagged LoadResp .ld: begin
                cacheTagRam.upd(reqIndex,Valid(reqTag));
                cacheDataRam.upd(reqIndex,ld.data);	
            end

            tagged StoreResp .st: noAction;
        endcase

        stage <= Access;
    endrule

    //-----------------------------------------------------------
    // Methods

    interface Client mmem_client;
        interface Get request  = fifoToGet(mainMemReqQ);
        interface Put response = fifoToPut(mainMemRespQ);
    endinterface

    interface Server proc_server;
        interface Put request  = tracePut("mkInstCacheBlocking", "reqTiny",toPut(reqQ));
        interface Get response = traceGet("mkInstCacheBlocking", "respTiny",toGet(respQ));
    endinterface

    interface Put statsEn_put = toPut(asReg(statsEn));

    interface ICacheStats stats;
        interface Get num_accesses = toGet(asReg(numAccesses));
        interface Get num_misses = toGet(asReg(numMisses));
        interface Get num_evictions = toGet(asReg(numEvictions));
    endinterface

endmodule

