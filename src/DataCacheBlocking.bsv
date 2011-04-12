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

import MemTypes::*;
import ProcTypes::*;
import Trace::*;

interface DCacheStats;
    interface Get#(Stat) num_accesses;
    interface Get#(Stat) num_misses;
    interface Get#(Stat) num_writebacks; 
endinterface

interface DCache#( type req_t, type resp_t );

    // Interface from processor to cache
    interface Server#(req_t,resp_t) proc_server;

    // Interface from cache to main memory
    interface Client#(MainMemReq,MainMemResp) mmem_client;

    // Interface for enabling/disabling statistics
    interface Put#(Bool) statsEn_put;

    // Interface for collecting statistics
    interface DCacheStats stats;

endinterface


//----------------------------------------------------------------------
// Cache Types
//----------------------------------------------------------------------

typedef 10   CacheLineIndexSz;
typedef 20   CacheLineTagSz;
typedef 32   CacheLineSz;

typedef Bit#(CacheLineIndexSz) CacheLineIndex;
typedef Bit#(CacheLineTagSz)   CacheLineTag;
typedef Bit#(CacheLineSz)      CacheLine;

typedef enum 
{ 
    Init,
    Access, 
    RefillReq, 
    RefillResp 
} 
CacheStage 
deriving (Eq,Bits);

//----------------------------------------------------------------------
// Helper functions
//----------------------------------------------------------------------

function Bit#(AddrSz) getAddr( DataReq req );

    Bit#(AddrSz) addr = ?;
    case ( req ) matches
        tagged LoadReq  .ld : addr = ld.addr;
        tagged StoreReq .st : addr = st.addr;
    endcase

    return addr;

endfunction

function CacheLineIndex getCacheLineIndex( DataReq req );
    Bit#(AddrSz) addr = getAddr(req);
    Bit#(CacheLineIndexSz) index = truncate( addr >> 2 );
    return index;
endfunction

function CacheLineTag getCacheLineTag( DataReq req );
    Bit#(AddrSz)         addr = getAddr(req);
    Bit#(CacheLineTagSz) tag  = truncate( addr >> fromInteger(valueOf(CacheLineIndexSz)) >> 2 );
    return tag;
endfunction

function Bit#(AddrSz) getCacheLineAddr( DataReq req );
    Bit#(AddrSz) addr = getAddr(req);
    return ((addr >> 2) << 2);
endfunction

//----------------------------------------------------------------------
// Main module
//----------------------------------------------------------------------

// The ram_style doc pragma keeps Xilinx from incorrectly inferring Block rams
// for the RegFiles.
(* doc = "synthesis attribute ram_style mkDataCache distributed;" *)
(* synthesize *)
module mkDataCache( DCache#(DataReq,DataResp) );

    //-----------------------------------------------------------
    // State

    Reg#(CacheStage) stage <- mkReg(Init);

    RegFile#(CacheLineIndex,Maybe#(CacheLineTag)) cacheTagRam  <- mkRegFileFull();
    RegFile#(CacheLineIndex,CacheLine)            cacheDataRam <- mkRegFileFull();

    FIFO#(DataReq)   reqQ  <- mkFIFO();
    FIFOF#(DataResp) respQ <- mkBypassFIFOF();

    FIFO#(MainMemReq)  mainMemReqQ  <- mkBypassFIFO();
    FIFO#(MainMemResp) mainMemRespQ <- mkFIFO();

    Reg#(CacheLineIndex) initCounter <- mkReg(1);

    // Statistics state

    Reg#(Bool)     statsEn        <- mkReg(False);

    Reg#(Stat) num_accesses <- mkReg(0);
    Reg#(Stat) num_misses <- mkReg(0);
    Reg#(Stat) num_writebacks <- mkReg(0);

    //-----------------------------------------------------------
    // Name some wires

    let req              = reqQ.first();
    let reqIndex         = getCacheLineIndex(req);
    let reqTag           = getCacheLineTag(req);
    let reqCacheLineAddr = getCacheLineAddr(req);

    //-----------------------------------------------------------
    // Initialize

    rule init ( stage == Init );
        traceTiny("mkDataCacheBlocking", "stage","i");
        initCounter <= initCounter + 1;
        cacheTagRam.upd(initCounter,Invalid);
        if ( initCounter == 0 ) begin
            stage <= Access;
        end
    endrule
   
    //-----------------------------------------------------------
    // Access cache rule

    rule access ( (stage == Access) && respQ.notFull() );

        // Statistics

        if ( statsEn ) begin
            num_accesses <= num_accesses + 1;
        end
 
        // Get the corresponding tag from the rams

        Maybe#(CacheLineTag) cacheLineTag = cacheTagRam.sub(reqIndex);
  
        // Handle cache hits ...

        if ( isValid(cacheLineTag) && ( unJust(cacheLineTag) == reqTag ) )
        begin
            traceTiny("mkDataCacheBlocking", "hitMiss","h");	 
            reqQ.deq();

            case ( req ) matches
                tagged LoadReq .ld: respQ.enq( LoadResp { tag: ld.tag, data: cacheDataRam.sub(reqIndex) } );
                tagged StoreReq .st: begin
                    respQ.enq( StoreResp { tag : st.tag } );
                    cacheDataRam.upd(reqIndex,st.data);
                end 
            endcase
        end

        // Handle cache misses ...
    
        else 
        begin
            traceTiny("mkDataCacheBlocking", "hitMiss","m");
            if ( statsEn ) begin
                num_misses <= num_misses + 1;
            end

            // Currently we don't use dirty bits so we always writeback the data if it is valid

            if ( isValid(cacheLineTag) )
            begin

                if ( statsEn ) begin
                    num_writebacks <= num_writebacks + 1;
                end

                MainMemReq wbReq = StoreReq {
                    tag  : 0, 
                    addr : { unJust(cacheLineTag), reqIndex, 2'b0 },
                    data : cacheDataRam.sub(reqIndex)
                };
	    
                mainMemReqQ.enq(wbReq);
                stage <= RefillReq;    
            end

            // Otherwise we can issue the refill request now

            else
            begin	    
                mainMemReqQ.enq( LoadReq { tag: 0, addr: reqCacheLineAddr } );
                stage <= RefillResp;    
            end
        end
    endrule

    //-----------------------------------------------------------
    // Refill request rule
   
    rule refillReq ( stage == RefillReq );
        traceTiny("mkDataCacheBlocking", "stage","r");
        mainMemReqQ.enq( LoadReq { tag: 0, addr: reqCacheLineAddr } );
        stage <= RefillResp;
    endrule
 
    //-----------------------------------------------------------
    // Refill response rule
   
    rule refillResp ( stage == RefillResp );
        traceTiny("mkDataCacheBlocking", "stage","R");
        traceTiny("mkDataCacheBlocking", "refill",mainMemRespQ.first());
 
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
        interface Get request  = toGet(mainMemReqQ);
        interface Put response = toPut(mainMemRespQ);
    endinterface

    interface Server proc_server;
        interface Put request  = tracePut("mkDataCacheBlocking", "reqTiny",toPut(reqQ));
        interface Get response = traceGet("mkDataCacheBlocking", "respTiny",toGet(respQ));
    endinterface

    interface Put statsEn_put = toPut(asReg(statsEn));

    interface DCacheStats stats;
        interface Get num_accesses = toGet(asReg(num_accesses));
        interface Get num_misses = toGet(asReg(num_misses));
        interface Get num_writebacks = toGet(asReg(num_writebacks));
    endinterface

endmodule

