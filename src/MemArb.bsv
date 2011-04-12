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
import FIFOF::*;
import FIFO::*;

import MemTypes::*;
import Trace::*;

interface MemArb;

    interface Server#(MainMemReq,MainMemResp) cache0_server;
    interface Server#(MainMemReq,MainMemResp) cache1_server;
    interface Client#(MainMemReq,MainMemResp) mmem_client;

endinterface

typedef enum { REQ0, REQ1 } ReqPtr deriving(Eq,Bits);

(* synthesize *)
module mkMemArb( MemArb );

    //-----------------------------------------------------------
    // State

    FIFOF#(MainMemReq) req0Q  <- mkFIFOF1();
    FIFO#(MainMemResp) resp0Q <- mkFIFO1();

    FIFOF#(MainMemReq) req1Q  <- mkFIFOF1();
    FIFO#(MainMemResp) resp1Q <- mkFIFO1();

    FIFO#(MainMemReq)  mreqQ  <- mkFIFO1();
    FIFO#(MainMemResp) mrespQ <- mkFIFO1();

    Reg#(ReqPtr) nextReq <- mkReg(REQ0);

    //-----------------------------------------------------------
    // Some wires

    let req0avail = req0Q.notEmpty();
    let req1avail = req1Q.notEmpty();
  
    //-----------------------------------------------------------
    // Rules

    rule chooseReq0 ( req0avail && (!req1avail || (nextReq == REQ0)) );
        traceTiny("mkMemArb", "memArb req0",req0Q.first());

        // Rewrite tag field if this is a load ...
        MainMemReq mreq = case ( req0Q.first() ) matches
            tagged LoadReq  .ld: return LoadReq { tag:0, addr:ld.addr };
            tagged StoreReq .st: return req0Q.first();
        endcase;

        // Send out the request
        mreqQ.enq(mreq);
        nextReq <= REQ1;
        req0Q.deq();

    endrule

    rule chooseReq1 ( req1avail && (!req0avail || (nextReq == REQ1)) );
        traceTiny("mkMemArb", "memArb req1",req1Q.first);

        // Rewrite tag field if this is a load ...
        MainMemReq mreq = case ( req1Q.first() ) matches
            tagged LoadReq  .ld : return LoadReq { tag:1, addr:ld.addr };
            tagged StoreReq .st : return req1Q.first();
        endcase;

        // Send out the request
        mreqQ.enq(mreq);
        nextReq <= REQ0;
        req1Q.deq();

    endrule

    rule returnResp;
        traceTiny("mkMemArb", "resp",mrespQ.first());

        // Use tag to figure out where to send response
        mrespQ.deq();
        let tag = case ( mrespQ.first() ) matches
            tagged LoadResp  .ld : return ld.tag;
            tagged StoreResp .st : return st.tag;
        endcase;
     
        if ( tag == 0 ) begin
            resp0Q.enq(mrespQ.first());                                    
        end else begin
            resp1Q.enq(mrespQ.first());
        end

    endrule

    //-----------------------------------------------------------
    // Methods
  
    interface Server cache0_server;
        interface Put request  = toPut(req0Q);
        interface Get response = toGet(resp0Q);
    endinterface

    interface Server cache1_server;
        interface Put request  = toPut(req1Q);
        interface Get response = toGet(resp1Q);
    endinterface

    interface Client mmem_client;
        interface Get request  = toGet(mreqQ);
        interface Put response = toPut(mrespQ);
    endinterface

endmodule

