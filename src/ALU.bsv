import ClientServer::*;
import GetPut::*;
import GetPutExt::*;
import FIFO::*;
import FIFOF::*;
import SpecialFIFOs::*;

import Trace::*;
import TomasuloTypes::*;

interface ALU;
    interface Server#(ALUReq, ALUResp) proc_server;
endinterface

(* synthesize *)
module mkALU( ALU );

    FIFOF#(ALUReq)   reqQ  <- mkFIFOF();
    FIFOF#(ALUResp) respQ <- mkBypassFIFOF();

    rule run;
        traceTiny("mkProc", "mkALU_run","E");
        
        let sra  = signedShiftRight;
        
        reqQ.deq();
        let req = reqQ.first();

        let x = req.op1;
        let y = req.op2;

        Data ans = ?;

        // *** check all of this    
        //  1. ensure arithmetic ops are done correctly
        case (req.op) matches
            tagged ADD:  ans = x + y;
            tagged SLT:  ans = slt(x, y);
            tagged SLTU: ans = sltu(x, y);
            tagged SLL:  ans = x << rshft(y);
            tagged SRL:  ans = x >> rshft(y);
            tagged SRA:  ans = sra(x, rshft(y)); 
            tagged SUB:  ans = x - y;
            tagged AND:  ans = x & y;
            tagged OR:   ans = x | y;
            tagged XOR:  ans = x ^ y;
            tagged NOR:  ans = ~(x | y);
            default: $display("[ERROR] ALU: invalid Op_Exec op [%x]!", req.op);
        endcase

        let resp = ALUResp{ans:ans, tag:req.tag};
        respQ.enq(resp);
    endrule

    interface Server proc_server;
        interface Put request  = tracePut("reqTiny", fifoToPut(reqQ));
        interface Get response = traceGet("respTiny", fifofToGet(respQ));
    endinterface
endmodule

module mkALUTest (Empty);

endmodule