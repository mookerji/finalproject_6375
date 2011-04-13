import ClientServer::*;
import GetPut::*;
//import GetPutExt::*;
import FIFO::*;
import FIFOF::*;
import SpecialFIFOs::*;

import Trace::*;
import TomasuloTypes::*;
import ProcTypes::*;

interface ALU;
    interface Server#(ALUReq, ALUResp) proc_server;
endinterface

(* synthesize *)
module mkALU( ALU );

    FIFO#(ALUReq)   reqQ  <- mkFIFO();
    FIFO#(ALUResp) respQ <- mkBypassFIFO();

    // Some abbreviations
    let sext = signExtend;
    let zext = zeroExtend;
    let sra  = signedShiftRight;

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
            tagged ADD  .it:  ans = x + y;
            tagged SLT  .it:  ans = slt(x, y);
            tagged SLTU .it:  ans = sltu(x, y);
            tagged SLL  .it:  ans = x << rshft(y);
            tagged SRL  .it:  ans = x >> rshft(y);
            tagged SRA  .it:  ans = sra(x, rshft(y)); 
            tagged SUB  .it:  ans = x - y;
            tagged AND  .it:  ans = x & y;
            tagged OR   .it:  ans = x | y;
            tagged XOR  .it:  ans = x ^ y;
            tagged NOR  .it:  ans = ~(x | y);
            default: $display("[ERROR] ALU: invalid Op_Exec op [%x]!", req.op);
        endcase

        let resp = ALUResp{data:ans, tag:req.tag};
        respQ.enq(resp);
    endrule

    interface Server proc_server;
        interface Put request  = fifoToPut(reqQ);
        interface Get response = fifoToGet(respQ);
    endinterface
endmodule

module mkALUTest (Empty);

endmodule
