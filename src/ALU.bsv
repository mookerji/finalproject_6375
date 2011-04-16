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

    FIFO#(ALUReq) reqQ <- mkFIFO();
    FIFO#(ALUResp) respQ <- mkBypassFIFO();

    // Some abbreviations
    let sext = signExtend;
    let zext = zeroExtend;
    let sra  = signedShiftRight;

    rule run;
        traceTiny("mkProc", "mkALU","EA");
        
        let sra  = signedShiftRight;
        
        reqQ.deq();
        let req = reqQ.first();

        let x = req.op1;
        let y = req.op2;

        Data ans = 0;
        Addr pc_plus4 = req.pc + 4;
        Addr next_pc = pc_plus4;

        // *** check all of this    
        //  1. ensure arithmetic ops are done correctly
        case (req.op) matches
            // standard arithmetic ops
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
            // branch and jump operations calculate the next_pc
            // branch operations
            tagged BLEZ .it:  if (signedLE(x, 0))  next_pc = pc_plus4 + (y << 2);
            tagged BGTZ .it:  if (signedGT(x, 0))  next_pc = pc_plus4 + (y << 2);
            tagged BLTZ .it:  if (signedLT(x, 0))  next_pc = pc_plus4 + (y << 2);
            tagged BGEZ .it:  if (signedGE(x, 0))  next_pc = pc_plus4 + (y << 2);
            tagged BEQ  .it:  if (x==y) next_pc = pc_plus4 + (sext(it.offset) << 2);
            tagged BNE  .it:  if (x!=y) next_pc = pc_plus4 + (sext(it.offset) << 2);
            // jump operations (these just pass through)
            tagged J    .it:  next_pc = { pc_plus4[31:28], it.target, 2'b0 };
            tagged JR   .it:  next_pc = x;
            tagged JAL  .it: begin
              next_pc = { pc_plus4[31:28], it.target, 2'b0 };
              ans = pc_plus4;
            end
            tagged JALR .it: begin
              next_pc = x;
              ans = pc_plus4;
            end
            default: $display("[ERROR] ALU: invalid Op_Exec op [%x]!", req.op);
        endcase

        let resp = ALUResp{op:req.op, data:ans, tag:req.tag, pc: req.pc, next_pc: next_pc, epoch: req.epoch};
        respQ.enq(resp);
    endrule

    interface Server proc_server;
        interface Put request  = fifoToPut(reqQ);
        interface Get response = fifoToGet(respQ);
    endinterface
endmodule

module mkALUTest (Empty);

endmodule
