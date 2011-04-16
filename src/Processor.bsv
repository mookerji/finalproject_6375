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
import FIFO::*;
import FIFOF::*;
import RWire::*;
import Vector::*;

import MemTypes::*;
import ProcTypes::*;
import Trace::*;
import SpecialFIFOs::*;

import ALU::*;
import TomasuloTypes::*;
import ReorderBuffer::*;
import CommonDataBus::*;
import BranchPredictor::*;
import RFile::*;
import ReservationStation::*;

interface ProcStats;
    interface Get#(Stat) num_cycles;
    interface Get#(Stat) num_inst;
endinterface

interface CPUToHost;
    method Bit#(32) cpuToHost(int req);
endinterface

interface Proc;

    // Interface from processor to caches
    interface Client#(DataReq,DataResp) dmem_client;
    interface Client#(InstReq,InstResp) imem_client;

    // Interface for enabling/disabling statistics on the rest of the core
    interface Get#(Bool) statsEn_get;

    // Interface for collecting statistics.
    interface ProcStats stats;

    // Interface to host
    interface CPUToHost tohost;

endinterface

typedef enum { PCgen, Decode_Issue, Execute, Completion, Writeback } Stage deriving(Eq,Bits);


//-----------------------------------------------------------
// Reference processor
//-----------------------------------------------------------


(* doc = "synthesis attribute ram_style mkProc distributed;" *)
(* synthesize *)
module  mkProc( Proc );	

    //-----------------------------------------------------------
    // State

    // Standard processor state
   
    Reg#(Addr)  realpc    <- mkReg(32'h00001000);
    BranchPredictor#(16) predictor <- mkBranchPredictor();
    RFile#(Bit#(32))       rf    <- mkRFile(0, True);

    // Tomasulo algorithm data structures
    //TODO: make these fifos the correct datastructure
    FIFO#(RSEntry) mem_rs <- mkLFIFO();
    FIFO#(RSEntry) jb_rs <- mkLFIFO();
    CommonDataBus#(CDBPacket, 4) cdb <- mkCDB();
    ROB#(16) rob <- mkReorderBuffer();
    RFile#(RenameEntry) rename <- mkRFile(tagged Valid, False);
    ReservationStation alu_rs <- mkReservationStation(cdb);
    
    // plug and play execution unit
    ALU alu_exec <- mkALU();
    FIFO#(ALUReq) aluReqQ <- mkBypassFIFO();
    FIFO#(ALUResp) aluRespQ <- mkBypassFIFO();
    mkConnection( fifoToGet(aluReqQ), alu_exec.proc_server.request );
    mkConnection( fifoToPut(aluRespQ), alu_exec.proc_server.response );

    Reg#(Bit#(32)) cp0_tohost   <- mkReg(0);
    Reg#(Bit#(32)) cp0_fromhost <- mkReg(0);
    Reg#(Bool)    cp0_statsEn  <- mkReg(False);

    // Memory request/response state
   
    FIFO#(InstReq)  instReqQ    <- mkLFIFO();
    FIFO#(InstResp) instRespQ   <- mkBypassFIFO();

    FIFO#(DataReq)  dataReqQ    <- mkLFIFO();
    FIFO#(DataResp) dataRespQ   <- mkBypassFIFO();

    // Statistics state
    Reg#(Stat) num_cycles <- mkReg(0);
    Reg#(Stat) num_inst <- mkReg(0);

    function Instr firstInst();
        case ( instRespQ.first() ) matches
            tagged LoadResp  .ld : return unpack(ld.data);
            tagged StoreResp .st : return ?;
        endcase
    endfunction

    function Epoch firstInstEpoch();
        case ( instRespQ.first() ) matches
            tagged LoadResp  .ld : return unpack(ld.tag);
            tagged StoreResp .st : return unpack(st.tag);
        endcase
    endfunction

    let pc_plus4 = realpc + 4;
    
    // Some abbreviations
    let sext = signExtend;
    let zext = zeroExtend;
    let sra  = signedShiftRight;
    
    rule fetch; 
        // no branch prediction
        traceTiny("mkProc", "fetch","F");
        traceTiny("mkProc", "pc", realpc);
        let x <- predictor.predict();
        instReqQ.enq( LoadReq{ addr:x, tag:predictor.currentEpoch() } );
    endrule
    
    // resolveOperand resolves an RSEntry Operand using an instructions src Rindx
    // oper_indx should be 0 or 1 to resolve first and second operands, respectively
    function ActionValue#(Operand) resolveOperand(Rindx src, Bit#(2) oper_indx);
        return
            actionvalue
                Operand operand = ?;
                
                // grab mapped register
                if (oper_indx > 1) $display( " RTL-ERROR : %m resolveOperand: Invalid oper_indx!" );
                let rename_reg = (oper_indx==0) ? rename.rd1(src) : rename.rd2(src);
                
                // set reservation station operands
                case (rename_reg) matches 
                    // if register has valid datam, use it 
                    tagged Valid: begin
                        operand = (oper_indx==0) ? tagged Imm rf.rd1(src) : tagged Imm rf.rd2(src);
                    end 
                    tagged Tag .rob_tag: begin
                        // if common data bus has bypassed rob data, use it   
                        if (cdb.hasData()) begin
                            let cdb_data <- cdb.get(oper_indx);
                            if (cdb_data.tag == rob_tag) begin
                                operand = tagged Imm fromMaybe(?, cdb_data.data);
                            end
                        
                        // if the rob has tagged data, use it
                        end else if (isValid(rob.get(rob_tag))) begin
                            // **** fix this
                            //operand = tagged Imm rob.get(rob_tag);
                        
                        // if nowhere has it, invalidate the operand
                        end else begin 
                            operand = tagged Tag rob_tag;
                        end
                    end 
                    default:
                        $display( " RTL-ERROR : %m resolveOperand: Invalid oper_indx!" );
                endcase
                return operand;
            endactionvalue;
    endfunction 
    
    function Bool decode_issue_valid();
        case (firstInst()) matches
          tagged SW .it: return False;
          default: return !rob.isFull();
        endcase
    endfunction
    
    // this stage decodes instructions, packs them into RSEntries, checks three sources 
    //  for instruction operands using setRSEntry_src{1,2}, enqueues them
    //  in the ReservationStation, and updates the reorder buffer.
    rule decode_issue (decode_issue_valid());
        traceTiny("mkProc", "decode_issue","I");
        
        // grab instruction
        instRespQ.deq();

        // initialize reservation station
        //  add op, rob tag to entry
        RSEntry rs_entry = ?; 
        
        // grab and set reorder buffer entry tag
        let defaultEntry = ROBEntry { data:tagged Invalid, mispredict:tagged Invalid, dest:0, epoch:0 };
        rs_entry.tag <- rob.reserve(defaultEntry);
        
        // by default, assume second operand is 0.
        rs_entry.op2 = tagged Imm 0;
        
        // check in three places for register value with setRSEntry_src1 and setRSEntry_src2
        case ( firstInst() ) matches

            // -- Memory Ops ------------------------------------------------
            // Rindx rbase; Rindx {rdst, rsrc}; Simm offset;
            tagged LW .it : begin
                // *** wait until rob entry is empty
                rs_entry.op = tagged LW {};
                rs_entry.op1 <- resolveOperand(it.rbase, 0);
                rs_entry.op2 = tagged Imm sext(it.offset);
            end

            tagged SW .it :begin
                // *** wait until rob entry is empty
                // never happens
                rs_entry.op = tagged SW {};
                rs_entry.op1 <- resolveOperand(it.rbase, 0);
                rs_entry.op2 = tagged Imm sext(it.offset);
            end

            // -- Simple Ops ------------------------------------------------
            // ALU immediate and non-immediate instructions are handled identically
            //  in the alu, so the op tags are changed correspondingly.
            // Rindx rsrc;  Rindx rdst;  Simm imm;
            tagged ADDIU .it : begin
                rs_entry.op = tagged ADD {};
                rs_entry.op2 = tagged Imm sext(it.imm);
            end
            tagged SLTI  .it : begin
                rs_entry.op = tagged  SLT {};
                rs_entry.op1 <- resolveOperand(it.rsrc, 0);
                rs_entry.op2 = tagged Imm sext(it.imm);
            end
            tagged SLTIU .it : begin
                rs_entry.op = tagged  SLTU {};
                rs_entry.op1 <- resolveOperand(it.rsrc, 0);
                rs_entry.op2 = tagged Imm sext(it.imm); 
            end
            tagged ANDI  .it : begin
                rs_entry.op = tagged  AND {};
                rs_entry.op1 <- resolveOperand(it.rsrc, 0);
                rs_entry.op2 = tagged Imm zext(it.imm); 
            end
            tagged ORI   .it : begin
                rs_entry.op = tagged  OR {};
                rs_entry.op1 <- resolveOperand(it.rsrc, 0);
                rs_entry.op2 = tagged Imm zext(it.imm); 
            end
            tagged XORI  .it : begin
                rs_entry.op = tagged  XOR {};
                rs_entry.op1 <- resolveOperand(it.rsrc, 0);
                rs_entry.op2 = tagged Imm zext(it.imm); 
            end

            // Rindx rdst;  Zimm imm;
            tagged LUI   .it : begin
                rs_entry.op = tagged  LUI {};
                rs_entry.op1 = tagged Imm zext(it.imm);
                rs_entry.op2 = tagged Imm 32'd16;
            end

            // Rindx rsrc;  Rindx rdst;  Shamt shamt;
            // *** make sure these 2nd operands are dispatched correctly
            tagged SLL   .it : begin
                rs_entry.op = tagged  SLL {};
                rs_entry.op1 <- resolveOperand(it.rsrc, 0);
                rs_entry.op2 = tagged Imm zext(it.shamt);
            end
            tagged SRL   .it : begin
                rs_entry.op = tagged  SRL {};
                rs_entry.op1 <- resolveOperand(it.rsrc, 0);
                rs_entry.op2 = tagged Imm zext(it.shamt);
            end
            tagged SRA   .it : begin
                rs_entry.op = tagged  SRA {};
                rs_entry.op1 <- resolveOperand(it.rsrc, 0);
                rs_entry.op2 = tagged Imm zext(it.shamt);
            end

            // Rindx rsrc;  Rindx rdst;  Rindx rshamt;
            tagged SLLV  .it : begin
                rs_entry.op = tagged  SLL {};
                rs_entry.op1 <- resolveOperand(it.rsrc, 0);
                rs_entry.op2 <- resolveOperand(it.rshamt, 1);
            end
            tagged SRLV  .it : begin
                rs_entry.op = tagged  SRL {};
                rs_entry.op1 <- resolveOperand(it.rsrc, 0);
                rs_entry.op2 <- resolveOperand(it.rshamt, 1);
            end
            tagged SRAV  .it : begin
                rs_entry.op = tagged  SRA {};
                rs_entry.op1 <- resolveOperand(it.rsrc, 0);
                rs_entry.op2 <- resolveOperand(it.rshamt, 1);
            end

            // Rindx rsrc1; Rindx rsrc2; Rindx rdst;
            tagged ADDU  .it : begin
                rs_entry.op = tagged  ADD {};
                rs_entry.op1 <- resolveOperand(it.rsrc1, 0);
                rs_entry.op2 <- resolveOperand(it.rsrc2, 1);
            end
            tagged SUBU  .it : begin
                rs_entry.op = tagged  SUB {};
                rs_entry.op1 <- resolveOperand(it.rsrc1, 0);
                rs_entry.op2 <- resolveOperand(it.rsrc2, 1);
            end
            tagged AND   .it : begin
                rs_entry.op = tagged  AND {};
                rs_entry.op1 <- resolveOperand(it.rsrc1, 0);
                rs_entry.op2 <- resolveOperand(it.rsrc2, 1);
            end
            tagged OR    .it : begin
                rs_entry.op = tagged  OR {};
                rs_entry.op1 <- resolveOperand(it.rsrc1, 0);
                rs_entry.op2 <- resolveOperand(it.rsrc2, 1);
            end
            tagged XOR   .it : begin
                rs_entry.op = tagged  XOR {};
                rs_entry.op1 <- resolveOperand(it.rsrc1, 0);
                rs_entry.op2 <- resolveOperand(it.rsrc2, 1);
            end
            tagged NOR   .it : begin
                rs_entry.op = tagged  NOR {};
                rs_entry.op1 <- resolveOperand(it.rsrc1, 0);
                rs_entry.op2 <- resolveOperand(it.rsrc2, 1);
            end
            tagged SLT   .it : begin
                rs_entry.op = tagged  SLT {};
                rs_entry.op1 <- resolveOperand(it.rsrc1, 0);
                rs_entry.op2 <- resolveOperand(it.rsrc2, 1);
            end
            tagged SLTU  .it : begin
                rs_entry.op = tagged  SLTU {};
                rs_entry.op1 <- resolveOperand(it.rsrc1, 0);
                rs_entry.op2 <- resolveOperand(it.rsrc2, 1);
            end

            // -- Branches --------------------------------------------------
            // Rindx rsrc;  Simm offset;
            tagged BLEZ  .it : begin
                rs_entry.op = tagged  BLEZ {};
                rs_entry.op1 <- resolveOperand(it.rsrc, 0);
                rs_entry.op2 = tagged Imm sext(it.offset);
            end
            tagged BGTZ  .it : begin
                rs_entry.op = tagged  BGTZ {};
                rs_entry.op1 <- resolveOperand(it.rsrc, 0);
                rs_entry.op2 = tagged Imm sext(it.offset);
            end
            tagged BLTZ  .it : begin
                rs_entry.op = tagged  BLTZ {};
                rs_entry.op1 <- resolveOperand(it.rsrc, 0);
                rs_entry.op2 = tagged Imm sext(it.offset);
            end
            tagged BGEZ  .it : begin
                rs_entry.op = tagged  BGEZ {};
                rs_entry.op1 <- resolveOperand(it.rsrc, 0);
                rs_entry.op2 = tagged Imm sext(it.offset);
            end

            // Rindx rsrc1; Rindx rsrc2; Simm offset;
            // these two instructions need to have the offset stored
            //  with instruction tag
            tagged BEQ   .it : begin
                rs_entry.op = tagged  BEQ {offset:it.offset};
                rs_entry.op1 <- resolveOperand(it.rsrc1, 0);
                rs_entry.op2 <- resolveOperand(it.rsrc2, 1);
            end
            tagged BNE   .it : begin
                rs_entry.op = tagged  BNE {offset:it.offset};
                rs_entry.op1 <- resolveOperand(it.rsrc1, 0);
                rs_entry.op2 <- resolveOperand(it.rsrc2, 1);
            end

            // -- Jumps -----------------------------------------------------
            // Target target;
            tagged J     .it : begin
                rs_entry.op = tagged  J {target:it.target};
            end
            // Rindx rsrc;
            tagged JR    .it : begin
                rs_entry.op = tagged  JR {};
                rs_entry.op1 <- resolveOperand(it.rsrc, 0);
            end
            // Target target;
            tagged JAL   .it : begin
                rs_entry.op = tagged  JAL {target:it.target};
            end
            // Rindx rsrc;  Rindx rdst;
            tagged JALR  .it : begin
                rs_entry.op = tagged  JALR {};
                rs_entry.op1 <- resolveOperand(it.rsrc, 0);
            end

            // -- Cop0 ------------------------------------------------------
            // Rindx rsrc;  CP0indx cop0dst;
            tagged MTC0  .it : begin
                rs_entry.op = tagged  MTC0 {};
                rs_entry.op1 <- resolveOperand(it.rsrc, 0);
            end

            // Rindx rdst;  CP0indx cop0src;
            tagged MFC0  .it : begin
                rs_entry.op = tagged  MFC0 {};
                case (it.cop0src)
                    5'd10 :  rs_entry.op1 = tagged Imm zext(pack(cp0_statsEn)); 
                    5'd20 :  rs_entry.op1 = tagged Imm zext(cp0_fromhost);
                    5'd21 :  rs_entry.op1 = tagged Imm zext(cp0_tohost); 
                    default : $display( " RTL-ERROR : %m decode_issue: Illegal MFC0 cop0src register!" );
                endcase
            end

            // -- Illegal ---------------------------------------------------
            default : $display( " RTL-ERROR : %m decode_issue: Illegal instruction !" );
        endcase
        
        // update reservation station by instruction type
        case (instr_type(firstInst()))
            ALU_OP: alu_rs.put(rs_entry);
            JB_OP:  jb_rs.enq(rs_entry);
            MEM_OP: mem_rs.enq(rs_entry);
            default:
                    $display( " RTL-ERROR : %m decode_issue: Illegal instruction type for rsentry!" );
        endcase
        
        // initialize reorder buffer entry
        ROBEntry rob_entry = ?;
        rob_entry.epoch = predictor.currentEpoch(); //todo: use the epoch for this instruction
        rob_entry.dest = fromMaybe(?, instr_dest(firstInst()));
        // update rename table
        if (isValid(instr_dest(firstInst()))) begin
            RenameEntry dest_map = tagged Tag rs_entry.tag;
            rename.wr(rob_entry.dest, dest_map);
        end
        
        // update reorder buffer 
        rob.update(rs_entry.tag, rob_entry); 
    endrule
        
    rule dispatch_alu;
        let entry <- alu_rs.getReadyEntry();
        
        // pass the ROB tag through the ALU
        Data op1_val = ?;
        Data op2_val = ?;
        case (entry.op1) matches
          tagged Imm .x: op1_val = x;
          tagged Tag .x: $display("Shouldn't happen");
        endcase
        case (entry.op2) matches
          tagged Imm .x: op2_val = x;
          tagged Tag .x: $display("Shouldn't happen");
        endcase
        ALUReq req = ALUReq{op:entry.op, op1:op1_val, op2:op2_val, tag:entry.tag};
        aluReqQ.enq(req);
    endrule
    
    rule dispatch_load;
        mem_rs.deq();
        let entry = mem_rs.first();
    endrule
    
    // alu completion stage
    rule alu_compl;
        traceTiny("mkProc", "compl","C");
        let ans = aluRespQ.first();
        aluRespQ.deq();
        
        case (instr_ext_type(ans.op))
            ALU_OP: begin
                // generate cdb packet and put on bus
                CDBPacket cdb_ans = CDBPacket{data: tagged Valid ans.data, tag:ans.tag, epoch:0};
                cdb.put(cdb_ans);
            end
            JB_OP:  begin
                let next_pc = ans.data;
                realpc <= next_pc;
                if (next_pc != predictor.confirmPredict(realpc)) begin
                    let rob_entry = fromMaybe(?, rob.get(ans.tag));
                    rob_entry.mispredict = tagged Valid tuple2(realpc, next_pc);
                    rob.update(ans.tag, rob_entry);
                end
            end
            default:
                $display( " RTL-ERROR : %m alu_compl: Illegal instruction type for aluRespQ!" );
        endcase
    endrule

    rule purge (rob.getLast().epoch != predictor.currentEpoch());
        rob.complete();
    endrule
    
    rule graduate (rob.getLast().epoch == predictor.currentEpoch());
        traceTiny("mkProc", "graduate","G");
        
        // update register file with data and update rename
        ROBEntry head = rob.getLast();
        if (isValid(head.data)) begin
            rf.wr(head.dest, fromMaybe(0, head.data));

	   // check type for rename
	   if(rename.rd1(head.dest) matches tagged Tag .ren_tag &&& ren_tag == rob.getLastTag())
              rename.wr(head.dest, tagged Valid);
        end

        //if mispredict
        if (isValid(head.mispredict)) begin
            match { .src, .dst } = fromMaybe(tuple2(0,0), head.mispredict); 
            predictor.mispredict(src, dst);
        end

        // pick up mispredicts from here
        // retire from the reorder buffer
        rob.complete();
    endrule

    rule inc_num_cycles;
        if ( cp0_statsEn ) begin
            num_cycles <= num_cycles+1;
        end
    endrule

    //-----------------------------------------------------------
    // Methods

    interface Client imem_client;
        interface Get request  = toGet(instReqQ);
        interface Put response = toPut(instRespQ);
    endinterface

    interface Client dmem_client;
        interface Get request  = toGet(dataReqQ);
        interface Put response = toPut(dataRespQ);
    endinterface

    interface Get statsEn_get = toGet(asReg(cp0_statsEn));

    interface ProcStats stats;
        interface Get num_cycles = toGet(asReg(num_cycles));
        interface Get num_inst = toGet(asReg(num_inst));
    endinterface

    interface CPUToHost tohost;
        method Bit#(32) cpuToHost(int req);
            return (case (req)
                0: cp0_tohost;
                1: realpc;
                2: 0;
            endcase);
        endmethod
    endinterface
endmodule 
