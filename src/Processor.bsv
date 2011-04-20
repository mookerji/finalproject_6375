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
import FShow::*;
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
   
    BranchPredictor#(16) predictor <- mkBranchPredictor();
    RFile#(Bit#(32))       rf    <- mkRFile(0, True);
    FIFO#(FetchMetaData) pcQ <- mkFIFO();

    // Tomasulo algorithm data structures
    //TODO: make these fifos the correct datastructure
    FIFO#(RSEntry) mem_rs <- mkLFIFO();
    CommonDataBus#(CDBPacket) cdb <- mkCDB();
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
        return pcQ.first().epoch;
    endfunction

    function Addr firstInstPc();
        return pcQ.first().pc;
    endfunction

    // Some abbreviations
    let sext = signExtend;
    let zext = zeroExtend;
    let sra  = signedShiftRight;
    
    rule fetch; 
        // no branch prediction
        traceTiny("mkProc", "fetch","F");
        let pc <- predictor.predict();
        let epoch = predictor.currentEpoch();
        pcQ.enq(FetchMetaData { pc: pc, epoch: epoch } );
        instReqQ.enq( LoadReq{ addr: pc, tag: 0} );
    endrule
   
    // resolveOperand resolves an RSEntry Operand using an instructions src Rindx
    function ActionValue#(Operand) resolveOperand(Rindx src, 
                                                    function RenameEntry renameRead(Rindx r), 
                                                    function Data rfRead(Rindx r), 
                                                    ActionValue#(CDBPacket) cdb_data_av);
        return
            actionvalue
                Operand operand = ?;
                
                // grab mapped register
                let rename_reg = renameRead(src);
                
                // set reservation station operands
                case (rename_reg) matches 
                    // if register has valid datam, use it 
                    tagged Valid: begin
                        operand = tagged Imm rfRead(src);
                    end 
                    tagged Tag .rob_tag: begin
                        // if common data bus has bypassed rob data, use it   
                        if (cdb.hasData()) begin
                            let cdb_data <- cdb_data_av;
                            if (cdb_data.tag == rob_tag) begin
                                operand = tagged Imm fromMaybe(?, cdb_data.data);
                            end
                        
                        // if the rob has tagged data and data is valid, use it
                        end else if (rob.get(rob_tag) matches tagged Valid .rob_entry 
                                        &&& isValid(rob_entry.data)) begin
                            operand = tagged Imm fromMaybe(?, rob_entry.data);
                        
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
    
    function ActionValue#(Operand) resolveOperand0(Rindx src);
      let cdb_data_av = cdb.get0();
      return resolveOperand(src, rename.rd1, rf.rd1, cdb_data_av);
    endfunction
 
    function ActionValue#(Operand) resolveOperand1(Rindx src);
      let cdb_data_av = cdb.get1();
      return resolveOperand(src, rename.rd2, rf.rd2, cdb_data_av);
    endfunction
 
    function Bool decode_issue_valid();
        case (firstInst()) matches
            tagged LW .it: return rob.isEmpty();
            tagged SW .it: return rob.isEmpty();
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
        pcQ.deq();

        // initialize reservation station
        //  add op, rob tag to entry
        RSEntry rs_entry = ?; 
        rs_entry.op1 = tagged Imm 0;
        rs_entry.op2 = tagged Imm 0;
        
        // check in three places for register value with setRSEntry_src1 and setRSEntry_src2
        case ( firstInst() ) matches

            // -- Memory Ops ------------------------------------------------
            // Rindx rbase; Rindx {rdst, rsrc}; Simm offset;
            // offsets are handled inconsistently. change eventually
            tagged LW .it : begin
                rs_entry.op = tagged LW {};
                Addr addr = rf.rd1(it.rbase) + sext(it.offset);
                dataReqQ.enq(LoadReq{ addr:addr, tag: zeroExtend(it.rdst)});
            end

            tagged SW .it :begin
                rs_entry.op = tagged SW {};
                Addr addr = rf.rd1(it.rbase) + sext(it.offset);
                dataReqQ.enq( StoreReq{ tag:0, addr:addr, data:rf.rd2(it.rsrc) } );
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
                rs_entry.op1 <- resolveOperand0(it.rsrc);
                rs_entry.op2 = tagged Imm sext(it.imm);
            end
            tagged SLTIU .it : begin
                rs_entry.op = tagged  SLTU {};
                rs_entry.op1 <- resolveOperand0(it.rsrc);
                rs_entry.op2 = tagged Imm sext(it.imm); 
            end
            tagged ANDI  .it : begin
                rs_entry.op = tagged  AND {};
                rs_entry.op1 <- resolveOperand0(it.rsrc);
                rs_entry.op2 = tagged Imm zext(it.imm); 
            end
            tagged ORI   .it : begin
                rs_entry.op = tagged  OR {};
                rs_entry.op1 <- resolveOperand0(it.rsrc);
                rs_entry.op2 = tagged Imm zext(it.imm); 
            end
            tagged XORI  .it : begin
                rs_entry.op = tagged  XOR {};
                rs_entry.op1 <- resolveOperand0(it.rsrc);
                rs_entry.op2 = tagged Imm zext(it.imm); 
            end

            // Rindx rdst;  Zimm imm;
            tagged LUI   .it : begin
                rs_entry.op = tagged  LUI {};
                rs_entry.op1 = tagged Imm zext(it.imm);
            end

            // Rindx rsrc;  Rindx rdst;  Shamt shamt;
            // *** make sure these 2nd operands are dispatched correctly
            tagged SLL   .it : begin
                rs_entry.op = tagged  SLL {};
                rs_entry.op1 <- resolveOperand0(it.rsrc);
                rs_entry.op2 = tagged Imm zext(it.shamt);
            end
            tagged SRL   .it : begin
                rs_entry.op = tagged  SRL {};
                rs_entry.op1 <- resolveOperand0(it.rsrc);
                rs_entry.op2 = tagged Imm zext(it.shamt);
            end
            tagged SRA   .it : begin
                rs_entry.op = tagged  SRA {};
                rs_entry.op1 <- resolveOperand0(it.rsrc);
                rs_entry.op2 = tagged Imm zext(it.shamt);
            end

            // Rindx rsrc;  Rindx rdst;  Rindx rshamt;
            tagged SLLV  .it : begin
                rs_entry.op = tagged  SLL {};
                rs_entry.op1 <- resolveOperand0(it.rsrc);
                rs_entry.op2 <- resolveOperand1(it.rshamt);
            end
            tagged SRLV  .it : begin
                rs_entry.op = tagged  SRL {};
                rs_entry.op1 <- resolveOperand0(it.rsrc);
                rs_entry.op2 <- resolveOperand1(it.rshamt);
            end
            tagged SRAV  .it : begin
                rs_entry.op = tagged  SRA {};
                rs_entry.op1 <- resolveOperand0(it.rsrc);
                rs_entry.op2 <- resolveOperand1(it.rshamt);
            end

            // Rindx rsrc1; Rindx rsrc2; Rindx rdst;
            tagged ADDU  .it : begin
                rs_entry.op = tagged  ADD {};
                rs_entry.op1 <- resolveOperand0(it.rsrc1);
                rs_entry.op2 <- resolveOperand1(it.rsrc2);
            end
            tagged SUBU  .it : begin
                rs_entry.op = tagged  SUB {};
                rs_entry.op1 <- resolveOperand0(it.rsrc1);
                rs_entry.op2 <- resolveOperand1(it.rsrc2);
            end
            tagged AND   .it : begin
                rs_entry.op = tagged  AND {};
                rs_entry.op1 <- resolveOperand0(it.rsrc1);
                rs_entry.op2 <- resolveOperand1(it.rsrc2);
            end
            tagged OR    .it : begin
                rs_entry.op = tagged  OR {};
                rs_entry.op1 <- resolveOperand0(it.rsrc1);
                rs_entry.op2 <- resolveOperand1(it.rsrc2);
            end
            tagged XOR   .it : begin
                rs_entry.op = tagged  XOR {};
                rs_entry.op1 <- resolveOperand0(it.rsrc1);
                rs_entry.op2 <- resolveOperand1(it.rsrc2);
            end
            tagged NOR   .it : begin
                rs_entry.op = tagged  NOR {};
                rs_entry.op1 <- resolveOperand0(it.rsrc1);
                rs_entry.op2 <- resolveOperand1(it.rsrc2);
            end
            tagged SLT   .it : begin
                rs_entry.op = tagged  SLT {};
                rs_entry.op1 <- resolveOperand0(it.rsrc1);
                rs_entry.op2 <- resolveOperand1(it.rsrc2);
            end
            tagged SLTU  .it : begin
                rs_entry.op = tagged  SLTU {};
                rs_entry.op1 <- resolveOperand0(it.rsrc1);
                rs_entry.op2 <- resolveOperand1(it.rsrc2);
            end

            // -- Branches --------------------------------------------------
            // Rindx rsrc;  Simm offset;
            tagged BLEZ  .it : begin
                rs_entry.op = tagged  BLEZ {};
                rs_entry.op1 <- resolveOperand0(it.rsrc);
                rs_entry.op2 = tagged Imm sext(it.offset);
            end
            tagged BGTZ  .it : begin
                rs_entry.op = tagged  BGTZ {};
                rs_entry.op1 <- resolveOperand0(it.rsrc);
                rs_entry.op2 = tagged Imm sext(it.offset);
            end
            tagged BLTZ  .it : begin
                rs_entry.op = tagged  BLTZ {};
                rs_entry.op1 <- resolveOperand0(it.rsrc);
                rs_entry.op2 = tagged Imm sext(it.offset);
            end
            tagged BGEZ  .it : begin
                rs_entry.op = tagged  BGEZ {};
                rs_entry.op1 <- resolveOperand0(it.rsrc);
                rs_entry.op2 = tagged Imm sext(it.offset);
            end

            // Rindx rsrc1; Rindx rsrc2; Simm offset;
            // these two instructions need to have the offset stored
            //  with instruction tag
            tagged BEQ   .it : begin
                rs_entry.op = tagged  BEQ {offset:it.offset};
                rs_entry.op1 <- resolveOperand0(it.rsrc1);
                rs_entry.op2 <- resolveOperand1(it.rsrc2);
            end
            tagged BNE   .it : begin
                rs_entry.op = tagged  BNE {offset:it.offset};
                rs_entry.op1 <- resolveOperand0(it.rsrc1);
                rs_entry.op2 <- resolveOperand1(it.rsrc2);
            end

            // -- Jumps -----------------------------------------------------
            // Target target;
            tagged J     .it : begin
                rs_entry.op = tagged  J {target:it.target};
            end
            // Rindx rsrc;
            tagged JR    .it : begin
                rs_entry.op = tagged  JR {};
                rs_entry.op1 <- resolveOperand0(it.rsrc);
            end
            // Target target;
            tagged JAL   .it : begin
                rs_entry.op = tagged  JAL {target:it.target};
            end
            // Rindx rsrc;  Rindx rdst;
            tagged JALR  .it : begin
                rs_entry.op = tagged  JALR {};
                rs_entry.op1 <- resolveOperand0(it.rsrc);
            end

            // -- Cop0 ------------------------------------------------------
            // Rindx rsrc;  CP0indx cop0dst;
            // make sure the ROB etc handles 
            tagged MTC0  .it : begin
                rs_entry.op = tagged  ADD {};
                rs_entry.op1 <- resolveOperand0(it.rsrc);
            end

            // Rindx rdst;  CP0indx cop0src;
            tagged MFC0  .it : begin
                rs_entry.op = tagged  ADD {};
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

        rs_entry.epoch = firstInstEpoch();
        rs_entry.pc = firstInstPc();
        if (instr_type(firstInst()) != MEM_OP) begin
            // reserve reorder buffer entry
            rs_entry.tag <- rob.reserve(firstInstEpoch(), firstInstPc(), instr_dest(firstInst()));
	    $display("decode_issue tag: %d", rs_entry.tag);

            // update reservation station by instruction type
            case (instr_type(firstInst()))
                ALU_OP: alu_rs.put(rs_entry);
                JB_OP:  alu_rs.put(rs_entry);
                default:
                        $display( " RTL-ERROR : %m decode_issue: Illegal instruction type for rsentry!" );
            endcase

            // update rename table
            if (instr_dest(firstInst()) matches tagged ArchReg .areg)
                rename.wr(areg, tagged Tag rs_entry.tag);
        end else begin
            mem_rs.enq(rs_entry);
        end
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
	$display("dispatch_alu tag: %d", entry.tag);
        $write("putting ALUReq ",fshow(entry.op));
        $display("| %d | %d | %d | %h | %d",op1_val, op2_val, entry.tag, entry.pc, entry.epoch);
        ALUReq req = ALUReq{op:entry.op, op1:op1_val, op2:op2_val, tag:entry.tag, pc: entry.pc, epoch: entry.epoch};
        aluReqQ.enq(req);
    endrule

    rule mem_graduate_ld (dataRespQ.first() matches tagged LoadResp .ld
                            &&& mem_rs.first().op matches tagged LW .it);
        dataRespQ.deq();
        mem_rs.deq();
        rf.wr( truncate(ld.tag), ld.data );
    endrule
    
    rule mem_graduate_st (dataRespQ.first() matches tagged StoreResp .st
                            &&& mem_rs.first().op matches tagged SW .it);
        dataRespQ.deq();
        mem_rs.deq();
    endrule
    
    // alu completion stage
    rule alu_compl;
        traceTiny("mkProc", "compl","C");
        let ans = aluRespQ.first();
        aluRespQ.deq();
	

        // update ROB 
        // generate cdb packet and put on bus
	$display("alu_compl for alu tag: %d", ans.tag);
        CDBPacket cdb_ans = CDBPacket{data: tagged Valid ans.data, tag:ans.tag, epoch:ans.epoch};
        cdb.put(cdb_ans);
        if (instr_ext_type(ans.op) == JB_OP) begin
            $display("alu_compl for jb tag: %d", ans.tag);
            let next_pc = ans.next_pc;
            if (next_pc != predictor.confirmPredict(ans.pc)) begin
                rob.updatePrediction(ans.tag, next_pc);
            end
        end
    endrule

    rule cdb_rob_bypass;
        let packet <- cdb.get3();
        $display("cdb_rob_bypass tag: %d", packet.tag);
        if (packet.data matches tagged Valid .it)
          rob.updateData(packet.tag, it);
    endrule

    rule purge (rob.getLast().epoch != predictor.currentEpoch());
        rob.complete();
    endrule
    
    rule graduate (rob.getLast().data matches tagged Valid .data &&& rob.getLast().epoch == predictor.currentEpoch());
        traceTiny("mkProc", "graduate","G");
        
        // update register file with data and update rename
        ROBEntry head = rob.getLast();
        
        case (head.dest) matches
            // MTC0
            tagged SpecReg .sreg:
              case (sreg)
                  5'd10 : cp0_statsEn <= unpack(truncate(data)); 
                  5'd21 : cp0_tohost  <= truncate(data);
                  default: $display("oh noes this never happens to us!!!");
              endcase
            tagged ArchReg .areg: rf.wr(areg, data);
            default : $display("blow chunks through a windowpane");
        endcase

        // check type for rename
        if(head.dest matches tagged ArchReg .areg &&& rename.rd1(areg) matches tagged Tag .ren_tag &&& ren_tag == rob.getLastTag()) begin
           rename.wr(areg, tagged Valid);
$display("flushed rename map entry");
        end

        //if mispredict
        if (head.mispredict matches tagged Valid .dst) begin
            predictor.mispredict(head.pc, dst);
$display("committed mispredict");
        end
$display("graduation happened");
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
                1: 0;
                2: 0;
            endcase);
        endmethod
    endinterface
endmodule 
