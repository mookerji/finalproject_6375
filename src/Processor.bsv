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
import RWire::*;
import Vector::*;

import MemTypes::*;
import ProcTypes::*;
import Trace::*;
import SFIFO::*;
import SpecialFIFOs::*;

import ALU::*;
import TomasuloTypes::*;
import ReorderBuffer::*;
import CommonDataBus::*;

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
// Register file module
//-----------------------------------------------------------

interface RFile;
    method Action   wr( Rindx rindx, Bit#(32) data );
    method Bit#(32) rd1( Rindx rindx );
    method Bit#(32) rd2( Rindx rindx );
endinterface

interface RenameFile;
    method Action   wr( Rindx rindx, RenameEntry entry );
    method RenameEntry rd1( Rindx rindx );
    method RenameEntry rd2( Rindx rindx );
endinterface

module mkRFile( RFile );
   
    RegFile#(Rindx,Bit#(32)) rfile <- mkRegFileWCF(0, 31);
    RWire#(Rindx) indxWire <- mkRWire();
    RWire#(Bit#(32)) dataWire <- mkRWire();
   
    method Action wr( Rindx rindx, Bit#(32) data );
        rfile.upd( rindx, data );
	    indxWire.wset(rindx);
	    dataWire.wset(data);
    endmethod
   
    method Bit#(32) rd1( Rindx rindx );
	    if (rindx == 0) return 0;
	    else if (indxWire.wget() matches tagged Valid .indx &&& indx == rindx)
	        return fromMaybe(0, dataWire.wget());
        else return rfile.sub(rindx);
    endmethod
   
    method Bit#(32) rd2( Rindx rindx );
	    if (rindx == 0) return 0;
	    else if (indxWire.wget() matches tagged Valid .indx &&& indx == rindx)
	        return fromMaybe(0, dataWire.wget());
        else return rfile.sub(rindx);
    endmethod

endmodule

// make this polymorphic using RFile 
module mkRenameFile( RenameFile );
   
    RegFile#(Rindx,RenameEntry) rfile <- mkRegFileWCF(0, 31);
    RWire#(Rindx) indxWire <- mkRWire();
    RWire#(RenameEntry) dataWire <- mkRWire();
   
    method Action wr( Rindx rindx, RenameEntry entry );
        rfile.upd( rindx, entry );
	    indxWire.wset(rindx);
	    dataWire.wset(entry);
    endmethod
   
    method RenameEntry rd1( Rindx rindx );
	    if (rindx == 0) return 0;
	    else if (indxWire.wget() matches tagged Valid .indx &&& indx == rindx)
	        return fromMaybe(0, dataWire.wget());
        else return rfile.sub(rindx);
    endmethod
   
    method RenameEntry rd2( Rindx rindx );
	    if (rindx == 0) return 0;
	    else if (indxWire.wget() matches tagged Valid .indx &&& indx == rindx)
	        return fromMaybe(0, dataWire.wget());
        else return rfile.sub(rindx);
    endmethod

endmodule

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
    RFile       rf    <- mkRFile;

	// Tomasulo algorithm data structures
	FIFO#(RSEntry) alu_rs <- mkBypassFIFOF();
	FIFO#(RSEntry) mem_rs <- mkBypassFIFOF();
	FIFO#(RSEntry) jb_rs <- mkBypassFIFOF();
	CommonDataBus#(CDBPacket, 4) cdb <- mkCDB();
	ROB#(16) rob <- mkReorderBuffer();
	RenameFile rename <- mkRenameFile();
	
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
	
    Instr inst = case ( instRespQ.first() ) matches
     	   			tagged LoadResp  .ld : return unpack(ld.data);
	 	   	   		tagged StoreResp .st : return ?;
		   		endcase;

	rule purge (False);
	endrule

	rule fetch;	
	    // no branch prediction
		traceTiny("mkProc", "fetch","F");
		traceTiny("mkProc", "pc", realpc);
        instReqQ.enq( LoadReq{ addr:pc_plus4, tag:0 } );
	endrule
	
	// this function has to be refactored and checked
	// *** see comments
	// *** action concurrency is going to be very important here
	function ActionValue#(RSEntry) setRSEntry_src1(RSEntry entry, Rindx src);
	    return 
	        actionvalue
	            // grab reservation station entry 
	            let ret = entry;
	            
                // grab mapped register
	            let rename_reg = rename.rd1(src);

	            // set reservation station entries
	            // if register has valid datam, use it
	            if (rename_reg matches tagged Valid) begin
	                ret.op1 = tagged Value rf.rd1(src);

	            // if common data bus has bypassed rob data, use it
	            end else if ((rename_reg matches tagged Tag .rob_tag) &&& cdb.hasData()) begin   
	                let cdb_data <- cdb.get(0);
	                if (cdb_data.tag == rob_tag) begin
	                    ret.op1 = tagged Value fromMaybe(cdb_data.data);
                    end 
                    
	            // if the rob has tagged data, use it
	            end else if ((rename_reg matches tagged Tag .rob_tag) 
	                            &&& isValid(rob.get(rob_tag))) begin
	            // *** rob needs to support multiple reads
	                ret.op1 = tagged Value rob.get(rob_tag);

	            // if nowhere has it, invalidate the operand
	            end else if (rename_reg matches tagged Tag .rob_tag) 
	                ret.op1 = tagged Tag rob_tag
	            end
	            
	            // return reservation station entry
	            return ret;
	        endactionvalue;
	endfunction
	
	function ActionValue#(RSEntry) setRSEntry_src2(RSEntry entry, Rindx src);
	    return 
	        actionvalue
	            // grab reservation station entry 
	            let ret = entry;
	            
                // grab mapped register
	            let rename_reg = rename.rd1(src);

	            // set reservation station entries
	            // if register has valid datam, use it
	            if (rename_reg matches tagged Valid) begin
	                ret.op2 = tagged Value rf.rd2(src);

	            // if common data bus has bypassed rob data, use it
	            end else if ((rename_reg matches tagged Tag .rob_tag) &&& cdb.hasData()) begin   
	                let cdb_data <- cdb.get(1);
	                if (cdb_data.tag == rob_tag) begin
	                    ret.op2 = tagged Value fromMaybe(cdb_data.data);
                    end 
                    
	            // if the rob has tagged data, use it
	            end else if ((rename_reg matches tagged Tag .rob_tag) 
	                            &&& isValid(rob.get(rob_tag))) begin
	            // *** rob needs to support multiple reads
	                ret.op2 = tagged Value rob.get(rob_tag);

	            // if nowhere has it, invalidate the operand
	            end else if (rename_reg matches tagged Tag .rob_tag) 
	                ret.op2 = tagged Tag rob_tag
	            end
	            
	            // return reservation station entry
	            return ret;
	        endactionvalue;
	endfunction
	
	function RSEntry setRSEntry_imm1(RSEntry entry, Data imm);
	    let ret = entry;
	    ret.op1 = tagged Value imm;
        return ret;
	endfunction
	
	function RSEntry setRSEntry_imm2(RSEntry entry, Data imm);
	    let ret = entry;
        ret.op2 = tagged Value imm;
        return ret;
	endfunction
	
	function Bool decode_issue_valid(); 
	    return !rob.isFull();
    endfunction
	
	// this stage decodes instructions, packs them into RSEntries, checks three sources 
	//  for instruction operands using setRSEntry_src{1,2}, enqueues them
	//  in the ReservationStation, and updates the reorder buffer.
	rule decode_issue (decode_issue_valid());
		traceTiny("mkProc", "decode_issue","I");
		
		// grab instruction
		instRespQ.deq();

		// initialize reservation station
		//	add op, rob tag to entry
		RSEntry rs_entry = ?; 
		
		// grab and set reorder buffer entry tag
		// by default, assume second operand is 0.
		rs_entry.op2 = tagged Value 0;
		
		// check in three places for register value with setRSEntry_src1 and setRSEntry_src2
		case ( inst ) matches

            // -- Memory Ops ------------------------------------------------
            // Rindx rbase; Rindx {rdst, rsrc}; Simm offset;
            tagged LW .it : begin
                // *** wait until rob entry is empty
                rs_entry.op = tagged valid LW {};
                rs_entry <- setRSEntry_src1(rs_entry, it.rbase);
                rs_entry =  setRSEntry_imm2(rs_entry, sext(it.offset));
            end

            tagged SW .it :begin
                // *** wait until rob entry is empty
                rs_entry.op = tagged Valid SW {};
                rs_entry <- setRSEntry_src1(rs_entry, it.rbase);
                rs_entry =  setRSEntry_imm2(rs_entry, sext(it.offset));
            end

            // -- Simple Ops ------------------------------------------------
            // ALU immediate and non-immediate instructions are handled identically
            //  in the alu, so the op tags are changed correspondingly.
            // Rindx rsrc;  Rindx rdst;  Simm imm;
            tagged ADDIU .it : begin
                rs_entry.op = tagged Valid ADD {};
                rs_entry <- setRSEntry_src1(rs_entry, it.rsrc);
                rs_entry =  setRSEntry_imm2(rs_entry, sext(it.imm));
            end
            tagged SLTI  .it : begin
                rs_entry.op = tagged Valid  SLT {};
                rs_entry <- setRSEntry_src1(rs_entry, it.rsrc);
                rs_entry =  setRSEntry_imm2(rs_entry, sext(it.imm));
            end
            tagged SLTIU .it : begin
                rs_entry.op = tagged Valid  SLTU {};
                rs_entry <- setRSEntry_src1(rs_entry, it.rsrc);
                rs_entry =  setRSEntry_imm2(rs_entry, sext(it.imm));
            end
            tagged ANDI  .it : begin
                rs_entry.op = tagged Valid  AND {};
                rs_entry <- setRSEntry_src1(rs_entry, it.rsrc);
                rs_entry =  setRSEntry_imm2(rs_entry, zext(it.imm));
            end
            tagged ORI   .it : begin
                rs_entry.op = tagged Valid  OR {};
                rs_entry <- setRSEntry_src1(rs_entry, it.rsrc);
                rs_entry =  setRSEntry_imm2(rs_entry, zext(it.imm));
            end
            tagged XORI  .it : begin
                rs_entry.op = tagged Valid  XOR {};
                rs_entry <- setRSEntry_src1(rs_entry, it.rsrc);
                rs_entry =  setRSEntry_imm2(rs_entry, zext(it.imm));
            end

            // Rindx rdst;  Zimm imm;
            tagged LUI   .it : begin
                rs_entry.op = tagged Valid  LUI {};
                rs_entry =  setRSEntry_imm1(rs_entry, zext(it.imm));
                rs_entry =  setRSEntry_imm2(rs_entry, 32'd16);
            end

            // Rindx rsrc;  Rindx rdst;  Shamt shamt;
            // *** make sure these 2nd operands are dispatched correctly
            tagged SLL   .it : begin
                rs_entry.op = tagged Valid  SLL {};
                rs_entry <- setRSEntry_src1(rs_entry, it.rsrc);
                rs_entry =  setRSEntry_imm2(rs_entry, zext(it.shamt));
            end
            tagged SRL   .it : begin
                rs_entry.op = tagged Valid  SRL {};
                rs_entry <- setRSEntry_src1(rs_entry, it.rsrc);
                rs_entry =  setRSEntry_imm2(rs_entry, zext(it.shamt));
            end
            tagged SRA   .it : begin
                rs_entry.op = tagged Valid  SRA {};
                rs_entry <- setRSEntry_src1(rs_entry, it.rsrc);
                rs_entry =  setRSEntry_imm2(rs_entry, zext(it.shamt));
            end

            // Rindx rsrc;  Rindx rdst;  Rindx rshamt;
            tagged SLLV  .it : begin
                rs_entry.op = tagged Valid  SLL {};
                rs_entry <- setRSEntry_src1(rs_entry, it.rsrc);
                rs_entry <- setRSEntry_src2(rs_entry, it.rshamt);
            end
            tagged SRLV  .it : begin
                rs_entry.op = tagged Valid  SRL {};
                rs_entry <- setRSEntry_src1(rs_entry, it.rsrc);
                rs_entry <- setRSEntry_src2(rs_entry, it.rshamt);
            end
            tagged SRAV  .it : begin
                rs_entry.op = tagged Valid  SRA {};
                rs_entry <- setRSEntry_src1(rs_entry, it.rsrc);
                rs_entry <- setRSEntry_src2(rs_entry, it.rshamt);
            end

            // Rindx rsrc1; Rindx rsrc2; Rindx rdst;
            tagged ADDU  .it : begin
                rs_entry.op = tagged Valid  ADD {};
                rs_entry <- setRSEntry_src1(rs_entry, it.rsrc1);
                rs_entry <- setRSEntry_src2(rs_entry, it.rsrc2);
            end
            tagged SUBU  .it : begin
                rs_entry.op = tagged Valid  SUB {};
                rs_entry <- setRSEntry_src1(rs_entry, it.rsrc1);
                rs_entry <- setRSEntry_src2(rs_entry, it.rsrc2);
            end
            tagged AND   .it : begin
                rs_entry.op = tagged Valid  AND {};
                rs_entry <- setRSEntry_src1(rs_entry, it.rsrc1);
                rs_entry <- setRSEntry_src2(rs_entry, it.rsrc2);
            end
            tagged OR    .it : begin
                rs_entry.op = tagged Valid  OR {};
                rs_entry <- setRSEntry_src1(rs_entry, it.rsrc1);
                rs_entry <- setRSEntry_src2(rs_entry, it.rsrc2);
            end
            tagged XOR   .it : begin
                rs_entry.op = tagged Valid  XOR {};
                rs_entry <- setRSEntry_src1(rs_entry, it.rsrc1);
                rs_entry <- setRSEntry_src2(rs_entry, it.rsrc2);
            end
            tagged NOR   .it : begin
                rs_entry.op = tagged Valid  NOR {};
                rs_entry <- setRSEntry_src1(rs_entry, it.rsrc1);
                rs_entry <- setRSEntry_src2(rs_entry, it.rsrc2);
            end
            tagged SLT   .it : begin
                rs_entry.op = tagged Valid  SLT {};
                rs_entry <- setRSEntry_src1(rs_entry, it.rsrc1);
                rs_entry <- setRSEntry_src2(rs_entry, it.rsrc2);
            end
            tagged SLTU  .it : begin
                rs_entry.op = tagged Valid  SLTU {};
                rs_entry <- setRSEntry_src1(rs_entry, it.rsrc1);
                rs_entry <- setRSEntry_src2(rs_entry, it.rsrc2);
            end

            // -- Branches --------------------------------------------------
            // Rindx rsrc;  Simm offset;
            tagged BLEZ  .it : begin
                rs_entry.op = tagged Valid  BLEZ {};
                rs_entry <- setRSEntry_src1(rs_entry, it.rsrc);
                rs_entry =  setRSEntry_imm2(rs_entry, sext(it.offset));    
            end
            tagged BGTZ  .it : begin
                rs_entry.op = tagged Valid  BGTZ {};
                rs_entry <- setRSEntry_src1(rs_entry, it.rsrc);
                rs_entry =  setRSEntry_imm2(rs_entry, sext(it.offset));
            end
            tagged BLTZ  .it : begin
                rs_entry.op = tagged Valid  BLTZ {};
                rs_entry <- setRSEntry_src1(rs_entry, it.rsrc);
                rs_entry =  setRSEntry_imm2(rs_entry, sext(it.offset));
            end
            tagged BGEZ  .it : begin
                rs_entry.op = tagged Valid  BGEZ {};
                rs_entry <- setRSEntry_src1(rs_entry, it.rsrc);
                rs_entry =  setRSEntry_imm2(rs_entry, sext(it.offset));
            end

            // Rindx rsrc1; Rindx rsrc2; Simm offset;
            // these two instructions need to have the offset stored
            //  with instruction tag
            tagged BEQ   .it : begin
                rs_entry.op = tagged Valid  BEQ {offset:it.offset};
                rs_entry <- setRSEntry_src1(rs_entry, it.rsrc1);
                rs_entry <- setRSEntry_src2(rs_entry, it.rsrc2);
            end
            tagged BNE   .it : begin
                rs_entry.op = tagged Valid  BNE {offset:it.offset};
                rs_entry <- setRSEntry_src1(rs_entry, it.rsrc1);
                rs_entry <- setRSEntry_src2(rs_entry, it.rsrc2);
            end

            // -- Jumps -----------------------------------------------------
            // Target target;
            tagged J     .it : begin
                rs_entry.op = tagged Valid  J {target:it.target};
            end
            // Rindx rsrc;
            tagged JR    .it : begin
                rs_entry.op = tagged Valid  JR {};
                rs_entry <- setRSEntry_src1(rs_entry, it.rsrc);
            end
            // Target target;
            tagged JAL   .it : begin
                rs_entry.op = tagged Valid  JAL {target:it.target};
            end
            // Rindx rsrc;  Rindx rdst;
            tagged JALR  .it : begin
                rs_entry.op = tagged Valid  JALR {};
                rs_entry <- setRSEntry_src1(rs_entry, it.rsrc);
            end

            // -- Cop0 ------------------------------------------------------
            // Rindx rsrc;  CP0indx cop0dst;
            tagged MTC0  .it : begin
                rs_entry.op = tagged Valid  MTC0 {};
                rs_entry <- setRSEntry_src1(rs_entry, it.rsrc);
            end

            // Rindx rdst;  CP0indx cop0src;
            tagged MFC0  .it : begin
                rs_entry.op = tagged Valid  MFC0 {};
                case (it.cop0src)
                    5'd10 :  rs_entry =  setRSEntry_imm1(bool2bits(cp0_statsEn)); 
                    5'd20 :  rs_entry =  setRSEntry_imm1(zext(cp0_fromhost));
                    5'd21 :  rs_entry =  setRSEntry_imm1(zext(cp0_tohost)); 
                    default : $display( " RTL-ERROR : %m decode_issue: Illegal MFC0 cop0src register!" );
                endcase
            end

            // -- Illegal ---------------------------------------------------
            default : $display( " RTL-ERROR : %m decode_issue: Illegal instruction !" );
        endcase
		
		// update reservation station by instruction type
		case (instr_type(inst))
		    ALU_OP: alu_rs.enq(rs_entry);
		    JP_OP:  jb_rs.enq(rs_entry);
		    MEM_OP: mem_rs.enq(rs_entry);
		    default:
		            $display( " RTL-ERROR : %m decode_issue: Illegal instruction type for rsentry!" );
		endcase
		
		// initialize reorder buffer entry
		ROBEntry rob_entry = ?;
		rob_entry.epoch = 0;
		rob_entry.dest = instr_dest(inst);
		// update rename table
		if (isValid(rob_entry.dest)) begin
		    RenameEntry dest_map = tagged Tag rs_entry.tag;
		    rename.wr(rob_entry.dest, dest_map);
	    end
		
		// update reorder buffer 
		rob.update(rob_entry, rs_entry.tag); 
	endrule
		
	// alu reservation station snooping. runs if alu_rs is full and cdb has data
	rule alu_rs_snoop_cdb (cdb.hasData() && !alu_rs.notFull());
	    let entry = alu_rs.first();
	    let cdb_data = cdb.get(2);
	    
	    // checks to see if either operand has a tag, and if it matches cdb tag
	    if (entry.op1 matches tagged Tag .rob_tag &&& (rob_tag == cdb_data.tag) && isValid(cdb_data.data)) begin
	        // update alu_rs first operand state
	        alu_rs.deq();
	        entry.op1 = tagged Value cdb_data.data;
            alu_rs.enq(entry);
	    end else if (entry.op1 matches tagged Tag .rob_tag &&& (rob_tag == cdb_data.tag) && isValid(cdb_data.data)) begin
	        // update alu_rs second operand state
	        alu_rs.deq();
	        entry.op2 = tagged Value cdb_data.data;
            alu_rs.enq(entry);
	    else 
	        $display( " RTL-ERROR : %m alu_rs_snoop_cdb: Invalid data on cdb!" );
	endrule
	
	rule dispatch_alu (!alu_rs.notFull() 
                        && (alu_rs.first().op1 matches tagged Value .op1_val) 
                        && (alu_rs.first().op2 matches tagged Value .op2_val));
	    alu_rs.deq();
	    let entry = alu_rs.first();
	    
	    // pass the ROB tag through the ALU
	    ALUReq req = ALUReq{op:entry.op, op1:op1_val, op2:op2_val, tag:entry.tag};
	    aluReqQ.enq(req);
	endrule
	
	rule dispatch_mem (dispatch_mem_valid());
	
	endrule
    
	// jb reservation station snooping. runs if jb_rs is full and cdb has data
	rule jb_rs_snoop_cdb (cdb.hasData() && !jb_rs.notFull());
	    let entry = jb_rs.first();
	    let cdb_data = cdb.get(3);
	    
	    // checks to see if either operand has a tag, and if it matches cdb tag
	    if (entry.op1 matches tagged Tag .rob_tag &&& (rob_tag == cdb_data.tag) && isValid(cdb_data.data)) begin
	        // update alu_rs first operand state
	        jb_rs.deq();
	        entry.op1 = tagged Value cdb_data.data;
            jb_rs.enq(entry);
	    end else if (entry.op1 matches tagged Tag .rob_tag &&& (rob_tag == cdb_data.tag) && isValid(cdb_data.data)) begin
	        // update alu_rs second operand state
	        jb_rs.deq();
	        entry.op2 = tagged Value cdb_data.data;
            jb_rs.enq(entry);
	    else 
	        $display( " RTL-ERROR : %m jb_rs_snoop_cdb: Invalid data on cdb!" );
	endrule
	
	rule dispatch_branch(!jb_rs.notFull() 
                    && (jb_rs.first().op1 matches tagged Value .op1_val) 
                    && (jb_rs.first().op2 matches tagged Value .op2_val));
                    
        jb_rs.deq();
        let entry = jb_rs.first();
        Addr  next_pc = pc_plus4;

	    case (entry.op) matches
    	    // -- Branches --------------------------------------------------
            tagged BLEZ  .it : begin    
                if ( signedLE( op1_val, 0 ) ) begin
                    next_pc = pc_plus4 + (op2_val << 2);
                end
            end
            tagged BGTZ  .it : begin
                if ( signedGT( op1_val, 0 ) ) begin
                    next_pc = pc_plus4 + (op2_val << 2);
                end
            end
            tagged BLTZ  .it : begin
                if ( signedLT( op1_val, 0 ) ) begin
                    next_pc = pc_plus4 + (op2_val << 2);
                end
            end
            tagged BGEZ  .it : begin
                if ( signedGE( op1_val, 0 ) ) begin
                    next_pc = pc_plus4 + (op2_val << 2);
                end
            end

            tagged BEQ   .it : begin
                if ( op1_val == op2_val ) begin
                    next_pc = pc_plus4 + (sext(it.offset) << 2);
                end
            end
            tagged BNE   .it : begin
                if ( op1_val != op2_val ) begin
                    next_pc = pc_plus4 + (sext(it.offset) << 2);
                end
            end

            // -- Jumps -----------------------------------------------------
            // Target target;
            tagged J     .it : begin
                next_pc = { pc_plus4[31:28], it.target, 2'b0 };
            end
            // Rindx rsrc;
            tagged JR    .it : begin
                next_pc = op1_val
            end
            // Target target;
            tagged JAL   .it : begin
                // *** this has a destination
                // wba( 31, pc_plus4 );
                next_pc = { pc_plus4[31:28], it.target, 2'b0 };
            end
            // Rindx rsrc;  Rindx rdst;
            tagged JALR  .it : begin
                /// *** this has a destination
                // wba( it.rdst, pc_plus4 );
                next_pc = op1_val;
            end
            default: $display( " RTL-ERROR : %m dispatch_branch: Invalid branch op tag!" );
        endcase
        
        realpc <= next_pc;
	endrule
	
	// alu completion stage
	rule alu_compl;
		traceTiny("mkProc", "compl","C");
		let ans = aluRespQ.first();
        aluRespQ.deq();
        
        // generate cdb packet and put on bus
        CDBPacket cdb_ans = CDBPacket{data:?, tag:ans.tag, epoch:0};
        cdb_ans.data = tagged Valid ans.data;
        cdb.put(cdb_ans);
        
        // update ROB 
        let rob_entry = fromMaybe(rob.get(tag));
        rob_entry.data = tagged Valid ans.data;
        rob.update(tag, rob_entry);
	endrule
	
	rule writeback (!rob.isEmpty() && isValid(rob.getLast().data));
		traceTiny("mkProc", "writeback","W");
		
		// update register file with data and update rename
		let head = rob.getLast();
		if isValid (head.dest) begin
		    rf.wr(head.dest, fromMaybe(head.data));
		    RenameEntry entry = tagged Valid;
		    rename.wr(head.dest, entry); 
		    // ^ *** is this correct? the tags for the rob have to match, I think.
	    end else 
	        $display( " RTL-ERROR : %m writeback: Illegal instruction destination!" );
		
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
