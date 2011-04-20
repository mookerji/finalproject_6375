//----------------------------------------------------------------------
// Basic instruction type
//----------------------------------------------------------------------

import ProcTypes::*;
import FShow::*;

typedef union tagged {
              void Valid;
              ROBTag Tag;
              } RenameEntry deriving (Bits, Eq);

typedef Bit#(6) OpCode;

// *** Register file should probably just contain a unified
//      data type with ROBTags/Data. Why do we have a 
//      separate rename map again? Operands should be
//      copies of entries from 
typedef union tagged {
    ROBTag Tag;
    Data Imm; // ugh
} Operand deriving (Bits, Eq);


typedef Bit#(4) ROBTag;

typedef struct {
        InstrExt op;
        ROBTag tag;
        Operand op1;
        Operand op2;
        Addr pc;
        Epoch epoch;
        } RSEntry deriving (Bits, Eq);

typedef struct {
        Maybe#(Data) data;
        Addr pc;
        Maybe#(Addr) mispredict;
        WBReg dest;
        Epoch epoch;
        } ROBEntry deriving (Bits, Eq);

typedef struct {
        Maybe#(Data) data;
        ROBTag tag;
        Epoch epoch;
        } CDBPacket deriving (Bits, Eq);    

typedef union tagged {
  Rindx ArchReg;
  CP0indx SpecReg;
} WBReg deriving (Bits, Eq);

typedef union tagged
{
  struct {  } LW;
  struct {  } SW;

  struct { } ADD;
  struct { } SUB;
  struct { } SLT;
  struct { } SLTU;
  struct { } AND;
  struct { } OR;
  struct { } XOR;
  struct { } NOR;
  struct { } LUI;

  struct { } SLL;
  struct { } SRL;
  struct { } SRA;

  struct { Target target; } J;
  struct { Target target; } JAL;
  struct {  } JR;
  struct {  } JALR;

  struct { Simm offset;  } BEQ;
  struct { Simm offset;  } BNE;
  struct { } BLEZ;
  struct { } BGTZ;
  struct { } BLTZ;
  struct { } BGEZ;

  struct { } MFC0;
  struct { CP0indx cop0dst; } MTC0;

  void        ILLEGAL;

} InstrExt deriving (Bits, Eq);

instance FShow#(InstrExt);
  function Fmt fshow (InstrExt op);
    case (op) matches
      tagged LW .it: return fshow("LW");
      tagged SW .it: return fshow("SW");
      tagged ADD .it: return fshow("ADD");
      tagged SUB .it: return fshow("SUB");
      tagged SLT .it: return fshow("SLT");
      tagged SLTU .it: return fshow("SLTU");
      tagged AND .it: return fshow("AND");
      tagged OR .it: return fshow("OR");
      tagged XOR .it: return fshow("XOR");
      tagged NOR .it: return fshow("NOR");
      tagged LUI .it: return fshow("LUI");
      tagged SLL .it: return fshow("SLL");
      tagged SRL .it: return fshow("SRL");
      tagged SRA .it: return fshow("SRA");
      tagged J .it: return fshow("J");
      tagged JAL .it: return fshow("JAL");
      tagged JR .it: return fshow("JR");
      tagged JALR .it: return fshow("JALR");
      tagged BEQ .it: return fshow("BEQ");
      tagged BNE .it: return fshow("BNE");
      tagged BLEZ .it: return fshow("BLEZ");
      tagged BGTZ .it: return fshow("BGTZ");
      tagged BLTZ .it: return fshow("BLTZ");
      tagged BGEZ .it: return fshow("BGEZ");
      tagged MFC0 .it: return fshow("MFC0");
      tagged MTC0 .it: return fshow("MTC0");
      tagged ILLEGAL .it: return fshow("ILLEGAL");
    endcase
  endfunction
endinstance

typedef struct {
    InstrExt  op;
    Data      op1;
    Data      op2;
    ROBTag    tag;
    Addr      pc;
    Epoch     epoch;
} ALUReq deriving (Eq, Bits);

typedef struct {
    InstrExt  op;
    Data   data;
    ROBTag tag;
    Addr   pc;
    Addr   next_pc;
    Epoch  epoch;
} ALUResp deriving(Eq,Bits);                

// operation types/classes
typedef enum {ALU_OP, MEM_OP, JB_OP, MTC0_OP} Op_type deriving(Eq);

// determine instruction type from instruction tag
function Op_type instr_type( Instr inst );
    case ( inst ) matches
        tagged LW    .it : return MEM_OP;
        tagged SW    .it : return MEM_OP;
        tagged ADDIU .it : return ALU_OP;
        tagged SLTI  .it : return ALU_OP;
        tagged SLTIU .it : return ALU_OP;
        tagged ANDI  .it : return ALU_OP;
        tagged ORI   .it : return ALU_OP;
        tagged XORI  .it : return ALU_OP;
        tagged LUI   .it : return ALU_OP;
        tagged SLL   .it : return ALU_OP;
        tagged SRL   .it : return ALU_OP;
        tagged SRA   .it : return ALU_OP;
        tagged SLLV  .it : return ALU_OP;
        tagged SRLV  .it : return ALU_OP;
        tagged SRAV  .it : return ALU_OP;
        tagged ADDU  .it : return ALU_OP;
        tagged SUBU  .it : return ALU_OP;
        tagged AND   .it : return ALU_OP;
        tagged OR    .it : return ALU_OP;
        tagged XOR   .it : return ALU_OP;
        tagged NOR   .it : return ALU_OP;
        tagged SLT   .it : return ALU_OP;
        tagged SLTU  .it : return ALU_OP;
        tagged J     .it : return JB_OP;
        tagged JAL   .it : return JB_OP;
        tagged JR    .it : return JB_OP;
        tagged JALR  .it : return JB_OP;
        tagged BEQ   .it : return JB_OP;
        tagged BNE   .it : return JB_OP;
        tagged BLEZ  .it : return JB_OP;
        tagged BGTZ  .it : return JB_OP;
        tagged BLTZ  .it : return JB_OP;
        tagged BGEZ  .it : return JB_OP;
        tagged MFC0  .it : return ALU_OP;
        tagged MTC0  .it : return ALU_OP; // handle this separately
        tagged ILLEGAL   : return ?;
        default          : return ?;
    endcase
endfunction 

// determine instruction type from InstrExt 
// this duplicates instr_type functionality
// used in alu_compl
function Op_type instr_ext_type( InstrExt inst );
    case ( inst ) matches
        tagged ADD   .it : return ALU_OP;
        tagged SLT   .it : return ALU_OP;
        tagged SLTU  .it : return ALU_OP;
        tagged AND   .it : return ALU_OP;
        tagged OR    .it : return ALU_OP;
        tagged XOR   .it : return ALU_OP;
        tagged LUI   .it : return ALU_OP;
        tagged SLL   .it : return ALU_OP;
        tagged SRL   .it : return ALU_OP;
        tagged SRA   .it : return ALU_OP;
        tagged NOR   .it : return ALU_OP;
        tagged J     .it : return JB_OP;
        tagged JAL   .it : return JB_OP;
        tagged JR    .it : return JB_OP;
        tagged JALR  .it : return JB_OP;
        tagged BEQ   .it : return JB_OP;
        tagged BNE   .it : return JB_OP;
        tagged BLEZ  .it : return JB_OP;
        tagged BGTZ  .it : return JB_OP;
        tagged BLTZ  .it : return JB_OP;
        tagged BGEZ  .it : return JB_OP;
        tagged MFC0  .it : return ALU_OP;
        tagged MTC0  .it : return ALU_OP;
        tagged ILLEGAL   : return ?;
        default          : return ?;
    endcase
endfunction 


// return instruction destination
function WBReg instr_dest(Instr inst);
    case ( inst ) matches
        tagged LW    .it :        return tagged ArchReg (it.rdst);
        tagged ADDIU .it :        return tagged ArchReg (it.rdst);
        tagged SLTI  .it :        return tagged ArchReg (it.rdst);
        tagged SLTIU .it :        return tagged ArchReg (it.rdst);
        tagged ANDI  .it :        return tagged ArchReg (it.rdst);
        tagged ORI   .it :        return tagged ArchReg (it.rdst);
        tagged XORI  .it :        return tagged ArchReg (it.rdst);
        tagged LUI   .it :        return tagged ArchReg (it.rdst);
        tagged SLL   .it :        return tagged ArchReg (it.rdst);
        tagged SRL   .it :        return tagged ArchReg (it.rdst);
        tagged SRA   .it :        return tagged ArchReg (it.rdst);
        tagged SLLV  .it :        return tagged ArchReg (it.rdst);
        tagged SRLV  .it :        return tagged ArchReg (it.rdst);
        tagged SRAV  .it :        return tagged ArchReg (it.rdst);
        tagged ADDU  .it :        return tagged ArchReg (it.rdst);
        tagged SUBU  .it :        return tagged ArchReg (it.rdst);
        tagged AND   .it :        return tagged ArchReg (it.rdst);
        tagged OR    .it :        return tagged ArchReg (it.rdst);
        tagged XOR   .it :        return tagged ArchReg (it.rdst);
        tagged NOR   .it :        return tagged ArchReg (it.rdst);
        tagged SLT   .it :        return tagged ArchReg (it.rdst);
        tagged SLTU  .it :        return tagged ArchReg (it.rdst);
        tagged JAL   .it :        return tagged ArchReg (31);
        tagged JALR  .it :        return tagged ArchReg (it.rdst);
        tagged MFC0  .it :        return tagged ArchReg (it.rdst);
        tagged MTC0  .it :        return tagged SpecReg (it.cop0dst);
        default :                 return tagged ArchReg 0;
    endcase
endfunction     

//function OpCode get_opcode(Instr inst) = instrBits[ 31 : 26];

//-----------------------------------------------------------
// Helper functions
//-----------------------------------------------------------

function Bit#(32) slt( Bit#(32) val1, Bit#(32) val2 );
    return zeroExtend( pack( signedLT(val1,val2) ) );
endfunction

function Bit#(32) sltu( Bit#(32) val1, Bit#(32) val2 );
    return zeroExtend( pack( val1 < val2 ) );
endfunction

function Bit#(32) rshft( Bit#(32) val );
    return zeroExtend(val[4:0]);
endfunction
