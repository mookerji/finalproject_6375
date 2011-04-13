//----------------------------------------------------------------------
// Basic instruction type
//----------------------------------------------------------------------

import ProcTypes::*;

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
        Bool full;
        Operand op1;
        Operand op2;
        } RSEntry deriving (Bits, Eq);

typedef struct {
        Maybe#(Data) data;
        Maybe#(Tuple2#(Addr,Addr)) mispredict;
        Rindx dest;
        Epoch epoch;
        } ROBEntry deriving (Bits, Eq);

typedef struct {
        Maybe#(Data) data;
        ROBTag tag;
        Epoch epoch;
        } CDBPacket deriving (Bits, Eq);    

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
  struct { } MTC0;

  void        ILLEGAL;

} InstrExt deriving (Bits, Eq);

typedef struct {
    InstrExt  op;
    Data      op1;
    Data      op2;
    ROBTag    tag;
} ALUReq deriving (Eq, Bits);

typedef struct {
  Data   data;
  ROBTag tag;
} ALUResp deriving(Eq,Bits);                

// operation types/classes
typedef enum {ALU_OP, MEM_OP, JB_OP} Op_type deriving(Eq);

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
        tagged MTC0  .it : return ALU_OP;
        tagged ILLEGAL   : return ?;
        default          : return ?;
    endcase
endfunction 

// return instruction destination
function Maybe#(Rindx) instr_dest(Instr inst);
    case ( inst ) matches
        tagged LW    .it :        return Valid (it.rdst);
        tagged ADDIU .it :        return Valid (it.rdst);
        tagged SLTI  .it :        return Valid (it.rdst);
        tagged SLTIU .it :        return Valid (it.rdst);
        tagged ANDI  .it :        return Valid (it.rdst);
        tagged ORI   .it :        return Valid (it.rdst);
        tagged XORI  .it :        return Valid (it.rdst);
        tagged LUI   .it :        return Valid (it.rdst);
        tagged SLL   .it :        return Valid (it.rdst);
        tagged SRL   .it :        return Valid (it.rdst);
        tagged SRA   .it :        return Valid (it.rdst);
        tagged SLLV  .it :        return Valid (it.rdst);
        tagged SRLV  .it :        return Valid (it.rdst);
        tagged SRAV  .it :        return Valid (it.rdst);
        tagged ADDU  .it :        return Valid (it.rdst);
        tagged SUBU  .it :        return Valid (it.rdst);
        tagged AND   .it :        return Valid (it.rdst);
        tagged OR    .it :        return Valid (it.rdst);
        tagged XOR   .it :        return Valid (it.rdst);
        tagged NOR   .it :        return Valid (it.rdst);
        tagged SLT   .it :        return Valid (it.rdst);
        tagged SLTU  .it :        return Valid (it.rdst);
        tagged JAL   .it :        return Valid (31);
        tagged JALR  .it :        return Valid (it.rdst);
        tagged MFC0  .it :        return Valid (it.rdst);
        tagged MTC0  .it :        return Valid (it.cop0dst);
        default :                 return Invalid;
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
