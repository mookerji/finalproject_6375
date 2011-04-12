
import Trace::*;

//----------------------------------------------------------------------
// Other typedefs
//----------------------------------------------------------------------

typedef Bit#(32) Addr;
typedef Int#(18) Stat;

//----------------------------------------------------------------------
// Basic instruction type
//----------------------------------------------------------------------

typedef Bit#(5)  Rindx;
typedef Bit#(16) Simm;
typedef Bit#(16) Zimm;
typedef Bit#(8) Epoch;
typedef Bit#(5)  Shamt;
typedef Bit#(26) Target;
typedef Bit#(5)  CP0indx;
typedef Bit#(32) Data;

//----------------------------------------------------------------------
// Pipeline typedefs
//----------------------------------------------------------------------

typedef union tagged                
{
  // Memory operations
  struct { Rindx rbase; Rindx rdst;  Simm offset;  } LW;
  struct { Rindx rbase; Rindx rsrc;  Simm offset;  } SW; 

  // Arithmetic operations
  struct { Rindx rsrc;  Rindx rdst;  Simm imm;     } ADDIU;
  struct { Rindx rsrc;  Rindx rdst;  Simm imm;     } SLTI;
  struct { Rindx rsrc;  Rindx rdst;  Simm imm;     } SLTIU;
  struct { Rindx rsrc;  Rindx rdst;  Zimm imm;     } ANDI;
  struct { Rindx rsrc;  Rindx rdst;  Zimm imm;     } ORI;
  struct { Rindx rsrc;  Rindx rdst;  Zimm imm;     } XORI;
  struct {              Rindx rdst;  Zimm imm;     } LUI;

  struct { Rindx rsrc;  Rindx rdst;  Shamt shamt;  } SLL;
  struct { Rindx rsrc;  Rindx rdst;  Shamt shamt;  } SRL;
  struct { Rindx rsrc;  Rindx rdst;  Shamt shamt;  } SRA;
  struct { Rindx rsrc;  Rindx rdst;  Rindx rshamt; } SLLV;
  struct { Rindx rsrc;  Rindx rdst;  Rindx rshamt; } SRLV;
  struct { Rindx rsrc;  Rindx rdst;  Rindx rshamt; } SRAV;
  struct { Rindx rsrc1; Rindx rsrc2; Rindx rdst;   } ADDU;
  struct { Rindx rsrc1; Rindx rsrc2; Rindx rdst;   } SUBU;
  struct { Rindx rsrc1; Rindx rsrc2; Rindx rdst;   } AND;
  struct { Rindx rsrc1; Rindx rsrc2; Rindx rdst;   } OR;
  struct { Rindx rsrc1; Rindx rsrc2; Rindx rdst;   } XOR;
  struct { Rindx rsrc1; Rindx rsrc2; Rindx rdst;   } NOR;
  struct { Rindx rsrc1; Rindx rsrc2; Rindx rdst;   } SLT;
  struct { Rindx rsrc1; Rindx rsrc2; Rindx rdst;   } SLTU;

  // Jump and branch operations
  struct { Target target;                          } J;
  struct { Target target;                          } JAL;
  struct { Rindx rsrc;                             } JR;
  struct { Rindx rsrc;  Rindx rdst;                } JALR;
  struct { Rindx rsrc1; Rindx rsrc2; Simm offset;  } BEQ;
  struct { Rindx rsrc1; Rindx rsrc2; Simm offset;  } BNE;
  struct { Rindx rsrc;  Simm offset;               } BLEZ;
  struct { Rindx rsrc;  Simm offset;               } BGTZ;
  struct { Rindx rsrc;  Simm offset;               } BLTZ;
  struct { Rindx rsrc;  Simm offset;               } BGEZ;

  struct { Rindx rdst;  CP0indx cop0src;           } MFC0;
  struct { Rindx rsrc;  CP0indx cop0dst;           } MTC0; 

  void                                               ILLEGAL;

}
Instr deriving(Eq);

typedef union tagged {
  struct {Rindx dst;} WBMem;
  struct {Rindx dst; Bit#(32) data;} WBAlu;
  void WBnothing;
} WBResult deriving(Eq, Bits);

//----------------------------------------------------------------------
// Pack and Unpack
//----------------------------------------------------------------------

Bit#(6) opFUNC  = 6'b000000;  Bit#(6) fcSLL   = 6'b000000;
Bit#(6) opRT    = 6'b000001;  Bit#(6) fcSRL   = 6'b000010;
Bit#(6) opRS    = 6'b010000;  Bit#(6) fcSRA   = 6'b000011;
                              Bit#(6) fcSLLV  = 6'b000100;
Bit#(6) opLW    = 6'b100011;  Bit#(6) fcSRLV  = 6'b000110;
Bit#(6) opSW    = 6'b101011;  Bit#(6) fcSRAV  = 6'b000111;
                              Bit#(6) fcADDU  = 6'b100001;
Bit#(6) opADDIU = 6'b001001;  Bit#(6) fcSUBU  = 6'b100011;
Bit#(6) opSLTI  = 6'b001010;  Bit#(6) fcAND   = 6'b100100;
Bit#(6) opSLTIU = 6'b001011;  Bit#(6) fcOR    = 6'b100101;
Bit#(6) opANDI  = 6'b001100;  Bit#(6) fcXOR   = 6'b100110;
Bit#(6) opORI   = 6'b001101;  Bit#(6) fcNOR   = 6'b100111;
Bit#(6) opXORI  = 6'b001110;  Bit#(6) fcSLT   = 6'b101010;
Bit#(6) opLUI   = 6'b001111;  Bit#(6) fcSLTU  = 6'b101011;

Bit#(6) opJ     = 6'b000010;
Bit#(6) opJAL   = 6'b000011;
Bit#(6) fcJR    = 6'b001000;
Bit#(6) fcJALR  = 6'b001001;
Bit#(6) opBEQ   = 6'b000100;
Bit#(6) opBNE   = 6'b000101;
Bit#(6) opBLEZ  = 6'b000110;
Bit#(6) opBGTZ  = 6'b000111;
Bit#(5) rtBLTZ  = 5'b00000;
Bit#(5) rtBGEZ  = 5'b00001;

Bit#(5) rsMFC0  = 5'b00000;
Bit#(5) rsMTC0  = 5'b00100;

instance Bits#(Instr,32);

  // Pack Function

  function Bit#(32) pack( Instr instr );

    case ( instr ) matches

      tagged LW    .it : return { opLW,    it.rbase, it.rdst,  it.offset };
      tagged SW    .it : return { opSW,    it.rbase, it.rsrc,  it.offset };

      tagged ADDIU .it : return { opADDIU, it.rsrc,  it.rdst,  it.imm                      }; 
      tagged SLTI  .it : return { opSLTI,  it.rsrc,  it.rdst,  it.imm                      }; 
      tagged SLTIU .it : return { opSLTIU, it.rsrc,  it.rdst,  it.imm                      }; 
      tagged ANDI  .it : return { opANDI,  it.rsrc,  it.rdst,  it.imm                      }; 
      tagged ORI   .it : return { opORI,   it.rsrc,  it.rdst,  it.imm                      }; 
      tagged XORI  .it : return { opXORI,  it.rsrc,  it.rdst,  it.imm                      }; 
      tagged LUI   .it : return { opLUI,   5'b0,     it.rdst,  it.imm                      };

      tagged SLL   .it : return { opFUNC,  5'b0,     it.rsrc,  it.rdst,   it.shamt, fcSLL  }; 
      tagged SRL   .it : return { opFUNC,  5'b0,     it.rsrc,  it.rdst,   it.shamt, fcSRL  }; 
      tagged SRA   .it : return { opFUNC,  5'b0,     it.rsrc,  it.rdst,   it.shamt, fcSRA  }; 

      tagged SLLV  .it : return { opFUNC,  it.rshamt, it.rsrc, it.rdst,   5'b0,     fcSLLV }; 
      tagged SRLV  .it : return { opFUNC,  it.rshamt, it.rsrc, it.rdst,   5'b0,     fcSRLV }; 
      tagged SRAV  .it : return { opFUNC,  it.rshamt, it.rsrc, it.rdst,   5'b0,     fcSRAV }; 

      tagged ADDU  .it : return { opFUNC,  it.rsrc1, it.rsrc2, it.rdst,   5'b0,     fcADDU }; 
      tagged SUBU  .it : return { opFUNC,  it.rsrc1, it.rsrc2, it.rdst,   5'b0,     fcSUBU }; 
      tagged AND   .it : return { opFUNC,  it.rsrc1, it.rsrc2, it.rdst,   5'b0,     fcAND  }; 
      tagged OR    .it : return { opFUNC,  it.rsrc1, it.rsrc2, it.rdst,   5'b0,     fcOR   }; 
      tagged XOR   .it : return { opFUNC,  it.rsrc1, it.rsrc2, it.rdst,   5'b0,     fcXOR  }; 
      tagged NOR   .it : return { opFUNC,  it.rsrc1, it.rsrc2, it.rdst,   5'b0,     fcNOR  }; 
      tagged SLT   .it : return { opFUNC,  it.rsrc1, it.rsrc2, it.rdst,   5'b0,     fcSLT  }; 
      tagged SLTU  .it : return { opFUNC,  it.rsrc1, it.rsrc2, it.rdst,   5'b0,     fcSLTU }; 

      tagged J     .it : return { opJ,     it.target                                       }; 
      tagged JAL   .it : return { opJAL,   it.target                                       }; 
      tagged JR    .it : return { opFUNC,  it.rsrc,  5'b0,     5'b0,      5'b0,     fcJR   };
      tagged JALR  .it : return { opFUNC,  it.rsrc,  5'b0,     it.rdst,   5'b0,     fcJALR };
      tagged BEQ   .it : return { opBEQ,   it.rsrc1, it.rsrc2, it.offset                   }; 
      tagged BNE   .it : return { opBNE,   it.rsrc1, it.rsrc2, it.offset                   }; 
      tagged BLEZ  .it : return { opBLEZ,  it.rsrc,  5'b0,     it.offset                   }; 
      tagged BGTZ  .it : return { opBGTZ,  it.rsrc,  5'b0,     it.offset                   }; 
      tagged BLTZ  .it : return { opRT,    it.rsrc,  rtBLTZ,   it.offset                   }; 
      tagged BGEZ  .it : return { opRT,    it.rsrc,  rtBGEZ,   it.offset                   }; 

      tagged MFC0  .it : return { opRS,    rsMFC0,   it.rdst,  it.cop0src, 11'b0           }; 
      tagged MTC0  .it : return { opRS,    rsMTC0,   it.rsrc,  it.cop0dst, 11'b0           };  

    endcase

  endfunction

  // Unpack Function

  function Instr unpack( Bit#(32) instrBits );

    let opcode = instrBits[ 31 : 26 ];
    let rs     = instrBits[ 25 : 21 ];
    let rt     = instrBits[ 20 : 16 ];
    let rd     = instrBits[ 15 : 11 ];
    let shamt  = instrBits[ 10 :  6 ];
    let funct  = instrBits[  5 :  0 ];
    let imm    = instrBits[ 15 :  0 ];
    let target = instrBits[ 25 :  0 ];

    case ( opcode )

      opLW        : return LW    { rbase:rs, rdst:rt,  offset:imm  };
      opSW        : return SW    { rbase:rs, rsrc:rt,  offset:imm  };
      opADDIU     : return ADDIU { rsrc:rs,  rdst:rt,  imm:imm     };
      opSLTI      : return SLTI  { rsrc:rs,  rdst:rt,  imm:imm     };
      opSLTIU     : return SLTIU { rsrc:rs,  rdst:rt,  imm:imm     };
      opANDI      : return ANDI  { rsrc:rs,  rdst:rt,  imm:imm     };
      opORI       : return ORI   { rsrc:rs,  rdst:rt,  imm:imm     };
      opXORI      : return XORI  { rsrc:rs,  rdst:rt,  imm:imm     };
      opLUI       : return LUI   {           rdst:rt,  imm:imm     };
      opJ         : return J     { target:target                   };
      opJAL       : return JAL   { target:target                   };
      opBEQ       : return BEQ   { rsrc1:rs, rsrc2:rt, offset:imm  };
      opBNE       : return BNE   { rsrc1:rs, rsrc2:rt, offset:imm  };
      opBLEZ      : return BLEZ  { rsrc:rs,  offset:imm            };
      opBGTZ      : return BGTZ  { rsrc:rs,  offset:imm            };

      opFUNC  : 
        case ( funct )
          fcSLL   : return SLL   { rsrc:rt,  rdst:rd,  shamt:shamt };
          fcSRL   : return SRL   { rsrc:rt,  rdst:rd,  shamt:shamt };
          fcSRA   : return SRA   { rsrc:rt,  rdst:rd,  shamt:shamt };
          fcSLLV  : return SLLV  { rsrc:rt,  rdst:rd,  rshamt:rs   };
          fcSRLV  : return SRLV  { rsrc:rt,  rdst:rd,  rshamt:rs   };
          fcSRAV  : return SRAV  { rsrc:rt,  rdst:rd,  rshamt:rs   };
          fcADDU  : return ADDU  { rsrc1:rs, rsrc2:rt, rdst:rd     };
          fcSUBU  : return SUBU  { rsrc1:rs, rsrc2:rt, rdst:rd     };
          fcAND   : return AND   { rsrc1:rs, rsrc2:rt, rdst:rd     };
          fcOR    : return OR    { rsrc1:rs, rsrc2:rt, rdst:rd     };
          fcXOR   : return XOR   { rsrc1:rs, rsrc2:rt, rdst:rd     };
          fcNOR   : return NOR   { rsrc1:rs, rsrc2:rt, rdst:rd     };
          fcSLT   : return SLT   { rsrc1:rs, rsrc2:rt, rdst:rd     }; 
          fcSLTU  : return SLTU  { rsrc1:rs, rsrc2:rt, rdst:rd     };
          fcJR    : return JR    { rsrc:rs                         };
          fcJALR  : return JALR  { rsrc:rs,  rdst:rd               };
          default : return ILLEGAL;
        endcase

      opRT : 
        case ( rt )
          rtBLTZ  : return BLTZ  { rsrc:rs,  offset:imm            };
          rtBGEZ  : return BGEZ  { rsrc:rs,  offset:imm            };
          default : return ILLEGAL;
        endcase

      opRS : 
        case ( rs )
          rsMFC0  : return MFC0  { rdst:rt,  cop0src:rd            };
          rsMTC0  : return MTC0  { rsrc:rt,  cop0dst:rd            };
          default : return ILLEGAL;
        endcase

      default : return ILLEGAL;
      
    endcase

  endfunction

endinstance

//----------------------------------------------------------------------
// Trace
//----------------------------------------------------------------------

instance Traceable#(Instr);

  function Action traceTiny( String loc, String ttag, Instr inst );
    case ( inst ) matches

      tagged LW    .it : $fdisplay(stderr,  " => %s:%s lw", loc,   ttag );
      tagged SW    .it : $fdisplay(stderr,  " => %s:%s sw", loc,   ttag );

      tagged ADDIU .it : $fdisplay(stderr,  " => %s:%s addi", loc, ttag );
      tagged SLTI  .it : $fdisplay(stderr,  " => %s:%s sli", loc,  ttag );
      tagged SLTIU .it : $fdisplay(stderr,  " => %s:%s sliu", loc, ttag );
      tagged ANDI  .it : $fdisplay(stderr,  " => %s:%s andi", loc, ttag );
      tagged ORI   .it : $fdisplay(stderr,  " => %s:%s ori", loc,  ttag );
      tagged XORI  .it : $fdisplay(stderr,  " => %s:%s xori", loc, ttag );
      tagged LUI   .it : $fdisplay(stderr,  " => %s:%s lui", loc,  ttag );
                                          
      tagged SLL   .it : $fdisplay(stderr,  " => %s:%s sll", loc,  ttag );
      tagged SRL   .it : $fdisplay(stderr,  " => %s:%s srl", loc,  ttag );
      tagged SRA   .it : $fdisplay(stderr,  " => %s:%s sra", loc,  ttag );
      tagged SLLV  .it : $fdisplay(stderr,  " => %s:%s sllv", loc, ttag );
      tagged SRLV  .it : $fdisplay(stderr,  " => %s:%s srlv", loc, ttag );
      tagged SRAV  .it : $fdisplay(stderr,  " => %s:%s srav", loc, ttag );
                                          
      tagged ADDU  .it : $fdisplay(stderr,  " => %s:%s addu", loc, ttag );
      tagged SUBU  .it : $fdisplay(stderr,  " => %s:%s subu", loc, ttag );
      tagged AND   .it : $fdisplay(stderr,  " => %s:%s and", loc,  ttag );
      tagged OR    .it : $fdisplay(stderr,  " => %s:%s or", loc,   ttag );
      tagged XOR   .it : $fdisplay(stderr,  " => %s:%s xor", loc,  ttag );
      tagged NOR   .it : $fdisplay(stderr,  " => %s:%s nor", loc,  ttag );
      tagged SLT   .it : $fdisplay(stderr,  " => %s:%s slt", loc,  ttag );
      tagged SLTU  .it : $fdisplay(stderr,  " => %s:%s sltu", loc, ttag );
                                          
      tagged J     .it : $fdisplay(stderr,  " => %s:%s j", loc,    ttag );
      tagged JAL   .it : $fdisplay(stderr,  " => %s:%s jal", loc,  ttag );
      tagged JR    .it : $fdisplay(stderr,  " => %s:%s jr", loc,   ttag );
      tagged JALR  .it : $fdisplay(stderr,  " => %s:%s jalr", loc, ttag );
      tagged BEQ   .it : $fdisplay(stderr,  " => %s:%s beq", loc,  ttag );
      tagged BNE   .it : $fdisplay(stderr,  " => %s:%s bne", loc,  ttag );
      tagged BLEZ  .it : $fdisplay(stderr,  " => %s:%s blez", loc, ttag );
      tagged BGTZ  .it : $fdisplay(stderr,  " => %s:%s bgtz", loc, ttag );
      tagged BLTZ  .it : $fdisplay(stderr,  " => %s:%s bltz", loc, ttag );
      tagged BGEZ  .it : $fdisplay(stderr,  " => %s:%s bgez", loc, ttag );
                                           
      tagged MFC0  .it : $fdisplay(stderr,  " => %s:%s mfc0", loc, ttag );
      tagged MTC0  .it : $fdisplay(stderr,  " => %s:%s mtc0", loc, ttag );

      tagged ILLEGAL   : $fdisplay(stderr,  " => %s:%s ill", loc,  ttag );

    endcase
  endfunction

  function Action traceFull( String loc, String ttag, Instr inst );
    case ( inst ) matches

      tagged LW    .it : $fdisplay(stderr,  " => %s:%s lw r%0d, 0x%x(r%0d)", loc, ttag, it.rdst, it.offset, it.rbase );
      tagged SW    .it : $fdisplay(stderr,  " => %s:%s sw r%0d, 0x%x(r%0d)", loc, ttag, it.rsrc, it.offset, it.rbase );

      tagged ADDIU .it : $fdisplay(stderr,  " => %s:%s addiu r%0d, r%0d, 0x%x", loc, ttag, it.rdst, it.rsrc, it.imm );
      tagged SLTI  .it : $fdisplay(stderr,  " => %s:%s slti r%0d, r%0d, 0x%x", loc,  ttag, it.rdst, it.rsrc, it.imm );
      tagged SLTIU .it : $fdisplay(stderr,  " => %s:%s sltiu r%0d, r%0d, 0x%x", loc, ttag, it.rdst, it.rsrc, it.imm );
      tagged ANDI  .it : $fdisplay(stderr,  " => %s:%s andi r%0d, r%0d, 0x%x", loc,  ttag, it.rdst, it.rsrc, it.imm );
      tagged ORI   .it : $fdisplay(stderr,  " => %s:%s ori r%0d, r%0d, 0x%x", loc,   ttag, it.rdst, it.rsrc, it.imm );
      tagged XORI  .it : $fdisplay(stderr,  " => %s:%s xori r%0d, r%0d, 0x%x", loc,  ttag, it.rdst, it.rsrc, it.imm );
      tagged LUI   .it : $fdisplay(stderr,  " => %s:%s lui r%0d, 0x%x", loc,         ttag, it.rdst, it.imm );
                                      
      tagged SLL   .it : $fdisplay(stderr,  " => %s:%s sll r%0d, r%0d, %0d", loc,   ttag, it.rdst, it.rsrc, it.shamt );
      tagged SRL   .it : $fdisplay(stderr,  " => %s:%s srl r%0d, r%0d, %0d", loc,   ttag, it.rdst, it.rsrc, it.shamt );
      tagged SRA   .it : $fdisplay(stderr,  " => %s:%s sra r%0d, r%0d, %0d", loc,   ttag, it.rdst, it.rsrc, it.shamt );
      tagged SLLV  .it : $fdisplay(stderr,  " => %s:%s sllv r%0d, r%0d, r%0d", loc, ttag, it.rdst, it.rsrc, it.rshamt );
      tagged SRLV  .it : $fdisplay(stderr,  " => %s:%s srlv r%0d, r%0d, r%0d", loc, ttag, it.rdst, it.rsrc, it.rshamt );
      tagged SRAV  .it : $fdisplay(stderr,  " => %s:%s srav r%0d, r%0d, r%0d", loc, ttag, it.rdst, it.rsrc, it.rshamt );
                                      
      tagged ADDU  .it : $fdisplay(stderr,  " => %s:%s addu r%0d, r%0d, r%0d", loc, ttag, it.rdst, it.rsrc1, it.rsrc2 );
      tagged SUBU  .it : $fdisplay(stderr,  " => %s:%s subu r%0d, r%0d, r%0d", loc, ttag, it.rdst, it.rsrc1, it.rsrc2 );
      tagged AND   .it : $fdisplay(stderr,  " => %s:%s and r%0d, r%0d, r%0d", loc,  ttag, it.rdst, it.rsrc1, it.rsrc2 );
      tagged OR    .it : $fdisplay(stderr,  " => %s:%s or r%0d, r%0d, r%0d", loc,   ttag, it.rdst, it.rsrc1, it.rsrc2 );
      tagged XOR   .it : $fdisplay(stderr,  " => %s:%s xor r%0d, r%0d, r%0d", loc,  ttag, it.rdst, it.rsrc1, it.rsrc2 );
      tagged NOR   .it : $fdisplay(stderr,  " => %s:%s nor r%0d, r%0d, r%0d", loc,  ttag, it.rdst, it.rsrc1, it.rsrc2 );
      tagged SLT   .it : $fdisplay(stderr,  " => %s:%s slt r%0d, r%0d, r%0d", loc,  ttag, it.rdst, it.rsrc1, it.rsrc2 );
      tagged SLTU  .it : $fdisplay(stderr,  " => %s:%s sltu r%0d, r%0d, r%0d", loc, ttag, it.rdst, it.rsrc1, it.rsrc2 );
                                      
      tagged J     .it : $fdisplay(stderr,  " => %s:%s j 0x%x", loc,    ttag, it.target );
      tagged JAL   .it : $fdisplay(stderr,  " => %s:%s jal 0x%x", loc,  ttag, it.target );
      tagged JR    .it : $fdisplay(stderr,  " => %s:%s jr r%0d", loc,   ttag, it.rsrc );
      tagged JALR  .it : $fdisplay(stderr,  " => %s:%s jalr r%0d", loc, ttag, it.rsrc );
      tagged BEQ   .it : $fdisplay(stderr,  " => %s:%s beq r%0d, r%0d, 0x%x", loc, ttag, it.rsrc1, it.rsrc2, it.offset );
      tagged BNE   .it : $fdisplay(stderr,  " => %s:%s bne r%0d, r%0d, 0x%x", loc, ttag, it.rsrc1, it.rsrc2, it.offset );
      tagged BLEZ  .it : $fdisplay(stderr,  " => %s:%s blez r%0d, 0x%x", loc, ttag, it.rsrc, it.offset );
      tagged BGTZ  .it : $fdisplay(stderr,  " => %s:%s bgtz r%0d, 0x%x", loc, ttag, it.rsrc, it.offset );
      tagged BLTZ  .it : $fdisplay(stderr,  " => %s:%s bltz r%0d, 0x%x", loc, ttag, it.rsrc, it.offset );
      tagged BGEZ  .it : $fdisplay(stderr,  " => %s:%s bgez r%0d, 0x%x", loc, ttag, it.rsrc, it.offset );
                                      
      tagged MFC0  .it : $fdisplay(stderr,  " => %s:%s mfc0 r%0d, cpr%0d", loc, ttag, it.rdst, it.cop0src );
      tagged MTC0  .it : $fdisplay(stderr,  " => %s:%s mtc0 r%0d, cpr%0d", loc, ttag, it.rsrc, it.cop0dst );

      tagged ILLEGAL   : $fdisplay(stderr,  " => %s:%s illegal instruction", loc, ttag );

    endcase
  endfunction

endinstance

