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

typedef Bit#(4) ROBTag;

typedef Bit#(4) OpCode;
typedef Bit#(4) Operand;
typedef Bit#(18) Addr;

typedef struct {
		OpCode op;
		ROBTag tag;
		Bool full;
		Operand op1;
		Operand op2;
		} RSEntry deriving (Bits, Eq);

typedef struct {
		Maybe#(Data) data;
                Maybe#(Addr) mispredict;
		Rindx dest;
		Epoch epoch;
		} ROBEntry deriving (Bits, Eq);

typedef struct {
		Maybe#(Data) data;
		ROBTag tag;
		Epoch epoch;
		} CDBPacket deriving (Bits, Eq);

interface RegisterMap;
    method Action   wr( Rindx rindx, Bit#(32) data );
    method Bit#(32) rd1( Rindx rindx );
    method Bit#(32) rd2( Rindx rindx );
endinterface
