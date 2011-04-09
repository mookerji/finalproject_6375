typedef Rindx ROBTag;

typedef union tagged {
		      Data data;
		      ROBTag tag;
		      } RMap deriving (Bits, Eq);

typedef RMap Operand;

typedef struct {
		OpCode op;
		ROBTag tag;
		Bool full;		
		Operand op1;
		Operand op2;
		} RSEntry deriving (Bits, Eq);

typedef struct {
		Maybe#(Data) data;
		Rindx dest;
		Epoch epoch;
		} ROBEntry deriving (Bits, Eq);

typedef struct {
		Maybe#(Data) data;
		ROBTag tag;
		Epoch epoch;
		} CDBPacket deriving (Bits, Eq);