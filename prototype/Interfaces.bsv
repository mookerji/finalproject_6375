
typedef Bit#(5) RenameIndex;
typedef struct {Rindx reg, RenameIndex ren} RenamedRindx;

// This allows us to bypass a value to all of the waiting units
interface CommonDataBus;
  method Action put(RenamedRindx, Data data);
  method Maybe#(Tuple2#(RenamedRindx, Data)) get();
endinterface

typedef union tagged {
  RenamedRindx future;
  Data imm;
} FutureOrImm deriving (Bits, Eq);

typedef struct {
  Maybe#(RenamedRindx) dst;
  FutureOrImm src1;
  Maybe#(FutureOrImm) src2;
  OpCode op;
} ReservationEntry deriving (Bits, Eq);

typedef struct {
  Data src1;
  Data src2;
  RenamedRindx dst;
  OpCode op;
} ReadyOperation deriving (Bits, Eq);

typedef struct {
  Maybe#(Data) value; //nonvoid when value returns
  Maybe#(RenamedRindx) dst; //where to write this value
  Epoch epoch;
} ROBEntry deriving (Bits, Eq);

//holds issued instructions until their operands are ready
//updates held instructions by snooping CDB
module ReservationStation(CommonDataBus cdb);
  Vector#(2, Reg#(Maybe#(ReservationEntry))) entries;
  FIFO#(ReservationEntry) toExecFifo;

  //return true if no space in the entries vector
  method ActionValue#(Bool) full();
  //stores an instruction into an empty slot in the entries vector
  method Action put(Instr instr);

  //This rule puts instructions that are ready into a queue to be executed
  rule readyInstr;
    Maybe#(ReservationEntry) readyEntry = tagged Void;
    for (Integer i = 0; i < 2; i = i + 1) begin
      let entry = entries[fromInt(i)];
      if (isReady(entry)) readyEntry = entry;
    end
    if (isValid(readEntry)) toExecFifo.put(toReadyOperation(readyEntry));
  endrule

  rule bypass(isValid(cdb.get()));
    //update all entries that have the futures available now
  endrule

  //need to purge reservation entries whose epoch doesn't match the current one
endmodule

//recieves instructions in-order
//stores instructions in reorder buffer
//issues instructions to reservation station dispatched and preloads their operands
//  from register file and ROB
module IssueUnit(ArchRegFile archRegFile, ReOrderBuffer rob, DispatchUnit dispatch);
  FIFO#(Instr) instQ;

  //reorder buffer empty simplifies the store op, but it's not necessary for loads
  rule issueStore(instQ starts with a memop && reorder buffer empty);
    //issue the store from the architectural register
    //later, we'll issue the store into the ROB and the ROB will issue
    //the memory request upon graduation

    //OR

   //issue the load into the load buffer (addr) and the reorder buffer (destreg)
   //it's the same as in the writeback rule, but it writes the result to the CDB
   //the key is that the loadbuffer associates read requests with destination registers
  endrule

  rule issue(!instQ.empty() && instQ doesn't start with a memop);
    Instr inst = instQ.first();
    instQ.deq();
    //pack into ReservationEntry with immediate srcs as FutureOrImm.imm
    //  and register/memory srcs as FutureOrImm.future
    //for each valid src register
    //  look if argRegFile or rob contains the correct version
    //  update the reservationEntry accordingly
    rob.put(robEntry);
    dispatch.put(reservationEntry);
  endrule
endmodule

module DispatchUnit;
  FIFO#(ReservationEntry) entryQ;

  rule dispatch;
    //find an empty reservation station that accepts this type of entry and put it there
  endrule
endmodule

module ReOrderBuffer;
  CircularQueue(8, Reg#(Maybe#(ROBEntry))) entries;

  //insert an entry
  method Action put(ROBEntry entry);

  //lookup renamed reg
  method ActionValue#(Maybe#(Data)) lookupRenamedReg(RenamedRindx reg);

  rule purge(entries.first().epoch != currentEpoch());
    entries.deq();
  endrule

  rule graduate(readyToGraduate(entries.first()));
    //writeback the entry to the reg file
    //delete the entry from the ROB
  endrule

  rule recieve_cdb(isValid(cdb.get()));
    //fill in the waiting entry with its result
  endrule
endmodule
