import StmtFSM::*;
import Vector::*;
import TomasuloTypes::*;
import ProcTypes::*;

interface ROB#(numeric type robsize);
  method ActionValue#(Bit#(TLog#(robsize))) reserve(Epoch epoch, Addr pc, Rindx dest);
  method Action updatePrediction(Bit#(TLog#(robsize)) tag, Addr mispredict);
  method Action updateData(Bit#(TLog#(robsize)) tag, Data data);
  method Maybe#(ROBEntry) get(Bit#(TLog#(robsize)) tag);
  method ROBEntry getLast();
  method Bit#(TLog#(robsize)) getLastTag();
  method Action complete();
  method Bool isEmpty();
  method Bool isFull();
endinterface

//16 entry ROB
module mkReorderBuffer(ROB#(robsize));
  ROBEntry defaultEntry = ROBEntry { pc: 0, data:tagged Invalid, mispredict:tagged Invalid, dest:0, epoch:0 };

  Vector#(robsize, Reg#(Maybe#(ROBEntry))) entries <- replicateM(mkReg(tagged Invalid));
  Reg#(Bit#(TLog#(robsize))) addPtr <- mkReg(0);
  Reg#(Bit#(TLog#(robsize))) removePtr <- mkReg(0);

  function Bool isFullFn();
    return isValid(entries[addPtr]);
  endfunction

  method Bool isEmpty();
    return addPtr == removePtr && !isValid(entries[addPtr]);
  endmethod

  method Bool isFull();
    return isFullFn();
  endmethod

  //initialize the reorder buffer's tail and return its key
  method ActionValue#(Bit#(TLog#(robsize))) reserve(Epoch epoch, Addr pc, Rindx dest) if (!isFullFn());
    let entry = ROBEntry {
      data: tagged Invalid,
      pc: pc,
      mispredict: tagged Invalid,
      dest: dest,
      epoch: epoch
    };
    let tag = addPtr;
    entries[tag] <= tagged Valid entry;
    addPtr <= tag + 1;
    return tag;
  endmethod

  method Action updateData(Bit#(TLog#(robsize)) tag, Data data);
    if (!isValid(entries[tag])) $display("fuck this shit");
    let entry = fromMaybe(?, entries[tag]);
    entry.data = tagged Valid data;
    entries[tag] <= tagged Valid entry;
  endmethod

  method Action updatePrediction(Bit#(TLog#(robsize)) tag, Addr mispredict);
    if (!isValid(entries[tag])) $display("fuck this shit");
    let entry = fromMaybe(?, entries[tag]);
    entry.mispredict = tagged Valid mispredict;
    entries[tag] <= tagged Valid entry;
  endmethod

  method Maybe#(ROBEntry) get(Bit#(TLog#(robsize)) tag);
    return entries[tag];
  endmethod

  method ROBEntry getLast() if (isValid(entries[removePtr]));
    return fromMaybe(defaultEntry, entries[removePtr]);
  endmethod

  method Bit#(TLog#(robsize)) getLastTag() if (isValid(entries[removePtr]));
    return removePtr;
  endmethod

  method Action complete() if (isValid(entries[removePtr]));
    let tag = removePtr;
    entries[tag] <= tagged Invalid;
    removePtr <= tag + 1;
  endmethod
endmodule

module mkROBTest(Empty);
  ROB#(16) rob <- mkReorderBuffer();

  function makeROBE(Epoch epoch);
    return ROBEntry { pc: 0, data:tagged Invalid, mispredict:tagged Invalid, dest:0, epoch:epoch };
  endfunction

  Reg#(Bit#(4)) tag1 <- mkReg(10);
  Reg#(Bit#(4)) tag2 <- mkReg(10);

  Stmt testInOrder =
  seq
    $display("start testInOrder");
    action let ent <- rob.reserve(1,0,0); tag1 <= ent; endaction
    action let ent <- rob.reserve(2,0,0); tag2 <= ent; endaction
    if (rob.isEmpty()) $display("You failed");
    if (rob.getLast().epoch != 1) $display("You failed");
    rob.complete();
    if (rob.isEmpty()) $display("You failed");
    if (rob.getLast().epoch != 2) $display("You failed");
    rob.complete();
    if (!rob.isEmpty()) $display("You failed");
    $display("end testInOrder (no output == pass)");
  endseq;

  Reg#(Bit#(5)) index <- mkRegU();
  Vector#(16,Reg#(Bit#(4))) tagVector <- replicateM(mkRegU());

  Stmt testFilling =
  seq
    $display("start testFilling");
    index <= 0;
    while (index < 16) seq
      action
        let tag <- rob.reserve(0, 0, 0);
        tagVector[index] <= tag;
	index <= index + 1;
      endaction
    endseq
    for (index <= 0; index < 16; index <= index + 1) seq
      action
        case (rob.get(tagVector[index])) matches
          tagged Invalid: $display("failed");
          tagged Valid .x: noAction;
        endcase
      endaction
    endseq
    if (!rob.isFull()) $display("failed");
    for (index <= 0; index < 16; index <= index + 1) seq
      rob.complete();
    endseq
    if (!rob.isEmpty()) $display("failed");
    $display("end testFilling (no output == pass)");
  endseq;

  Stmt allTests = seq
    testInOrder;
    testFilling;
  endseq;

  mkAutoFSM(allTests);
endmodule
