import StmtFSM::*;
import Vector::*;
import TomasuloTypes::*;

interface ROB#(numeric type robsize);
  method ActionValue#(Bit#(TLog#(robsize))) reserve(ROBEntry robEntry);
  method Action update(Bit#(TLog#(robsize)) tag, ROBEntry robEntry);
  method Maybe#(ROBEntry) get(Bit#(TLog#(robsize)) tag);
  method ROBEntry getLast();
  method Action complete();
  method Bool isEmpty();
  method Bool isFull();
endinterface

//16 entry ROB
module mkReorderBuffer(ROB#(robsize));
  ROBEntry defaultEntry = ROBEntry { data:tagged Invalid, mispredict:tagged Invalid, dest:0, epoch:0 };

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
  method ActionValue#(Bit#(TLog#(robsize))) reserve(ROBEntry robEntry) if (!isFullFn());
    let tag = addPtr;
    entries[tag] <= tagged Valid robEntry;
    addPtr <= tag + 1;
    return tag;
  endmethod

  method Action update(Bit#(TLog#(robsize)) tag, ROBEntry robEntry);
    if (!isValid(entries[tag])) $display("fuck this shit");
    entries[tag] <= tagged Valid robEntry;
  endmethod

  method Maybe#(ROBEntry) get(Bit#(TLog#(robsize)) tag);
    return entries[tag];
  endmethod

  method ROBEntry getLast() if (isValid(entries[removePtr]));
    return fromMaybe(defaultEntry, entries[removePtr]);
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
    return ROBEntry { data:tagged Invalid, mispredict:tagged Invalid, dest:0, epoch:epoch };
  endfunction

  Reg#(Bit#(4)) tag1 <- mkReg(10);
  Reg#(Bit#(4)) tag2 <- mkReg(10);

  Stmt testInOrder =
  seq
    $display("start testInOrder");
    action let ent <- rob.reserve(makeROBE(1)); tag1 <= ent; endaction
    action let ent <- rob.reserve(makeROBE(2)); tag2 <= ent; endaction
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
        let tag <- rob.reserve(makeROBE({0,index}));
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
