import StmtFSM::*;
import Vector::*;
import TomasuloTypes::*;
import ProcTypes::*;
import ConfigReg::*;

interface ROB#(numeric type robsize);
  method ActionValue#(Bit#(TLog#(robsize))) reserve(Epoch epoch, Addr pc, WBReg dest);
  method Action updatePrediction(Bit#(TLog#(robsize)) tag, Addr mispredict);
  method Action updateData(Bit#(TLog#(robsize)) tag, Data data);
  method Maybe#(ROBEntry) get(Bit#(TLog#(robsize)) tag);
  method ROBEntry getLast();
  method Maybe#(Addr) getLastMispredict();
  method Bit#(TLog#(robsize)) getLastTag();
  method Action complete();
  method Bool isEmpty();
  method Bool isFull();
endinterface

//16 entry ROB
module mkReorderBuffer(ROB#(robsize));
  ROBEntry defaultEntry = ROBEntry { pc: 0, data:tagged Invalid, dest:tagged ArchReg 0, epoch:0 };

  Vector#(robsize, Reg#(Maybe#(ROBEntry))) entries <- replicateM(mkConfigReg(tagged Invalid));
  Vector#(robsize, Reg#(Maybe#(Addr))) mispredictEntries <- replicateM(mkConfigReg(tagged Invalid));
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
  method ActionValue#(Bit#(TLog#(robsize))) reserve(Epoch epoch, Addr pc, WBReg dest) if (!isFullFn());
    let entry = ROBEntry {
      data: tagged Invalid,
      pc: pc,
      dest: dest,
      epoch: epoch
    };
$display("reserved a rob entry for pc %h",pc);
    let tag = addPtr;
    entries[tag] <= tagged Valid entry;
    mispredictEntries[tag] <= tagged Invalid;
    addPtr <= tag + 1;
    return tag;
  endmethod

  method Action updateData(Bit#(TLog#(robsize)) tag, Data data);
    if (!isValid(entries[tag])) begin
      $display("fuck this shit data");
      for (Integer i = 0; i < valueof(robsize); i = i+1) begin
        if (entries[i] matches tagged Valid .ent) begin
            $display("tag:%d, data:%d, mispred:%d, dest:%d, pc:%d, epoch:%d", fromInteger(i), fromMaybe(22,ent.data), fromMaybe(222222,mispredictEntries[i]), ent.dest, ent.pc, ent.epoch);
        end else begin
            $display("%d invalid", valueof(robsize));
        end
      end
      $finish; 
    end
    let entry = fromMaybe(?, entries[tag]);
    entry.data = tagged Valid data;
    entries[tag] <= tagged Valid entry;
    $display("updated rob entry tag %d with data %d",tag, data);
  endmethod

  method Action updatePrediction(Bit#(TLog#(robsize)) tag, Addr mispredict);
    if (!isValid(entries[tag])) begin
      $display("fuck this shit mispredict");
      for (Integer i = 0; i < valueof(robsize); i = i+1) begin
        if (entries[i] matches tagged Valid .ent) begin
            $display("tag:%d, data:%d, mispred:%d, dest:%d, pc:%d, epoch:%d", fromInteger(i), fromMaybe(22,ent.data), fromMaybe(222222, mispredictEntries[i]), ent.dest, ent.pc, ent.epoch);
        end else begin
            $display("%d invalid", valueof(robsize));
        end
      end
      $finish; 
    end
    mispredictEntries[tag] <= tagged Valid mispredict;
    $display("updated rob entry tag %d with misprediction %h",tag, mispredict);
  endmethod

  method Maybe#(ROBEntry) get(Bit#(TLog#(robsize)) tag);
    return entries[tag];
  endmethod

  method ROBEntry getLast() if (isValid(entries[removePtr]));
    return fromMaybe(defaultEntry, entries[removePtr]);
  endmethod

  method Maybe#(Addr) getLastMispredict() if (isValid(entries[removePtr]));
    return mispredictEntries[removePtr];
  endmethod

  method Bit#(TLog#(robsize)) getLastTag() if (isValid(entries[removePtr]));
    return removePtr;
  endmethod

  method Action complete() if (isValid(entries[removePtr]));
$display("ROB complete");
    let tag = removePtr;
    entries[tag] <= tagged Invalid;
    removePtr <= tag + 1;
    if (addPtr == removePtr+1) $display("ROB is empty now");
  endmethod
endmodule

module mkROBTest(Empty);
  ROB#(16) rob <- mkReorderBuffer();

  function makeROBE(Epoch epoch);
    return ROBEntry { pc: 0, data:tagged Invalid, dest:tagged ArchReg 0, epoch:epoch };
  endfunction

  Reg#(Bit#(4)) tag1 <- mkReg(10);
  Reg#(Bit#(4)) tag2 <- mkReg(10);

  Stmt testInOrder =
  seq
    $display("start testInOrder");
    action let ent <- rob.reserve(1,0,tagged ArchReg 0); tag1 <= ent; endaction
    action let ent <- rob.reserve(2,0,tagged ArchReg 0); tag2 <= ent; endaction
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
        let tag <- rob.reserve(0, 0, tagged ArchReg 0);
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
