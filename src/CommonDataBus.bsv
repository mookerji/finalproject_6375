import StmtFSM::*;
import TomasuloTypes::*;
import Vector::*;
import Assert::*;

interface CommonDataBus#(type t, numeric type nlisteners);
  method Action put(t entry);
  method ActionValue#(t) get(Bit#(TLog#(nlisteners)) id);
  method Bool hasData();
  method Action dumpState();
endinterface

module mkCDB(CommonDataBus#(t, nlisteners)) provisos (Add#(1,a__,nlisteners), Bits#(t,c__));
  Vector#(nlisteners, Reg#(Bool)) acks <- replicateM(mkReg(False));
  Reg#(Maybe#(t)) data <- mkReg(tagged Invalid);

  function Bool andFn(Bool x, Bool y);
    return x && y;
  endfunction

  rule allAcked(foldr1(andFn, readVReg(acks)));
    writeVReg(acks, replicate(False));
    data <= tagged Invalid;
  endrule

  method Action dumpState();
    $display("dumping state");
    for (Integer i = 0; i < valueof(nlisteners); i = i + 1)
      $display("acks[%d] = %b",i,acks[i]);
  endmethod

  method Bool hasData();
    return isValid(data);
  endmethod

  method ActionValue#(t) get(Bit#(TLog#(nlisteners)) id) if (isValid(data));
    dynamicAssert(True == acks[id], "Cannot ack for an ID twice per bus cycle");
    if (acks[id] == True) $display("Cannot ack for an ID twice per bus cycle");
    acks[id] <= True;
    return fromMaybe(?, data);
  endmethod

  method Action put(t entry) if (!isValid(data));
    data <= tagged Valid entry;
  endmethod
endmodule

module mkTestCDB(Empty);
  CommonDataBus#(Bit#(8), 3) cdb <- mkCDB();

  Stmt test =
  seq
    $display("start CDB test");
    cdb.put(22);
    await(cdb.hasData());
    par
      action let v <- cdb.get(0); dynamicAssert(v == 22, "expect to get 22"); endaction
      action let v <- cdb.get(1); dynamicAssert(v == 22, "expect to get 22"); endaction
      action let v <- cdb.get(2); dynamicAssert(v == 22, "expect to get 22"); endaction
    endpar
    await(!cdb.hasData());
    cdb.put(52);
    await(cdb.hasData());
    par
      action let v <- cdb.get(0); dynamicAssert(v == 52, "expect to get 52"); endaction
      action let v <- cdb.get(2); dynamicAssert(v == 52, "expect to get 52"); endaction
      seq repeat (6) noAction; action let v <- cdb.get(1); dynamicAssert(v == 52, "expect to get 52"); endaction endseq
      seq repeat (4) noAction; action let v <- cdb.get(2); endaction endseq
    endpar
    await(!cdb.hasData());
    cdb.put(32);
    await(cdb.hasData());
    seq
      action let v <- cdb.get(0); dynamicAssert(v == 32, "expect to get 32"); endaction
      action let v <- cdb.get(1); dynamicAssert(v == 32, "expect to get 32"); endaction
      action let v <- cdb.get(2); dynamicAssert(v == 32, "expect to get 32"); endaction
      action let v <- cdb.get(2); endaction
    endseq
    await(!cdb.hasData());
    $display("end CDB test (2 instances of 'cannot ack for an id twice' == pass)");
  endseq;

  mkAutoFSM(test);
endmodule
