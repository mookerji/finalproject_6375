import StmtFSM::*;
import TomasuloTypes::*;
import Vector::*;

interface CommonDataBus#(type t, numeric type nlisteners);
  method Action put(t entry);
  method ActionValue#(t) get(Bit#(TLog#(nlisteners)) id);
  method Bool hasData();
  method Action dumpState();
endinterface

module mkCDB(CommonDataBus#(t, nlisteners)) provisos (Add#(1,a__,nlisteners), Bits#(t,c__));
  Vector#(nlisteners, Reg#(Bool)) acks <- replicateM(mkReg(False));
  Reg#(Maybe#(t)) data <- mkReg(tagged Invalid);
  RWire#(t) dataWire <- mkRWire();

  function Bool andFn(Bool x, Bool y);
    return x && y;
  endfunction

  rule allAcked(foldr1(andFn, readVReg(acks)) == True);
    writeVReg(acks, replicate(False));
    data <= tagged Invalid;
  endrule

  //we persist data when it hasn't been acked
  rule persist(dataWire.wget() matches tagged Valid .it &&& foldr1(andFn, readVReg(acks)) == False);
    data <= tagged Valid it;
  endrule

  method Action dumpState();
    $display("dumping state");
    for (Integer i = 0; i < valueof(nlisteners); i = i + 1)
      $display("acks[%d] = %b",i,acks[i]);
  endmethod

  method Bool hasData();
    return isValid(data);
  endmethod

  method ActionValue#(t) get(Bit#(TLog#(nlisteners)) id) if (isValid(data) || isValid(dataWire.wget()));
    if (acks[id] == True) $display("Cannot ack for an ID twice per bus cycle");
    acks[id] <= True;
    if (dataWire.wget() matches tagged Valid .it) return it;
    else if (data matches tagged Valid .it) return it;
    else begin
      $display("this shouldn't happen");
      return ?;
    end
  endmethod

  method Action put(t entry) if (!isValid(data));
    dataWire.wset(entry);
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
      action let v <- cdb.get(0); if (v != 22) $display("expect to get 22, got ", v); endaction
      action let v <- cdb.get(1); if (v != 22) $display("expect to get 22, got ", v); endaction
      action let v <- cdb.get(2); if (v != 22) $display("expect to get 22, got ", v); endaction
    endpar
    await(!cdb.hasData());
    cdb.put(52);
    await(cdb.hasData());
    par
      action let v <- cdb.get(0); if (v != 52) $display("expect to get 52"); endaction
      action let v <- cdb.get(2); if (v != 52) $display("expect to get 52"); endaction
      seq repeat (6) noAction; action let v <- cdb.get(1); if (v != 52) $display("expect to get 52"); endaction endseq
      seq repeat (4) noAction; action let v <- cdb.get(2); endaction endseq
    endpar
    await(!cdb.hasData());
    cdb.put(32);
    await(cdb.hasData());
    seq
      action let v <- cdb.get(0); if (v != 32) $display("expect to get 32"); endaction
      action let v <- cdb.get(1); if (v != 32) $display("expect to get 32"); endaction
      action let v <- cdb.get(2); if (v != 32) $display("expect to get 32"); endaction
      action let v <- cdb.get(2); endaction
    endseq
    await(!cdb.hasData());
    $display("end CDB test (2 instances of 'cannot ack for an id twice' == pass)");
  endseq;

  mkAutoFSM(test);

endmodule
