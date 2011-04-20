import StmtFSM::*;
import TomasuloTypes::*;
import Vector::*;

interface CommonDataBus#(type t);
  method Action put(t entry);
  method ActionValue#(t) get0();
  method ActionValue#(t) get1();
  method ActionValue#(t) get2();
  method ActionValue#(t) get3();
  method Bool hasData();
  method Action dumpState();
endinterface

module mkCDB(CommonDataBus#(t)) provisos (Bits#(t,c__));
  Vector#(4, Reg#(Bool)) acks <- replicateM(mkReg(False));
  Reg#(Maybe#(t)) data <- mkReg(tagged Invalid);
  RWire#(t) dataWire <- mkRWire();

  function Bool andFn(Bool x, Bool y);
    return x && y;
  endfunction

//  rule allAcked(foldr1(andFn, readVReg(acks)) == True);
  rule allAcked(acks[2] && acks[3]);
$display("all acked");
    writeVReg(acks, replicate(False));
    data <= tagged Invalid;
  endrule

  //we persist data when it hasn't been acked
  rule persist(dataWire.wget() matches tagged Valid .it &&& foldr1(andFn, readVReg(acks)) == False);
    data <= tagged Valid it;
  endrule

  method Action dumpState();
    $display("dumping state");
    for (Integer i = 0; i < 4; i = i + 1)
      $display("acks[%d] = %b",i,acks[i]);
  endmethod

  method Bool hasData();
    return isValid(data);
  endmethod

  method ActionValue#(t) get0() if ((isValid(data) || isValid(dataWire.wget())) && !acks[0]);
$display("get0 acked");
    acks[0] <= True;
    if (dataWire.wget() matches tagged Valid .it) return it;
    else if (data matches tagged Valid .it) return it;
    else begin
      $display("this shouldn't happen");
      return ?;
    end
  endmethod

  method ActionValue#(t) get1() if ((isValid(data) || isValid(dataWire.wget())) && !acks[1]);
$display("get1 acked");
    acks[1] <= True;
    if (dataWire.wget() matches tagged Valid .it) return it;
    else if (data matches tagged Valid .it) return it;
    else begin
      $display("this shouldn't happen");
      return ?;
    end
  endmethod

  method ActionValue#(t) get2() if ((isValid(data) || isValid(dataWire.wget())) && !acks[2]);
$display("get2 acked");
    acks[2] <= True;
    if (dataWire.wget() matches tagged Valid .it) return it;
    else if (data matches tagged Valid .it) return it;
    else begin
      $display("this shouldn't happen");
      return ?;
    end
  endmethod

  method ActionValue#(t) get3() if ((isValid(data) || isValid(dataWire.wget())) && !acks[3]);
$display("get3 acked");
    acks[3] <= True;
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
