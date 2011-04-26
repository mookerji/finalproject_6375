import Vector::*;
import TomasuloTypes::*;
import FShow::*;
import ConfigReg::*;
import ReorderBuffer::*;

interface ReservationStation;
  method ActionValue#(RSEntry) getReadyEntry();
  method Action put(RSEntry entry);
endinterface

module mkReservationStation(ROB#(16) rob, ReservationStation rsifc);
//this must be a configreg because we want dispatch_alu, check_completion, and decode_issue to fire simultaneously
//the check_completion rule must fire every cycle or else we could lose writeback results in the ROB
//this should be rewritten to use RWires and a regular reg, where those functions write to wires and the rule writes to the reg
//but the configreg is correct too
  Vector#(2, Reg#(Maybe#(RSEntry))) entries <- replicateM(mkConfigReg(tagged Invalid));

/*
  rule test;
    for (Integer i = 0; i < 2; i = i + 1) begin
      case (entries[i]) matches
        tagged Valid .e: $display("Reservation Station contains entry ",fshow(e.op)," [",fshow(e.op1),", ",fshow(e.op2),"]");
        tagged Invalid: $display("Reservation Station contains no entry");
      endcase
    end
  endrule
*/

  rule allfull(isValid(entries[0]) && isValid(entries[1]));
    $display("Reservation Station is full");
  endrule

  rule check_for_completion_of_dependencies;
    for (Integer i = 0; i < 2; i = i + 1) begin
      if (isValid(entries[i])) begin
        let entry = fromMaybe(?, entries[i]);
        Bool modified = False;
$display("Looking for ",fshow(entry.op1));
if (entry.op1 matches tagged Tag .it) begin
  let robDataStr = fshow("[No ROB Entry]");
  if (rob.get(it) matches tagged Valid .robent)
    robDataStr = fshow(robent.data);
  $display("data for the above tag in the ROB is ",robDataStr);
end
        if (entry.op1 matches tagged Tag .it &&& rob.get(it) matches tagged Valid .robent &&&
            robent.data matches tagged Valid .robdata) begin
           entry.op1 = tagged Imm robdata;
           modified = True;
        end
$display("Looking for ",fshow(entry.op2));
if (entry.op2 matches tagged Tag .it) begin
  let robDataStr = fshow("[No ROB Entry]");
  if (rob.get(it) matches tagged Valid .robent)
    robDataStr = fshow(robent.data);
  $display("data for the above tag in the ROB is ",robDataStr);
end
        if (entry.op2 matches tagged Tag .it &&& rob.get(it) matches tagged Valid .robent &&&
            robent.data matches tagged Valid .robdata) begin
           entry.op2 = tagged Imm robdata;
           modified = True;
        end
        if (modified) begin
          $display("Reservation Station found ROB update which updated an RSEntry");
          entries[i] <= tagged Valid entry;
        end
      end
    end
  endrule

  function Maybe#(UInt#(TLog#(2))) readyEntry();
    Maybe#(UInt#(TLog#(2))) index = tagged Invalid;
    for (Integer i = 0; i < 2; i = i + 1) begin
      if (isValid(entries[i])) begin
        let entry = fromMaybe(?, entries[i]);
        if (entry.op1 matches tagged Imm .a &&& entry.op2 matches tagged Imm .b) begin
          index = tagged Valid fromInteger(i);
        end
      end
    end
    return index;
  endfunction

  function Maybe#(UInt#(TLog#(2))) freeSlot();
    Maybe#(UInt#(TLog#(2))) index = tagged Invalid;
    for (Integer i = 0; i < 2; i = i + 1) begin
      if (!isValid(entries[i])) begin
        index = tagged Valid fromInteger(i);
      end
    end
    return index;
  endfunction

  method ActionValue#(RSEntry) getReadyEntry() if (readyEntry() matches tagged Valid .i);
    let e = fromMaybe(?, entries[i]);
    entries[i] <= tagged Invalid;
    $display($format("Got ready entry [%d] ",i),fshow(e.op)," [",fshow(e.op1),", ",fshow(e.op2),"]");
    return e;
  endmethod

  method Action put(RSEntry entry) if (freeSlot() matches tagged Valid .i);
    $display("Inserting new entry into slot %d",i);
    entries[i] <= tagged Valid entry;
  endmethod
endmodule
