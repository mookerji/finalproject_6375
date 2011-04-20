import Vector::*;
import TomasuloTypes::*;
import CommonDataBus::*;

interface ReservationStation;
  method ActionValue#(RSEntry) getReadyEntry();
  method Action put(RSEntry entry);
endinterface

module mkReservationStation(CommonDataBus#(CDBPacket) cdb, ReservationStation rsifc);

  Vector#(2, Reg#(Maybe#(RSEntry))) entries <- replicateM(mkReg(tagged Invalid));

  rule cdb_recv;
    let packet <- cdb.get2();
    let tag = packet.tag;
    if (isValid(packet.data)) begin
      let oper = tagged Imm fromMaybe(?, packet.data);
      for (Integer i = 0; i < 2; i = i + 1) begin
        if (isValid(entries[i])) begin
          let entry = fromMaybe(?, entries[i]);
          Bool modified = False;
          if (entry.op1 matches tagged Tag .it &&& it == tag) begin
             entry.op1 = oper;
            modified = True;
          end
          if (entry.op2 matches tagged Tag .it &&& it == tag) begin
            entry.op2 = oper;
            modified = True;
          end
          if (modified) entries[i] <= tagged Valid entry;
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

  method ActionValue#(RSEntry) getReadyEntry() if (isValid(readyEntry()));
    let i = fromMaybe(?, readyEntry());
    entries[i] <= tagged Invalid;
    return fromMaybe(?, entries[i]);
  endmethod

  method Action put(RSEntry entry) if (isValid(freeSlot()));
    let i = fromMaybe(?, readyEntry());
    entries[i] <= tagged Valid entry;
  endmethod
endmodule
