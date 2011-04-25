import Vector::*;
import TomasuloTypes::*;
import FShow::*;
import ConfigReg::*;

interface ReservationStation;
  method ActionValue#(RSEntry) getReadyEntry();
  method Action put(RSEntry entry);
endinterface

module mkReservationStation(RWire#(CDBPacket) cdb, ReservationStation rsifc);

  Vector#(2, Reg#(Maybe#(RSEntry))) entries <- replicateM(mkConfigReg(tagged Invalid));

/*
  rule fucklife;
    for (Integer i = 0; i < 2; i = i + 1) begin
      case (entries[i]) matches
        tagged Valid .e: $display("Reservation Station contains entry ",fshow(e.op)," [",fshow(e.op1),", ",fshow(e.op2),"]");
        tagged Invalid: $display("Reservation Station contains no entry");
      endcase
    end
  endrule
*/

  rule allfull(isValid(entries[0]) && isValid(entries[1]));
    $display("oscar the grouch lives in a trashcan");
  endrule

  rule cdb_recv(cdb.wget() matches tagged Valid .packet);
    $display("Reservation station got packet ",fshow(packet));
    for (Integer i = 0; i < 2; i = i + 1) begin
      case (entries[i]) matches
        tagged Valid .e: $display("Reservation Station contains entry ",fshow(e.op)," [",fshow(e.op1),", ",fshow(e.op2),"]");
        tagged Invalid: $display("Reservation Station contains no entry");
      endcase
    end
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
          if (modified) begin
            $display("Reservation Station got packet which updated an entry");
            entries[i] <= tagged Valid entry;
          end
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
    $display("Got ready entry ",fshow(e.op)," [",fshow(e.op1),", ",fshow(e.op2),"]");
    return e;
  endmethod

  method Action put(RSEntry entry) if (isValid(freeSlot()));
    let i = fromMaybe(?, readyEntry());
    entries[i] <= tagged Valid entry;
  endmethod
endmodule
