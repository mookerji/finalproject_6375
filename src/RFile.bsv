import RegFile::*;
import Vector::*;
import ConfigReg::*;
import ProcTypes::*;

//-----------------------------------------------------------
// Register file module
//-----------------------------------------------------------

interface RFile#(type t);
    method Action   wr( Rindx rindx, t data );
    method t rd1( Rindx rindx );
    method t rd2( Rindx rindx );
endinterface

module mkRFile#(t defaultValue, Bool bypass)(RFile#(t) rfifc ) provisos (Bits#(t,__a));
    RegFile#(Rindx,t) rfile <- mkRegFileWCF(0, 31);
    //TODO: dirty hack, fuck the lack of documentation of how to preload a reg file
    Vector#(32,ConfigReg#(Bool)) writtenYet <- replicateM(mkConfigReg(False));
    RWire#(Rindx) indxWire <- mkRWire();
    RWire#(t) dataWire <- mkRWire();
   
    method Action wr( Rindx rindx, t data );
        rfile.upd( rindx, data );
        indxWire.wset(rindx);
        dataWire.wset(data);
        writtenYet[rindx] <= True;
    endmethod
   
    method t rd1( Rindx rindx );
        if (rindx == 0) return defaultValue;
        else if (indxWire.wget() matches tagged Valid .indx &&& indx == rindx &&& bypass)
            return fromMaybe(defaultValue, dataWire.wget());
        else if (writtenYet[rindx]) return rfile.sub(rindx);
        else return defaultValue;
    endmethod
   
    method t rd2( Rindx rindx );
        if (rindx == 0) return defaultValue;
        else if (indxWire.wget() matches tagged Valid .indx &&& indx == rindx &&& bypass)
            return fromMaybe(defaultValue, dataWire.wget());
        else if (writtenYet[rindx]) return rfile.sub(rindx);
        else return defaultValue;
    endmethod

endmodule
