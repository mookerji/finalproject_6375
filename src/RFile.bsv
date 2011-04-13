import RegFile::*;
import ProcTypes::*;

//-----------------------------------------------------------
// Register file module
//-----------------------------------------------------------

interface RFile#(type t);
    method Action   wr( Rindx rindx, t data );
    method t rd1( Rindx rindx );
    method t rd2( Rindx rindx );
endinterface

module mkRFile(t defaultValue, RFile#(t) rfifc ) provisos (Bits#(t,__a));
    RegFile#(Rindx,t) rfile <- mkRegFileWCF(0, 31);
    RWire#(Rindx) indxWire <- mkRWire();
    RWire#(t) dataWire <- mkRWire();
   
    method Action wr( Rindx rindx, t data );
        rfile.upd( rindx, data );
        indxWire.wset(rindx);
        dataWire.wset(data);
    endmethod
   
    method t rd1( Rindx rindx );
        if (rindx == 0) return defaultValue;
        else if (indxWire.wget() matches tagged Valid .indx &&& indx == rindx)
            return fromMaybe(defaultValue, dataWire.wget());
        else return rfile.sub(rindx);
    endmethod
   
    method t rd2( Rindx rindx );
        if (rindx == 0) return defaultValue;
        else if (indxWire.wget() matches tagged Valid .indx &&& indx == rindx)
            return fromMaybe(defaultValue, dataWire.wget());
        else return rfile.sub(rindx);
    endmethod

endmodule
