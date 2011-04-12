
import Trace::*;

//----------------------------------------------------------------------
// Basic memory requests and responses
//----------------------------------------------------------------------

typedef union tagged
{
    struct { Bit#(addrSz) addr; Bit#(tagSz) tag;                    } LoadReq;
    struct { Bit#(addrSz) addr; Bit#(tagSz) tag; Bit#(dataSz) data; } StoreReq;  
}
MemReq#( type addrSz, type tagSz, type dataSz ) 
deriving(Eq,Bits);

typedef union tagged
{
    struct { Bit#(tagSz) tag; Bit#(dataSz) data; } LoadResp;
    struct { Bit#(tagSz) tag;                    } StoreResp;
}
MemResp#( type tagSz, type dataSz )
deriving(Eq,Bits);

//----------------------------------------------------------------------
// Specialized req/resp for inst/data/host
//----------------------------------------------------------------------

typedef 32 AddrSz;
typedef 08 TagSz;
typedef 32 DataSz;
typedef 32 InstSz;
typedef 32 HostDataSz;

typedef MemReq#(AddrSz,TagSz,0)          InstReq;
typedef MemResp#(TagSz,InstSz)           InstResp;

typedef MemReq#(AddrSz,TagSz,DataSz)     DataReq;
typedef MemResp#(TagSz,DataSz)           DataResp;

typedef MemReq#(AddrSz,TagSz,HostDataSz) HostReq;
typedef MemResp#(TagSz,HostDataSz)       HostResp;

//----------------------------------------------------------------------
// Specialized req/resp for main memory
//----------------------------------------------------------------------

typedef 32 MainMemAddrSz;
typedef 08 MainMemTagSz;
typedef 32 MainMemDataSz;

typedef MemReq#(MainMemAddrSz,MainMemTagSz,MainMemDataSz) MainMemReq;
typedef MemResp#(MainMemTagSz,MainMemDataSz)              MainMemResp;

//----------------------------------------------------------------------
// Tracing Functions
//----------------------------------------------------------------------

instance Traceable#(MemReq#(a,b,c));

    function Action traceTiny( String loc, String ttag, MemReq#(a,b,c) req );
        case ( req ) matches
            tagged LoadReq  .ld : $fdisplay(stderr,  " => %s:%s l%2x", loc, ttag, ld.tag );
            tagged StoreReq .st : $fdisplay(stderr,  " => %s:%s s%2x", loc, ttag, st.tag );
        endcase
    endfunction

    function Action traceFull( String loc, String ttag, MemReq#(a,b,c) req );
        case ( req ) matches
            tagged LoadReq  .ld : $fdisplay(stderr,  " => %s:%s Ld { addr=%x, tag=%x }",  loc, ttag, ld.addr, ld.tag );
            tagged StoreReq .st : $fdisplay(stderr,  " => %s:%s St { addr=%x, tag=%x, data=%x }", loc, ttag, st.addr, st.tag, st.data );
        endcase
    endfunction

endinstance

instance Traceable#(MemResp#(a,b));

    function Action traceTiny( String loc, String ttag, MemResp#(a,b) resp );
        case ( resp ) matches
            tagged LoadResp  .ld : $fdisplay(stderr,  " => %s:%s l%2x", loc, ttag, ld.tag );
            tagged StoreResp .st : $fdisplay(stderr,  " => %s:%s s%2x", loc, ttag, st.tag );
        endcase
    endfunction

    function Action traceFull( String loc, String ttag, MemResp#(a,b) resp );
        case ( resp ) matches
            tagged LoadResp  .ld : $fdisplay(stderr,  " => %s:%s Ld { tag=%x, data=%x }",  loc, ttag, ld.tag, ld.data );
            tagged StoreResp .st : $fdisplay(stderr,  " => %s:%s St { tag=%x  }", loc, ttag, st.tag );
        endcase
    endfunction

endinstance

