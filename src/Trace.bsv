
import ClientServer::*;
import GetPut::*;

//----------------------------------------------------------------------
// ToString typeclass
//----------------------------------------------------------------------

typeclass Traceable#( type item_t );
  function Action traceTiny( String loc, String traceTag, item_t item );
  function Action traceFull( String loc, String traceTag, item_t item );
endtypeclass

instance Traceable#(String);

  function Action traceTiny( String loc, String ttag, String str );
    $fdisplay(stderr,  " => %s:%s %s", loc, ttag, str );
  endfunction

  function Action traceFull( String loc, String ttag, String str );
    $fdisplay(stderr,  " => %s:%s %s", loc, ttag, str );
  endfunction

endinstance

instance Traceable#(Bit#(n));

  function Action traceTiny( String loc, String ttag, Bit#(n) b );
    $fdisplay(stderr,  " => %s:%s %x", loc, ttag, b );
  endfunction

  function Action traceFull( String loc, String ttag, Bit#(n) b );
    $fdisplay(stderr,  " => %s:%s %x", loc, ttag, b );
  endfunction

endinstance

//----------------------------------------------------------------------
// Tracing interface wrappers
//----------------------------------------------------------------------

function Get#(item_t) traceGet( String locStr, String tagStr, Get#(item_t) g )
                      provisos ( Traceable#(item_t) );
  return 
  (
    interface Get
      method ActionValue#(item_t) get();
        item_t item <- g.get();
        traceTiny(locStr, tagStr,item);
        return item;
      endmethod
    endinterface
  );
endfunction

function Put#(item_t) tracePut( String locStr, String tagStr, Put#(item_t) p )
                      provisos ( Traceable#(item_t) );
  return 
  (
    interface Put
      method Action put( item_t item );
        traceTiny(locStr, tagStr,item);
        p.put(item);
      endmethod
    endinterface
  );
endfunction

function Client#(req_t,resp_t) traceClient( String locStr, String reqTagStr, String respTagStr,
                                            Client#(req_t,resp_t) c )
                                  provisos ( Traceable#(req_t), Traceable#(resp_t) );
  return
  (
     interface Client
       interface Get request  = traceGet(locStr, reqTagStr,c.request);
       interface Put response = tracePut(locStr, respTagStr,c.response);
     endinterface
  );
endfunction

function Server#(req_t,resp_t) traceServer( String locStr, String reqTagStr, String respTagStr,
                                            Server#(req_t,resp_t) c )
                                  provisos ( Traceable#(req_t), Traceable#(resp_t) );
  return
  (
     interface Server
       interface Put request  = tracePut(locStr, reqTagStr,c.request);
       interface Get response = traceGet(locStr, respTagStr,c.response);
     endinterface
  );
endfunction

