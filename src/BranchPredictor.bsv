import Vector::*;
import ProcTypes::*;

//-----------------------------------------------------------
// Branch Predictor
//-----------------------------------------------------------
interface BranchPredictor#(numeric type btbsize);
	method ActionValue#(Addr) predict();
	method Addr confirmPredict(Addr currentPc);
	method Epoch currentEpoch();
	method Action mispredict(Addr srcPc, Addr dstPc);
endinterface

module mkBranchPredictor(BranchPredictor#(btbsize) bpifc );
    Reg#(Addr)  predPc <- mkReg(32'h00001000);
    Reg#(Epoch) epoch <- mkReg(0);
    //src dst pairs
    Vector#(btbsize, Reg#(Tuple2#(Addr, Addr))) branchTargetBuffer <- replicateM(mkReg(tuple2(0,0)));

    function Addr doPredict(Addr currentPc);
	Addr prediction = currentPc + 4;
	for (Integer i = 0; i < valueof(btbsize); i = i + 1) begin
		match {.srcAddr, .dstAddr} = branchTargetBuffer[i];
		if (srcAddr == currentPc) begin
			prediction = dstAddr;
		end
        end
	return prediction;
    endfunction

    method Addr confirmPredict(Addr currentPc) = doPredict(currentPc);

    method ActionValue#(Addr) predict();
	predPc <= doPredict(predPc);
	return predPc;
    endmethod

    method Epoch currentEpoch();
	return epoch;
    endmethod

    method Action mispredict(Addr srcPc, Addr dstPc);
	epoch <= epoch + 1;
	predPc <= dstPc;
	//Shift away everything
	UInt#(TLog#(btbsize)) shiftBase = fromInteger(valueof(btbsize)-1);
	//if we have it, find its location
	for (Integer i = 0; i < valueof(btbsize); i = i + 1) begin
		match {.srcAddr, .dstAddr} = branchTargetBuffer[i];
		if (srcAddr == srcPc) shiftBase = fromInteger(i);
        end
	for (Integer i = 1; i < valueof(btbsize); i = i + 1) begin
		if (fromInteger(i) <= shiftBase) branchTargetBuffer[i] <= branchTargetBuffer[i-1];
        end
	branchTargetBuffer[0] <= tuple2(srcPc, dstPc);
    endmethod
endmodule

