
#ifndef TESTBENCH_H
#define TESTBENCH_H

#include "SceMiHeaders.h"

#include "vmh-memory.h"
#include "ResetXactor.h"

class TestBench
{
public:
    TestBench(const char* prg);
    ~TestBench();

    void run();
    void handle_memreq(const MainMemReq& req);

private:

    unsigned int cpuToHost(unsigned int req);
    unsigned int getStat(StatID::E_StatID which);

    // Sce-Mi stuff
    SceMiParameters m_params;
    SceMi* m_scemi;
    SceMiServiceThread* m_sthread;

    // Memory Transactors
    OutportProxyT<MainMemReq> m_memreq;
    InportQueueT<MainMemResp> m_memresp;

    // CPUTohost Transactors
    InportProxyT<BitT<32> > m_tohostreq;
    OutportQueueT<BitT<32> > m_tohostresp;

    // Stats Transactors
    InportProxyT<StatID> m_statreq;
    OutportQueueT<Stat> m_statresp;

    // Reset
    ResetXactor m_reset;
       
    // Shutdown Transactor
    ShutdownXactor m_shutdown;

    // Simulated Memory
    FUNCP_SIMULATED_MEMORY m_memory;
};

#endif//TESTBENCH_H

