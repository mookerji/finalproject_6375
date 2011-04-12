
#include <cassert>
#include <string>

#include "TestBench.h"


void mem_cb(void* tb, const MainMemReq& req)
{
    ((TestBench*)tb)->handle_memreq(req);
}

TestBench::TestBench(const char* prg)
    : m_params("scemi.params"),
      m_scemi(SceMi::Init(SceMi::Version(SCEMI_VERSION_STRING), &m_params)),
      m_memreq("", "scemi_mmem_req_outport", m_scemi),
      m_memresp("", "scemi_mmem_resp_inport", m_scemi),
      m_tohostreq("", "scemi_tohost_request_inport", m_scemi),
      m_tohostresp("", "scemi_tohost_response_outport", m_scemi),
      m_statreq("", "scemi_stats_xx_req_inport", m_scemi),
      m_statresp("", "scemi_stats_xx_resp_outport", m_scemi),
      m_reset("", "scemi", m_scemi),
      m_shutdown("", "scemi_shutdown", m_scemi)
{
    m_memory = new FUNCP_SIMULATED_MEMORY_CLASS(prg);
    m_memreq.setCallBack(mem_cb, this);
    m_sthread = new SceMiServiceThread(m_scemi);
}

TestBench::~TestBench()
{
    m_shutdown.blocking_send_finish();
    m_sthread->stop();
    m_sthread->join();
    SceMi::Shutdown(m_scemi);
    delete m_sthread;
    delete m_memory;
}

void TestBench::run()
{
    unsigned int tohost = 0;

    // Reset the core
    m_reset.reset();

    // Now run and wait for it to finish.
    tohost = cpuToHost(0);
    for (int i = 0; i < 100 && tohost == 0; i++) {
        std::cout << "." << std::flush;
        sleep(1);
        tohost = cpuToHost(0);
    }

    if (tohost == 0) {
        printf("\n***TIMEOUT***\n");
    } else if (tohost == 1) {
        printf("\n***PASSED***\n");
    } else {
        printf("\n***FAILED*** (tohost = %i)\n", tohost);
    }

    // Get and dump the stats.
    int inst = getStat(StatID::e_PROC_INST);
    int cycles = getStat(StatID::e_PROC_CYCLES);
    std::cout << "proc.inst: " << inst << std::endl;
    std::cout << "proc.cycles: " << cycles << std::endl;
    std::cout << "icache.misses: " << getStat(StatID::e_ICACHE_MISSES) << std::endl;
    std::cout << "icache.evictions: " << getStat(StatID::e_ICACHE_EVICTIONS) << std::endl;
    std::cout << "icache_accesses: " << getStat(StatID::e_ICACHE_ACCESSES) << std::endl;
    std::cout << "dcache_misses: " << getStat(StatID::e_DCACHE_MISSES) << std::endl;
    std::cout << "dcache_accesses: " << getStat(StatID::e_DCACHE_ACCESSES) << std::endl;
    std::cout << "dcache_writebacks: " << getStat(StatID::e_DCACHE_WRITEBACKS) << std::endl;
    std::cout << "IPC: " << (double)inst/cycles << std::endl;
}

void TestBench::handle_memreq(const MainMemReq& thereq)
{
    MainMemReq req = thereq;
    MainMemResp response;
    UINT32 data;
    BitT<32>* addr;
    BitT<8>* tag;
    BitT<32>* sdata;
    switch (req.getTaggedUnionTag()) {
        case 0:     // LoadReq
            addr = (BitT<32>*)req.getMember(0)->getMember(0);
            tag = (BitT<8>*)req.getMember(0)->getMember(1);

            m_memory->Read(addr->get(), sizeof(UINT32), &data);

            response.the_tag = 0;
            response.m_LoadResp.m_data = BitT<32>(data);
            response.m_LoadResp.m_tag = *tag;

            // Send load response
            m_memresp.sendMessage(response);
            break;

        case 1:     // StoreReq
            addr = (BitT<32>*)req.getMember(1)->getMember(0);
            tag = (BitT<8>*)req.getMember(1)->getMember(1);
            sdata = (BitT<32>*)req.getMember(1)->getMember(2);


            data = sdata->get();
            m_memory->Write(addr->get(), sizeof(UINT32), &data);

            // Do NOT give a store response!
            // The Core does not expect a store response, and bad things will
            // happen (deadlock, for instance) if we give it a store response.
            break;

        default:
            assert(false && "unexpected mem request tag");
            break;
    }

}

unsigned int TestBench::cpuToHost(unsigned int req)
{
    // Send the request blocking.
    m_tohostreq.sendMessage(BitT<32>(req));

    // Get the response Blocking.
    return m_tohostresp.getMessage().get();
}

unsigned int TestBench::getStat(StatID::E_StatID which)
{
    m_statreq.sendMessage(StatID(which));
    return m_statresp.getMessage().get();
}

int main(int argc, char* argv[])
{
    if (argc < 2) {
        std::cerr << "You must supply a program to run." << std::endl;
        std::cerr << "example: tb foo.vmh" << std::endl;
        return 1;
    }
    const char* prg = argv[1];

    // Verify we can open the file for read.
    FILE* f = fopen(prg, "r");
    if (f == NULL) {
        std::cerr << "can't open " << prg << std::endl;
        return 1;
    } else {
        fclose(f);
    }

    TestBench tb(prg);
    tb.run();
}

