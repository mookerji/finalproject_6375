/**
 * @file vmh-memory.h
 * @brief Implementation of funcp_simulated_memory using VMH images.
 * @author Michael Adler
 */

#ifndef __VMH_MEMORY__
#define __VMH_MEMORY__

typedef unsigned char UINT8;
typedef unsigned short UINT16;
typedef unsigned int UINT32;
typedef unsigned long long UINT64;


typedef class FUNCP_SIMULATED_MEMORY_CLASS *FUNCP_SIMULATED_MEMORY;

// Response from VtoP
struct FUNCP_MEM_VTOP_RESP
{
    UINT64 pa;
    bool pageFault;    // Translation failed
    bool ioSpace;      // Reference is to uncacheable I/O space
};

class FUNCP_SIMULATED_MEMORY_CLASS
{
  public:
    //
    // Required public interface
    //

    FUNCP_SIMULATED_MEMORY_CLASS(const char* prg);
    ~FUNCP_SIMULATED_MEMORY_CLASS();

    void Read(UINT64 addr, UINT64 size, void *dest);
    void Write(UINT64 addr, UINT64 size, void *src);

  private:
    //
    // VMH-specific code...
    //
    UINT8 *memory;
};

#endif // __VMH_MEMORY__
