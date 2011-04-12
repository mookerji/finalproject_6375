/**
 * @file vmh-memory.cpp
 * @brief Implementation of funcp_simulated_memory using VMH images.
 * @author Michael Adler
 */

#include <strings.h>
#include <string.h>
#include <cassert>

#include "vmh-memory.h"
#include "vmh-utils.h"

enum { MEM_SIZE = 1048576 };    // 1MB

FUNCP_SIMULATED_MEMORY_CLASS::FUNCP_SIMULATED_MEMORY_CLASS(const char* prg)
{
    memory = new UINT8[MEM_SIZE];
    assert(memory != NULL && "Out of memory");

    bzero(memory, MEM_SIZE);

    // Load image
    bool s = vmh_load_image(prg, memory, MEM_SIZE);
    assert(s && "Failed to load VMH image");
}

FUNCP_SIMULATED_MEMORY_CLASS::~FUNCP_SIMULATED_MEMORY_CLASS()
{
    delete [] memory;
}


void
FUNCP_SIMULATED_MEMORY_CLASS::Read(
    UINT64 addr,
    UINT64 size,
    void *dest)
{
    if (addr + size > MEM_SIZE)
    {
        // Might be bad path or speculative
        bzero(dest, size);
        return;
    }

    switch (size)
    {
      case 8:
        *(UINT64*)dest = *(UINT64*)(&memory[addr]);
        break;
      case 4:
        *(UINT32*)dest = *(UINT32*)(&memory[addr]);
        break;
      case 2:
        *(UINT16*)dest = *(UINT16*)(&memory[addr]);
        break;
      case 1:
        *(UINT8*)dest = *(UINT8*)(&memory[addr]);
        break;
      default:
        memcpy(dest, &memory[addr], size);
        break;
    }
}


void
FUNCP_SIMULATED_MEMORY_CLASS::Write(
    UINT64 addr,
    UINT64 size,
    void *src)
{
    assert(addr + size <= MEM_SIZE);
    switch (size)
    {
      case 8:
        *(UINT64*)(&memory[addr]) = *(UINT64*)src;
        break;
      case 4:
        *(UINT32*)(&memory[addr]) = *(UINT32*)src;
        break;
      case 2:
        *(UINT16*)(&memory[addr]) = *(UINT16*)src;
        break;
      case 1:
        *(UINT8*)(&memory[addr]) = *(UINT8*)src;
        break;
      default:
        memcpy(&memory[addr], src, size);
        break;
    }
}

