#ifndef _SENDVAREDIT_PATCH_H_
#define _SENDVAREDIT_PATCH_H_

#include <vector>
#include <string>

// doesn't compile without these two in this order when including sp::MacroAssembler and I'm clueless as to why
#include "extension.h"
// #include <memory>
#include <platform.h>

#include <asm/asm.h>
#include <macro-assembler-x86.h>

class PatchMask
{
public:
    enum MaskEntry {
        EIGNORE = -0x10000,
        ENOP = -0x20000,
    };

private:
    std::vector<int> mask;

public:
    PatchMask() {}

    // Space delimited string. 'n' = replace with NOP, 'i' to ignore a byte, string integer to shift a byte over to make space.
    PatchMask(const std::string &mask_string);

    // Index into the mask
    inline int operator[](size_t index) const { return *&mask[index]; }

    // Number of bytes the mask ranges over
    inline size_t MaskSize() const { return mask.size(); }

    // If parsing didn't fail
    inline bool Valid() const { return !mask.empty(); }

    // Get how many bytes will be writable with this mask
    size_t WritableSize() const;
};

class Patcher
{
private:
    void* base = nullptr;
    PatchMask mask;

    bool writable = false;
    bool patched = false;
    char* original = nullptr;

public:
    Patcher() {}
    Patcher(void* base, PatchMask mask);
    ~Patcher();

    inline bool Valid() const { return original != nullptr; }

    bool Patch(sp::MacroAssembler &masm);
    void UnPatch();

private:
    bool ApplyMask();
};

#endif // _SENDVAREDIT_PATCH_H_