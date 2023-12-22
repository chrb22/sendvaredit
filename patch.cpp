#include <patch.h>

#include <utility>
#include <vector>
#include <string>
#include <cstring>

#include <asm/asm.h>
#include <macro-assembler-x86.h>

#include <jit/jit_helpers.h>
#include <CDetour/detourhelpers.h>

std::vector<std::pair<size_t, size_t>> SplitString(const std::string &string, char character)
{
    std::vector<std::pair<size_t, size_t>> slices;

    size_t pos = string.find_first_not_of(character);
    size_t next;
    while ((next = string.find_first_of(character, pos)) != std::string::npos) {
        slices.push_back(std::make_pair(pos, next - pos));

        pos = string.find_first_not_of(character, next);
    }

    if (pos != std::string::npos)
        slices.push_back(std::make_pair(pos, string.length() - pos));

    return slices;
}

PatchMask::PatchMask(const std::string &mask_string)
{
    auto slices = SplitString(mask_string, ' ');

    for (size_t index = 0; index < slices.size(); index++) {
        size_t pos = slices[index].first;
        size_t len = slices[index].second;

        if (len == 1) {
            switch (mask_string[pos]) {
                case 'i':
                    mask.push_back(PatchMask::EIGNORE);
                    continue;
                case 'n':
                    mask.push_back(PatchMask::ENOP);
                    continue;
            }
        }

        size_t len_parse = 0;
        int shift = std::stoi(mask_string.substr(pos, len), &len_parse);
        if (len_parse == len) {
            mask.push_back(shift);
            continue;
        }

        // parsing failed
        mask.clear();
        break;
    }
}

size_t PatchMask::WritableSize() const
{
    int lowest_shift = mask.size();

    size_t index;
    for (index = 0; index < mask.size(); index++) {
        int entry = mask[index];

        if (entry == PatchMask::EIGNORE || lowest_shift == 0)
            break;

        if (entry != PatchMask::ENOP)
            if (entry < lowest_shift)
                lowest_shift = entry;

        lowest_shift--;
    }

    return index;
}

Patcher::Patcher(void* base, PatchMask mask)
    : base(base), mask(mask)
{
    if (!base || !mask.Valid())
        return;

    original = new char[mask.MaskSize()];
    std::memcpy(original, base, mask.MaskSize());
}

Patcher::~Patcher()
{
    if (Valid()) {
        UnPatch();
        delete[] original;
    }
}

bool Patcher::ApplyMask()
{
    if (!Valid() || !writable || patched)
        return false;

    for (int offset = mask.MaskSize() - 1; offset >= 0; offset--) {
        int entry = mask[offset];

        switch (entry) {
            case PatchMask::EIGNORE:
                continue;
            case PatchMask::ENOP:
                *((unsigned char*)base + offset) = 0x90;
                continue;
            default:
                *((unsigned char*)base + offset + entry) = *((char*)base + offset);
                *((unsigned char*)base + offset) = 0x90;
                continue;
        }
    }

    return true;
}

bool Patcher::Patch(sp::MacroAssembler &masm)
{
    if (!Valid() || patched)
        return false;

    if (masm.length() > mask.WritableSize())
        return false;

    if (!writable) {
        SetMemPatchable(base, mask.MaskSize());
        writable = true;
    }

    ApplyMask();
    masm.emitToExecutableMemory(base);

    patched = true;

    return true;
}

void Patcher::UnPatch()
{
    if (!patched)
        return;

    std::memcpy(base, original, mask.MaskSize());
    patched = false;
}