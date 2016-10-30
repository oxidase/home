// -*- compile-command: "g++ -Wall -std=c++11 -ggdb -Ofast -msse4.1 -msse4.2 -mavx -g indexed_data.cpp -o indexed_data" -*-
#include <iostream>
#include <sstream>
#include <vector>
#include <array>
#include <string>
#include <iterator>
#include <assert.h>
#include <iomanip>
#include <typeinfo>

template<int N, typename T = std::string>
struct VariableGroupBlock
{
    static constexpr std::uint32_t BLOCK_SIZE = N;

    typedef T                      ResultType;
    typedef typename T::value_type ValueType;

    static_assert(0 <= BLOCK_SIZE && BLOCK_SIZE <= 16);
    static_assert(sizeof(ValueType) == 1, "data basic type must char");

    struct BlockReference
    {
        std::uint32_t offset;
        std::uint32_t descriptor;
    };

    /// Returns ceiling(log_256(value + 1))
    inline std::uint32_t log256(std::uint32_t value) const
    {
        assert(value < 0x1000000);
        return value == 0 ? 0 : value < 0x100 ? 1 : value < 0x10000 ? 2 : 3;
    }

    /// Advance data iterator by the value of byte_length bytes at length iterator.
    /// Advance length iterator by byte_length.
    template<typename DataIterator>
    inline void var_advance(DataIterator& data, DataIterator& length, std::uint32_t byte_length) const
    {
        typedef typename DataIterator::difference_type difference_type;
        data += byte_length >= 1 ? static_cast<difference_type>(*length++)       : 0;
        data += byte_length >= 2 ? static_cast<difference_type>(*length++) <<  8 : 0;
        data += byte_length == 3 ? static_cast<difference_type>(*length++) << 16 : 0;
    }

    /// Summation of 16 2-bit values using SWAR
    inline std::uint32_t sum2bits(std::uint32_t value) const
    {
        value = (value >> 2 & 0x33333333) + (value & 0x33333333);
        value = (value >> 4 & 0x0f0f0f0f) + (value & 0x0f0f0f0f);
        value = (value >> 8 & 0x00ff00ff) + (value & 0x00ff00ff);
        return (value >> 16 & 0x0000ffff) + (value & 0x0000ffff);
    }

    /// Write a block reference {offset, descriptor}, where offset
    /// is a global block offset and descriptor is a 32-bit value
    /// of prefix length. sum(descriptor) equals to the block
    /// prefix length.
    /// Returns the block prefix length.
    template<typename Offset, typename OffsetIterator>
    Offset WriteBlockReference(std::ostream& out, Offset data_offset, OffsetIterator first, OffsetIterator last) const
    {
        Offset prefix_length = 0;
        BlockReference refernce{data_offset, 0};
        for ( ; first != last; --last)
        {
            const std::uint32_t data_length = *last - *std::prev(last);
            const std::uint32_t byte_length = log256(data_length);
            refernce.descriptor = (refernce.descriptor << 2) | byte_length;
            prefix_length += byte_length;
        }

        out.write((const char *)&refernce, sizeof(refernce));

        return prefix_length;
    }

    /// Write a block prefix that is an array of variable encoded data lengths:
    ///   0 is omitted;
    ///   1..255 is 1 byte;
    ///   256..65535 is 2 bytes;
    ///   65536..16777215 is 3 bytes.
    /// [first..last] is an inclusive range of block data.
    /// The length of the last item in the block is not stored.
    template<typename OffsetIterator>
    void WriteBlockPrefix(std::ostream& out, OffsetIterator first, OffsetIterator last) const
    {
        for (OffsetIterator curr = first, next = std::next(first); curr != last; ++curr, ++next)
        {
            const std::uint32_t data_length = *next - *curr;
            const std::uint32_t byte_length = log256(data_length);
            if (byte_length == 0)
                continue;

            out.write((const char *)&data_length, byte_length);
        }
    }

    /// Return an item stored in the referenced block.
    /// [first..last) is a range of the complete block data with prefix.
    template<typename DataIterator>
    ResultType ReadRefrencedBlock(const BlockReference& reference, std::uint32_t local_index, DataIterator first, DataIterator last) const
    {
        std::uint32_t descriptor = reference.descriptor;
        DataIterator data = first + sum2bits(descriptor); // advance to block data
        for (std::uint32_t i = 0; i < local_index; ++i, descriptor >>= 2)
        {
            var_advance(data, first, descriptor & 0x3);
        }

        if (local_index < BLOCK_SIZE)
        {
            last = data;
            var_advance(last, first, descriptor & 0x3);
        }

        return ResultType(data, last);
    }
};


template<int N, typename T = std::string>
struct FixedGroupBlock
{
    static constexpr std::uint32_t BLOCK_SIZE = N;

    typedef T                      ResultType;
    typedef typename T::value_type ValueType;

    static_assert(sizeof(ValueType) == 1, "data basic type must char");

    struct BlockReference
    {
        std::uint32_t offset;
    };

    /// Write a block reference {offset}, where offset is a global block offset
    /// Returns the fixed block prefix length.
    template<typename Offset, typename OffsetIterator>
    Offset WriteBlockReference(std::ostream& out, Offset data_offset, OffsetIterator, OffsetIterator) const
    {
        BlockReference refernce{data_offset};
        out.write((const char *)&refernce, sizeof(refernce));

        return BLOCK_SIZE;
    }

    /// Write a fixed length block prefix.
    template<typename OffsetIterator>
    void WriteBlockPrefix(std::ostream& out, OffsetIterator first, OffsetIterator last) const
    {
        std::uint32_t index = 0;
        std::array<ValueType, BLOCK_SIZE> block_prefix;
        for (OffsetIterator curr = first, next = std::next(first); curr != last; ++curr, ++next)
        {
            const std::uint32_t data_length = *next - *curr;
            assert(data_length < 256);
            block_prefix[index++] = data_length;
        }
        out.write((const char *)block_prefix.data(), block_prefix.size());
    }

    /// Return an item stored in the referenced block.
    /// [first..last) is a range of the complete block data with prefix.
    template<typename DataIterator>
    ResultType ReadRefrencedBlock(const BlockReference& reference, std::uint32_t local_index, DataIterator first, DataIterator last) const
    {
        DataIterator data = first + BLOCK_SIZE; /// advance to block data
        for (std::uint32_t i = 0; i < local_index; ++i)
        {
            data += *first++;
        }

        if (local_index < BLOCK_SIZE)
            last = data + *first;

        return ResultType(data, last);
    }
};


template<typename GroupBlock>
struct IndexedData
{
    static constexpr std::uint32_t BLOCK_SIZE = GroupBlock::BLOCK_SIZE;

    typedef typename GroupBlock::BlockReference BlockReference;
    typedef typename GroupBlock::ResultType     ResultType;
    typedef typename GroupBlock::ValueType      ValueType;

    std::vector<BlockReference> block_references;
    std::vector<ValueType> data;

    template<typename OffsetIterator, typename DataIterator>
    void write(std::ostream& out, OffsetIterator first, OffsetIterator last, DataIterator data) const
    {
        static_assert(sizeof(typename DataIterator::value_type) == 1, "data basic type must char");

        assert(first < last);
        const OffsetIterator sentinel = std::prev(last);

        /// write number of blocks
        const std::uint32_t number_of_blocks = 1 + (std::distance(first, sentinel) - 1) / (BLOCK_SIZE + 1);
        out.write((const char *)&number_of_blocks, sizeof(number_of_blocks));

        /// write block references and compute the total data size that includes prefix and data
        const GroupBlock block;
        std::uint32_t data_size = 0;
        for (OffsetIterator curr = first, next = first; next != sentinel; curr = next)
        {
            std::advance(next, std::min<typename OffsetIterator::difference_type>(BLOCK_SIZE, std::distance(next, sentinel)));
            data_size += block.WriteBlockReference(out, data_size, curr, next);
            std::advance(next, std::min<typename OffsetIterator::difference_type>(1, std::distance(next, sentinel)));
            data_size += *next - *curr;
        }

        /// write the total data size
        out.write((const char *)&data_size, sizeof(data_size));

        /// write data blocks that are (prefix, data)
        for (OffsetIterator curr = first, next = first; next != sentinel; curr = next)
        {
            std::advance(next, std::min<typename OffsetIterator::difference_type>(BLOCK_SIZE, std::distance(next, sentinel)));
            block.WriteBlockPrefix(out, curr, next);
            std::advance(next, std::min<typename OffsetIterator::difference_type>(1, std::distance(next, sentinel)));
            std::copy(data + *curr, data + *next, std::ostream_iterator<unsigned char>(out));
        }
    }

    void read(std::istream& in)
    {
        std::uint32_t number_of_blocks, data_size;

        in.read((char *)&number_of_blocks, sizeof(number_of_blocks));

        block_references.resize(number_of_blocks);
        in.read((char *)block_references.data(), number_of_blocks * sizeof(BlockReference));

        in.read((char *)&data_size, sizeof(data_size));

        data.resize(data_size);
        in.read((char *)data.data(), data_size * sizeof(ValueType));
    }

    ResultType at(std::uint32_t index) const
    {
        const std::uint32_t block_idx = index / (BLOCK_SIZE + 1);
        const std::uint32_t internal_idx = index % (BLOCK_SIZE + 1);

        if (block_idx >= block_references.size())
            return ResultType();

        const auto first = data.begin() + block_references[block_idx].offset;
        const auto last = block_idx + 1 == block_references.size()
            ? data.end()
            : data.begin() + block_references[block_idx + 1].offset;

        const GroupBlock block;
        return block.ReadRefrencedBlock(block_references[block_idx], internal_idx, first, last);
    }
};


template<typename GroupBlock, typename Offsets, typename Data>
void test(const Offsets& offsets, const Data& data)
{
    std::stringstream sstr;
    GroupBlock blocked_data;
    blocked_data.write(sstr, offsets.begin(), offsets.end(), data.begin());

    std::cout << "\n" << typeid(GroupBlock).name() << "\nsaved size = " << std::dec << sstr.str().size() << "\n";
    for (auto c : sstr.str())
        std::cout << std::hex << std::setw(2) << std::setfill('0') << (int)((unsigned char)c) << " ";
    std::cout << "\n";

    blocked_data.read(sstr);

    std::cout << "'" << blocked_data.at(0) << "'";
    for (std::size_t i = 1; i < offsets.size() - 1; ++i)
        std::cout << ", '" << blocked_data.at(i) << "'";
    std::cout << std::endl;
}

int main()
{
    assert(VariableGroupBlock<16>().sum2bits(0xe4) == 6);
    assert(VariableGroupBlock<16>().sum2bits(0x11111111) == 8);
    assert(VariableGroupBlock<16>().sum2bits(0x55555555) == 16);
    assert(VariableGroupBlock<16>().sum2bits(0xffffffff) == 48);

    assert(VariableGroupBlock<16>().log256(0) == 0);
    assert(VariableGroupBlock<16>().log256(1) == 1);
    assert(VariableGroupBlock<16>().log256(255) == 1);
    assert(VariableGroupBlock<16>().log256(256) == 2);
    assert(VariableGroupBlock<16>().log256(1024) == 2);
    assert(VariableGroupBlock<16>().log256(16777215) == 3);

    std::vector<std::string> str = {"", "A", "bb", "ccc", "dDDd", "E", "ff", "ggg", "hhhh", "I", "jj", "",
                                    "kkk", "llll", "M", "nn", "ooo", "pppp", "q", "r", "S", "T", "",
                                    "u", "V", "W", "X", "Y", "Z","","","","","","","", "0", ""};

    std::vector<unsigned char> name_char_data;
    std::vector<std::uint32_t> name_offsets;
    for (auto s : str)
    {
        name_offsets.push_back(name_char_data.size());
        std::copy(s.begin(), s.end(), std::back_inserter(name_char_data));
    }
    name_offsets.push_back(name_char_data.size());

    test<IndexedData<VariableGroupBlock<16, std::string>>>(name_offsets, name_char_data);
    test<IndexedData<FixedGroupBlock<16, std::string>>>(name_offsets, name_char_data);

    test<IndexedData<VariableGroupBlock<0, std::string>>>(name_offsets, name_char_data);
    test<IndexedData<FixedGroupBlock<0, std::string>>>(name_offsets, name_char_data);

    test<IndexedData<VariableGroupBlock<1, std::string>>>(name_offsets, name_char_data);
    test<IndexedData<FixedGroupBlock<1, std::string>>>(name_offsets, name_char_data);

    test<IndexedData<FixedGroupBlock<32, std::string>>>(name_offsets, name_char_data);
    test<IndexedData<FixedGroupBlock<128, std::string>>>(name_offsets, name_char_data);

    // 🐼 check
    std::string panda = "🐼";
    for (int i = 0; i < 1000; ++i) std::copy(panda.begin(), panda.end(), std::back_inserter(name_char_data));
    name_offsets.push_back(name_char_data.size());
    test<IndexedData<VariableGroupBlock<16, std::string>>>(name_offsets, name_char_data);

    return 0;
}
