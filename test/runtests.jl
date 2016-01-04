using WideInts
using Base.Test

using SmallInts

typealias U4 UInt4
typealias U8 UInt8
typealias U16 UInt16
typealias I4 Int4
typealias I8 Int8
typealias I16 Int16

typealias WU4 WideUInt{U4}
typealias WU8 WideUInt{U8}
typealias WU16 WideUInt{U16}
typealias WI4 WideInt{I4,U4}
typealias WI8 WideInt{I8,U8}
typealias WI16 WideInt{I16,U16}

typealias WWU4 WideUInt{WU4}
typealias WWU8 WideUInt{WU8}

typealias WWI4 WideInt{WI4,WU4}
typealias WWI8 WideInt{WI8,WU8}

wu0 = WU16(0, 0)
wu1 = WU16(1, 0)
wu2 = WU16(2, 0)
wu3 = WU16(3, 0)

wi0 = WI16(0, 0)
wi1 = WI16(1, 0)
wi2 = WI16(2, 0)
wi3 = WI16(3, 0)

wu16 = WU16(0, 1)
wu21 = WU16(0x0201, 0)
wu4321 = WU16(0x0201, 0x0403)
wu43 = WU16(0, 0x0403)

wi16 = WI16(0, 1)
wi17 = WI16(0, -1)
wi21 = WI16(0x0201, 0)
wi4321 = WI16(0x0201, 0x0403)
wi43 = WI16(0, 0x0403)

@test (wu1.lo, wu1.hi) == (1, 0)
@test (wu16.lo, wu16.hi) == (0, 1)

@test (wi1.lo, wi1.hi) == (1, 0)
@test (wi16.lo, wi16.hi) == (0, 1)
@test (wi17.lo, wi17.hi) == (0, -1)

@test wu1 % UInt16 == 1
@test wu16 % UInt16 == 0

@test wi1 % Int16 == 1
@test wi16 % Int16 == 0
@test wi17 % Int16 == 0

@test wu4321 % UInt8 === UInt8(0x01)
@test wu4321 % UInt16 === UInt16(0x0201)
@test wu4321 % UInt32 === UInt32(0x04030201)
@test wu4321 % UInt64 === UInt64(0x04030201)
@test WWU8(wu4321) % UInt8 === UInt8(0x01)
@test WWU8(wu4321) % UInt16 === UInt16(0x0201)
@test WWU8(wu4321) % UInt32 === UInt32(0x04030201)
@test WWU8(wu4321) % UInt64 === UInt64(0x04030201)

@test wi4321 % Int8 === Int8(0x01)
@test wi4321 % Int16 === Int16(0x0201)
@test wi4321 % Int32 === Int32(0x04030201)
@test wi4321 % Int64 === Int64(0x04030201)
@test WWI8(wi4321) % Int8 === Int8(0x01)
@test WWI8(wi4321) % Int16 === Int16(0x0201)
@test WWI8(wi4321) % Int32 === Int32(0x04030201)
@test WWI8(wi4321) % Int64 === Int64(0x04030201)

@test UInt8(wu1) === UInt8(0x01)
@test UInt16(wu21) === UInt16(0x0201)
@test UInt32(wu4321) === UInt32(0x04030201)
@test UInt64(wu4321) === UInt64(0x04030201)
@test UInt8(WideUInt{UInt32}(12)) === UInt8(12)
@test UInt8(WideUInt{UInt64}(12)) === UInt8(12)

@test Int8(wi1) === Int8(0x01)
@test Int16(wi21) === Int16(0x0201)
@test Int32(wi4321) === Int32(0x04030201)
@test Int64(wi4321) === Int64(0x04030201)
@test Int8(WideInt{Int32,UInt32}(12)) === Int8(12)
@test Int8(WideInt{Int64,UInt64}(12)) === Int8(12)

@test WU8(wu21) === WU8(0x01, 0x02)
@test WU16(wu4321) === WU16(0x0201, 0x0403)
@test WWU8(wu4321) === WWU8(WU8(0x01, 0x02), WU8(0x03, 0x04))
@test WideUInt{UInt32}(wu4321) === WideUInt{UInt32}(0x04030201, 0)

@test WI8(wi21) === WI8(0x01, 0x02)
@test WI16(wi4321) === WI16(0x0201, 0x0403)
@test WWI8(wi4321) === WWI8(WU8(0x01, 0x02), WI8(0x03, 0x04))
@test WideInt{Int32,UInt32}(wi4321) === WideInt{Int32,UInt32}(0x04030201, 0)

@test UInt8(WU8(UInt8(0x12))) === UInt8(0x12)
@test UInt16(WU8(UInt16(0x1234))) === UInt16(0x1234)
@test UInt32(WU8(UInt32(0x1234))) === UInt32(0x1234)
@test UInt64(WU8(UInt64(0x1234))) === UInt64(0x1234)

@test Int8(WI8(Int8(0x12))) === Int8(0x12)
@test Int16(WI8(Int16(0x1234))) === Int16(0x1234)
@test Int32(WI8(Int32(0x1234))) === Int32(0x1234)
@test Int64(WI8(Int64(0x1234))) === Int64(0x1234)

@test WWU8(UInt32(0x12345678)) === WWU8(WU8(0x78, 0x56), WU8(0x34, 0x12))
@test WWU8(UInt32(0x12345678)) === WWU8(WU8(0x5678), WU8(0x1234))

@test WWI8(Int32(0x12345678)) === WWI8(WU8(0x78, 0x56), WI8(0x34, 0x12))
@test WWI8(Int32(0x12345678)) === WWI8(WU8(0x5678), WI8(0x1234))

@test UInt8(WideUInt{UInt32}(UInt8(0x12))) === UInt8(0x12)
@test UInt16(WideUInt{UInt32}(UInt16(0x1234))) === UInt16(0x1234)
@test UInt32(WideUInt{UInt32}(UInt32(0x12345678))) === UInt32(0x12345678)
@test UInt64(WideUInt{UInt32}(UInt64(0x12345678))) === UInt64(0x12345678)
@test UInt8(WWU8(UInt8(0x12))) === UInt8(0x12)
@test UInt16(WWU8(UInt16(0x1234))) === UInt16(0x1234)
@test UInt32(WWU8(UInt32(0x12345678))) === UInt32(0x12345678)
@test UInt64(WWU8(UInt64(0x12345678))) === UInt64(0x12345678)

@test Int8(WideInt{Int32,UInt32}(Int8(0x12))) === Int8(0x12)
@test Int16(WideInt{Int32,UInt32}(Int16(0x1234))) === Int16(0x1234)
@test Int32(WideInt{Int32,UInt32}(Int32(0x12345678))) === Int32(0x12345678)
@test Int64(WideInt{Int32,UInt32}(Int64(0x12345678))) === Int64(0x12345678)
@test Int8(WWI8(Int8(0x12))) === Int8(0x12)
@test Int16(WWI8(Int16(0x1234))) === Int16(0x1234)
@test Int32(WWI8(Int32(0x12345678))) === Int32(0x12345678)
@test Int64(WWI8(Int64(0x12345678))) === Int64(0x12345678)

@test count_ones(wu4321) == count_ones(UInt32(0x04030201))
@test count_ones(wu21) == count_ones(UInt32(0x0201))
@test count_zeros(wu4321) == count_zeros(UInt32(0x04030201))
@test count_zeros(wu21) == count_zeros(UInt32(0x0201))
@test leading_zeros(wu4321) == leading_zeros(UInt32(0x04030201))
@test leading_zeros(wu21) == leading_zeros(UInt32(0x0201))
@test leading_ones(wu4321) == leading_ones(UInt32(0x04030201))
@test leading_ones(~wu21) == leading_ones(~UInt32(0x0201))
@test trailing_zeros(wu4321) == trailing_zeros(UInt32(0x04030201))
@test trailing_zeros(wu43) == trailing_zeros(UInt32(0x04030000))
@test trailing_ones(wu4321) == trailing_ones(UInt32(0x04030201))
@test trailing_ones(~wu43) == trailing_ones(~UInt32(0x04030000))

@test count_ones(wi4321) == count_ones(Int32(0x04030201))
@test count_ones(wi21) == count_ones(Int32(0x0201))
@test count_zeros(wi4321) == count_zeros(Int32(0x04030201))
@test count_zeros(wi21) == count_zeros(Int32(0x0201))
@test leading_zeros(wi4321) == leading_zeros(Int32(0x04030201))
@test leading_zeros(wi21) == leading_zeros(Int32(0x0201))
@test leading_ones(wi4321) == leading_ones(Int32(0x04030201))
@test leading_ones(~wi21) == leading_ones(~Int32(0x0201))
@test trailing_zeros(wi4321) == trailing_zeros(Int32(0x04030201))
@test trailing_zeros(wi43) == trailing_zeros(Int32(0x04030000))
@test trailing_ones(wi4321) == trailing_ones(Int32(0x04030201))
@test trailing_ones(~wi43) == trailing_ones(~Int32(0x04030000))

@test ~wu4321 === WU16(~UInt32(0x04030201))
@test wu4321 & wu43 === wu43
@test wu43 | wu21 === wu4321
@test wu4321 $ wu21 === wu43

@test ~wi4321 === WI16(~Int32(0x04030201))
@test wi4321 & wi43 === wi43
@test wi43 | wi21 === wi4321
@test wi4321 $ wi21 === wi43

@test WU16(0x04030201) >> 8 === WU16(0x04030201 >> 8)
@test WU16(0x04030201) >> 16 === WU16(0x04030201 >> 16)
@test WU16(0x04030201) >> 24 === WU16(0x04030201 >> 24)
@test WU16(0x04030201) << 8 === WU16(0x04030201 << 8)
@test WU16(0x04030201) << 16 === WU16(0x04030201 << 16)
@test WU16(0x04030201) << 24 === WU16(0x04030201 << 24)

@test WI16(0x04030201) >> 8 === WI16(0x04030201 >> 8)
@test WI16(0x04030201) >> 16 === WI16(0x04030201 >> 16)
@test WI16(0x04030201) >> 24 === WI16(0x04030201 >> 24)
@test WI16(0x04030201) << 8 === WI16(0x04030201 << 8)
@test WI16(0x04030201) << 16 === WI16(0x04030201 << 16)
@test WI16(0x04030201) << 24 === WI16(0x04030201 << 24)

@test wu1 == wu1
@test wu2 != wu3
@test wu1 < wu2
@test wu1 <= wu2
@test wu2 <= wu2
@test wu3 > wu2
@test wu3 >= wu2
@test wu2 >= wu2
@test wu43 < wu4321

@test wi1 == wi1
@test wi2 != wi3
@test wi1 < wi2
@test wi1 <= wi2
@test wi2 <= wi2
@test wi3 > wi2
@test wi3 >= wi2
@test wi2 >= wi2
@test wi43 < wi4321

@test +wu4321 === wu4321
@test -wu4321 === WU16(-0x0201, -0x0404)
@test abs(wu4321) === wu4321
@test signbit(wu4321) === false

@test +wi4321 === wi4321
@test -wi4321 === WI16(-0x0201, -signed(0x0404))
@test abs(wi4321) === wi4321
@test signbit(wi4321) === false
@test +wi17 === wi17
@test -wi17 === WI16(0, 1)
@test abs(wi17) === -wi17
@test signbit(wi17) === true

@test WideInts.wideadd(UInt8(0x00), UInt8(0x00)) === WU8(0x00, 0x00)
@test WideInts.wideadd(UInt8(0x02), UInt8(0x03)) === WU8(0x05, 0x00)
@test WideInts.wideadd(UInt8(0x7f), UInt8(0x80)) === WU8(0xff, 0x00)
@test WideInts.wideadd(UInt8(0x7f), UInt8(0x81)) === WU8(0x00, 0x01)
@test WideInts.wideadd(UInt8(0xff), UInt8(0xff)) === WU8(0xfe, 0x01)

@test WU8(0x00, 0x00) + WU8(0x00, 0x00) === WU8(0x00, 0x00)
@test WU8(0x01, 0x02) + WU8(0x03, 0x04) === WU8(0x04, 0x06)
@test WU8(0x01, 0x02) + WU8(0xff, 0x04) === WU8(0x00, 0x07)
@test WU8(0x01, 0x02) + WU8(0x03, 0xff) === WU8(0x04, 0x01)

@test WI8(0x00, 0x00) + WI8(0x00, 0x00) === WI8(0x00, 0x00)
@test WI8(0x01, 0x02) + WI8(0x03, 0x04) === WI8(0x04, 0x06)
@test WI8(0x01, 0x02) + WI8(0xff, 0x04) === WI8(0x00, 0x07)
@test WI8(0x01, 0x02) + WI8(0x03,   -1) === WI8(0x04, 0x01)

@test WU8(0x00, 0x00) - WU8(0x00, 0x00) === WU8(0x00, 0x00)
@test WU8(0x03, 0x04) - WU8(0x01, 0x02) === WU8(0x02, 0x02)
@test WU8(0x03, 0x04) - WU8(0xff, 0x02) === WU8(0x04, 0x01)
@test WU8(0x03, 0x04) - WU8(0x01, 0xff) === WU8(0x02, 0x05)

@test WI8(0x00, 0x00) - WI8(0x00, 0x00) === WI8(0x00, 0x00)
@test WI8(0x03, 0x04) - WI8(0x01, 0x02) === WI8(0x02, 0x02)
@test WI8(0x03, 0x04) - WI8(0xff, 0x02) === WI8(0x04, 0x01)
@test WI8(0x03, 0x04) - WI8(0x01,   -1) === WI8(0x02, 0x05)

for x in 0x00:0x0f
    for y in 0x00:0x0f
        @test WideInts.dmul(U4(x), U4(y)) === WU4(widemul(x, y))
    end
end

for x in 0x00:0xff
    for y in 0x00:0x0f
        @test WideInts.dmul1(U8(x), U4(y)) === WU8(widemul(x, y))
    end
end

for x in 0x00:0xff
    for y in 0x00:0xff
        t1 = WideInts.dmul(WU4(x), WU4(y))
        t2 = WWU4(widemul(x, y))
        @test WideInts.dmul(WU4(x), WU4(y)) === WWU4(widemul(x, y))
    end
end

for x in 0x00:0xff
    ymax = x==0 ? 0xff : fld(0xff, x)
    for y in 0x00:ymax
        @test WU4(x) * WU4(y) === WU4(x*y)
    end
end

for x in 0x00:0xff
    for y in 0x01:0xff
        @test divrem(WU4(x), WU4(y)) === (WU4(div(x, y)), WU4(rem(x, y)))
    end
end
