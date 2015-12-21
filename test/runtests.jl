using WideInts
using Base.Test

typealias U8 UInt8
typealias U16 UInt16

typealias W8 WideUInt{U8}
typealias W16 WideUInt{U16}
typealias WW8 WideUInt{W8}

w0 = W16(0, 0)
w1 = W16(1, 0)
w2 = W16(2, 0)
w3 = W16(3, 0)

w16 = W16(0, 1)
w21 = W16(0x0201, 0)
w4321 = W16(0x0201, 0x0403)
w43 = W16(0, 0x0403)

@test (w1.lo, w1.hi) == (1, 0)
@test (w16.lo, w16.hi) == (0, 1)

@test w1 % UInt16 == 1
@test w16 % UInt16 == 0

@test w4321 % UInt8 === UInt8(0x01)
@test w4321 % UInt16 === UInt16(0x0201)
@test w4321 % UInt32 === UInt32(0x04030201)
@test w4321 % UInt64 === UInt64(0x04030201)
@test WW8(w4321) % UInt8 === UInt8(0x01)
@test WW8(w4321) % UInt16 === UInt16(0x0201)
@test WW8(w4321) % UInt32 === UInt32(0x04030201)
@test WW8(w4321) % UInt64 === UInt64(0x04030201)

@test UInt8(w1) === UInt8(0x01)
@test UInt16(w21) === UInt16(0x0201)
@test UInt32(w4321) === UInt32(0x04030201)
@test UInt64(w4321) === UInt64(0x04030201)
@test UInt8(WideUInt{UInt32}(12)) === UInt8(12)
@test UInt8(WideUInt{UInt64}(12)) === UInt8(12)

@test W8(w21) === W8(0x01, 0x02)
@test W16(w4321) === W16(0x0201, 0x0403)
@test WW8(w4321) === WW8(W8(0x01, 0x02), W8(0x03, 0x04))
@test WideUInt{UInt32}(w4321) === WideUInt{UInt32}(0x04030201, 0)

@test UInt8(W8(UInt8(0x12))) === UInt8(0x12)
@test UInt16(W8(UInt16(0x1234))) === UInt16(0x1234)
@test UInt32(W8(UInt32(0x1234))) === UInt32(0x1234)
@test UInt64(W8(UInt64(0x1234))) === UInt64(0x1234)

@test WW8(UInt32(0x12345678)) === WW8(W8(0x78, 0x56), W8(0x34, 0x12))
@test WW8(UInt32(0x12345678)) === WW8(W8(0x5678), W8(0x1234))

@test UInt8(WideUInt{UInt32}(UInt8(0x12))) === UInt8(0x12)
@test UInt16(WideUInt{UInt32}(UInt16(0x1234))) === UInt16(0x1234)
@test UInt32(WideUInt{UInt32}(UInt32(0x12345678))) === UInt32(0x12345678)
@test UInt64(WideUInt{UInt32}(UInt64(0x12345678))) === UInt64(0x12345678)
@test UInt8(WW8(UInt8(0x12))) === UInt8(0x12)
@test UInt16(WW8(UInt16(0x1234))) === UInt16(0x1234)
@test UInt32(WW8(UInt32(0x12345678))) === UInt32(0x12345678)
@test UInt64(WW8(UInt64(0x12345678))) === UInt64(0x12345678)

@test leading_zeros(w4321) == leading_zeros(UInt32(0x04030201))
@test leading_zeros(w21) == leading_zeros(UInt32(0x0201))
@test leading_ones(w4321) == leading_ones(UInt32(0x04030201))
@test leading_ones(~w21) == leading_ones(~UInt32(0x0201))
@test trailing_zeros(w4321) == trailing_zeros(UInt32(0x04030201))
@test trailing_zeros(w43) == trailing_zeros(UInt32(0x04030000))
@test trailing_ones(w4321) == trailing_ones(UInt32(0x04030201))
@test trailing_ones(~w43) == trailing_ones(~UInt32(0x04030000))

@test ~w4321 === W16(~UInt32(0x04030201))
@test w4321 & w43 === w43
@test w43 | w21 === w4321
@test w4321 $ w21 === w43

@test W16(0x04030201) >> 8 === W16(0x04030201 >> 8)
@test W16(0x04030201) >> 16 === W16(0x04030201 >> 16)
@test W16(0x04030201) >> 24 === W16(0x04030201 >> 24)
@test W16(0x04030201) << 8 === W16(0x04030201 << 8)
@test W16(0x04030201) << 16 === W16(0x04030201 << 16)
@test W16(0x04030201) << 24 === W16(0x04030201 << 24)

@test w1 == w1
@test w2 != w3
@test w1 < w2
@test w1 <= w2
@test w2 <= w2
@test w3 > w2
@test w3 >= w2
@test w2 >= w2
@test w43 < w4321

@test +w4321 === w4321
@test -w4321 === W16(-0x0201, -0x0404)
@test abs(w4321) === w4321

@test WideInts.wideadd(UInt8(0x00), UInt8(0x00)) === W8(0x00, 0x00)
@test WideInts.wideadd(UInt8(0x02), UInt8(0x03)) === W8(0x05, 0x00)
@test WideInts.wideadd(UInt8(0x7f), UInt8(0x80)) === W8(0xff, 0x00)
@test WideInts.wideadd(UInt8(0x7f), UInt8(0x81)) === W8(0x00, 0x01)
@test WideInts.wideadd(UInt8(0xff), UInt8(0xff)) === W8(0xfe, 0x01)

@test W8(0x00, 0x00) + W8(0x00, 0x00) === W8(0x00, 0x00)
@test W8(0x01, 0x02) + W8(0x03, 0x04) === W8(0x04, 0x06)
@test W8(0x01, 0x02) + W8(0xff, 0x04) === W8(0x00, 0x07)
@test W8(0x01, 0x02) + W8(0x03, 0xff) === W8(0x04, 0x01)

@test W8(0x00, 0x00) - W8(0x00, 0x00) === W8(0x00, 0x00)
@test W8(0x03, 0x04) - W8(0x01, 0x02) === W8(0x02, 0x02)
@test W8(0x03, 0x04) - W8(0xff, 0x02) === W8(0x04, 0x01)
@test W8(0x03, 0x04) - W8(0x01, 0xff) === W8(0x02, 0x05)

# const u8range = 0x00:0xff
# const u16range = 0x0000:0xffff
# const u16range_med =
#     Set(0x0000:0x03ff) ∪ Set(0x0000:0x11:0xffff) ∪ Set(0xfc00:0xffff)
# const u16range_short =
#     Set(0x0000:0x00ff) ∪ Set(0x0000:0x101:0xffff) ∪ Set(0xff00:0xffff)

#=TODO
info("loop 1")
for x in 0x00:0xff
    for y in 0x00:0xff
        @test WideInts.dmul(U8(x), U8(y)) === W8(widemul(x, y))
    end
end

info("loop 2")
for x in 0x0000:0x11:0xffff
    for y in 0x00:0xff
        @test WideInts.dmul1(U16(x), U8(y)) === W16(widemul(x, y))
    end
end

info("loop 3")
for x in 0x0000:0x101:0xffff
    for y in 0x0000:0x11:0xffff
        @test WideInts.dmul(W8(x), W8(y)) === WW8(widemul(x, y))
    end
end

info("loop 4")
for x in 0x0000:0xffff
    ymax = x==0 ? 0xffff : U16(fld(0xffff, x))
    for y in 0x0000:ymax
        @test W8(x) * W8(y) === W8(x*y)
    end
end

info("loop 5")
for x in 0x0000:0x11:0xffff
    for y in 0x01:0xff
        @test WideInts.ddiv(U16(x), U8(y)) === (U16(div(x, y)), U8(rem(x, y)))
    end
end

info("loop 6")
for x in 0x00000000:0x1001:0x00ffffff
    for y in 0x01:0xff
        @test WideInts.ddiv1(W16(x), U8(y)) === (W16(div(x, y)), U8(rem(x, y)))
    end
end
=#

info("loop 7")
for x in 0x00000000:0xffffffff
    if x % 0x100 == 0 info("x=$(hex(x))") end
    for y in 0x01:0xff
        @test WideInts.ddiv1(WW8(x), U8(y)) === (WW8(div(x, y)), U8(rem(x, y)))
    end
end
