using WideInts
using Base.Test

w0 = WideUInt{UInt16}(0, 0)
w1 = WideUInt{UInt16}(1, 0)
w2 = WideUInt{UInt16}(2, 0)
w3 = WideUInt{UInt16}(3, 0)

w16 = WideUInt{UInt16}(0, 1)
w21 = WideUInt{UInt16}(0x0201, 0)
w4321 = WideUInt{UInt16}(0x0201, 0x0403)
w43 = WideUInt{UInt16}(0, 0x0403)

@test (w1.lo, w1.hi) == (1, 0)
@test (w16.lo, w16.hi) == (0, 1)

@test w1 % UInt16 == 1
@test w16 % UInt16 == 0

@test w4321 % UInt8 === UInt8(0x01)
@test w4321 % UInt16 === UInt16(0x0201)
@test w4321 % UInt32 === UInt32(0x04030201)
@test w4321 % UInt64 === UInt64(0x04030201)

@test UInt8(w1) === UInt8(1)
@test UInt16(w21) === UInt16(0x0201)
@test UInt32(w4321) === UInt32(0x04030201)
@test UInt64(w4321) === UInt64(0x04030201)

@test WideUInt{UInt8}(w21) === WideUInt{UInt8}(0x01, 0x02)
@test WideUInt{UInt16}(w4321) === WideUInt{UInt16}(0x0201, 0x0403)
@test WideUInt{UInt32}(w4321) === WideUInt{UInt32}(0x04030201, 0)

@test leading_zeros(w4321) == leading_zeros(UInt32(0x04030201))
@test leading_zeros(w21) == leading_zeros(UInt32(0x0201))
@test leading_ones(w4321) == leading_ones(UInt32(0x04030201))
@test leading_ones(~w21) == leading_ones(~UInt32(0x0201))
@test trailing_zeros(w4321) == trailing_zeros(UInt32(0x04030201))
@test trailing_zeros(w43) == trailing_zeros(UInt32(0x04030000))
@test trailing_ones(w4321) == trailing_ones(UInt32(0x04030201))
@test trailing_ones(~w43) == trailing_ones(~UInt32(0x04030000))

@test ~w4321 === WideUInt{UInt16}(~UInt32(0x04030201))
@test w4321 & w43 === w43
@test w43 | w21 === w4321
@test w4321 $ w21 === w43

@test +w4321 === w4321
@test -w4321 === WideUInt{UInt16}(-0x0201, -0x0403)
@test abs(w4321) === w4321

typealias W8 WideUInt{UInt8}
@test W8(0x01, 0x02) + W8(0x03, 0x04) === W8(0x04, 0x06)
@test W8(0x01, 0x02) + W8(0xff, 0x04) === W8(0x00, 0x07)
@test W8(0x01, 0x02) + W8(0x03, 0xff) === W8(0x04, 0x01)
@test W8(0x03, 0x04) - W8(0x01, 0x02) === W8(0x02, 0x02)
@test W8(0x03, 0x04) - W8(0xff, 0x02) === W8(0x04, 0x01)
@test W8(0x03, 0x04) - W8(0x01, 0xff) === W8(0x02, 0x05)
