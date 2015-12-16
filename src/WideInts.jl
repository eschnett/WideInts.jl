module WideInts

export WideUInt

# typealias OtherUnsigned Union{subtypes(Unsigned)...}
# typealias OtherInteger
#     Union{setdiff(subtypes(Integer), [Unsigned])..., OtherUnsigned}
typealias OtherUnsigned Union{UInt8, UInt16, UInt32, UInt64, UInt128}
typealias OtherInteger Union{Int8, Int16, Int32, Int64, Int128, OtherUnsigned}

immutable WideUInt{T<:Unsigned} <: Unsigned
    lo::T
    hi::T
end

# Creation and type conversions

import Base: rem, convert, promote_rule

nbits{T}(::Type{T}) = 8*sizeof(T)
halfnbits{T}(::Type{T}) = 4*sizeof(T)

rem{T<:OtherUnsigned}(x::WideUInt{T}, ::Type{T}) = x.lo
function rem{T<:OtherUnsigned, U<:OtherInteger}(x::WideUInt{T}, ::Type{U})
    typemax(U) <= typemax(T) && return x.lo % U
    x.lo % U | x.hi % U << nbits(T)
end

convert{T<:OtherUnsigned}(::Type{WideUInt{T}}, x::WideUInt{T}) = x
function convert{T<:Unsigned, U<:Unsigned}(::Type{WideUInt{T}}, x::WideUInt{U})
    if nbits(T) < nbits(U)
        lo = x.lo % T
        hi = (x.lo >> nbits(T) | x.hi << (nbits(U) - nbits(T))) % T
        if 2*nbits(T) >= nbits(U)
            x.hi >> (2*nbits(T) - nbits(U)) != 0 && throw(InexactError())
        else
            (x.hi != 0) | (x.lo >> 2*nbits(T) != 0) && throw(InexactError())
        end
    else
        lo = x.lo % T | x.hi % T << nbits(U)
        hi = x.hi % T >> (nbits(T) - nbits(U))

    end
    WideUInt{T}(lo, hi)
end

convert{T<:OtherUnsigned}(::Type{WideUInt{T}}, x::T) = WideUInt{T}(x, 0)
function convert{T<:OtherUnsigned, U<:OtherInteger}(::Type{WideUInt{T}}, x::U)
    x < 0 && throw(InexactError())
    typemax(U) <= typemax(T) && return WideUInt{T}(x, 0)
    lo = x % T
    hi = (x >> nbits(T)) % T
    lo % U | (hi % U << nbits(T)) != x && throw(InexactError())
    WideUInt{T}(lo, hi)
end

function convert{T<:OtherUnsigned}(::Type{T}, x::WideUInt{T})
    x.hi != 0 && throw(InexactError())
    x.lo
end
function convert{T<:OtherInteger, U<:OtherUnsigned}(::Type{T}, x::WideUInt{U})
    r = x.lo % T | x.hi % T << nbits(U)
    WideUInt{U}(r) != x && throw(InexactError())
    r
end

promote_rule{T<:Unsigned, U<:Unsigned}(::Type{WideUInt{T}},
        ::Type{WideUInt{U}}) =
    WideUInt{promote_type(T, U)}
promote_rule{T<:Unsigned, U<:Unsigned}(::Type{WideUInt{T}}, ::Type{U}) =
    WideUInt{promote_type(T, U)}

# Bitwise operations

import Base: leading_zeros, leading_ones, trailing_zeros, trailing_ones
function leading_zeros{T<:Unsigned}(x::WideUInt{T})
    x.hi==0 && return nbits(T) + leading_zeros(x.lo)
    leading_zeros(x.hi)
end
function leading_ones{T<:Unsigned}(x::WideUInt{T})
    ~x.hi==0 && return nbits(T) + leading_ones(x.lo)
    leading_ones(x.hi)
end
function trailing_zeros{T<:Unsigned}(x::WideUInt{T})
    x.lo==0 && return nbits(T) + trailing_zeros(x.hi)
    trailing_zeros(x.lo)
end
function trailing_ones{T<:Unsigned}(x::WideUInt{T})
    ~x.lo==0 && return nbits(T) + trailing_ones(x.hi)
    trailing_ones(x.lo)
end

import Base: ~
~{T<:Unsigned}(x::WideUInt{T}) = WideUInt{T}(~x.lo, ~x.hi)

import Base: <<, >>, >>>
function <<{T<:Unsigned}(x::WideUInt{T}, y::Int)
    WideUInt{T}(x.lo << y, x.hi << y | x.lo >> (nbits(T) - y))
end
function >>{T<:Unsigned}(x::WideUInt{T}, y::Int)
    WideUInt{T}(x.lo >> y | x.hi << (nbits(T) - y), x.hi >> y)
end
>>>{T<:Unsigned}(x::WideUInt{T}, y::Int) = >>(x, y)

import Base: &, |, $
(&){T<:Unsigned}(x::WideUInt{T}, y::WideUInt{T}) =
    WideUInt{T}(x.lo & y.lo, x.hi & y.hi)
(|){T<:Unsigned}(x::WideUInt{T}, y::WideUInt{T}) =
    WideUInt{T}(x.lo | y.lo, x.hi | y.hi)
($){T<:Unsigned}(x::WideUInt{T}, y::WideUInt{T}) =
    WideUInt{T}(x.lo $ y.lo, x.hi $ y.hi)

# Comparisons

import Base: <
function <{T<:Unsigned}(x::WideUInt{T}, y::WideUInt{T})
    x.hi < y.hi && return true
    x.lo < y.lo
end

# Arithmetic operations

import Base: +, -, abs
+{T<:Unsigned}(x::WideUInt{T}) = x
-{T<:Unsigned}(x::WideUInt{T}) = WideUInt{T}(-x.lo, -x.hi)
abs{T<:Unsigned}(x::WideUInt{T}) = x

import Base: +, -
function wideadd{T<:Unsigned}(x::T, y::T)
    lo = x + y
    # c = x.lo + y.lo > typemax(T)
    c = x >= -y
    hi = c
    WideUInt{T}(lo, hi)
end
function +{T<:Unsigned}(x::WideUInt{T}, y::WideUInt{T})
    lo = x.lo + y.lo
    # c = x.lo + y.lo > typemax(T)
    c = x.lo >= -y.lo
    hi = x.hi + y.hi + c
    WideUInt{T}(lo, hi)
end
function widesub{T<:Unsigned}(x::T, y::T)
    lo = x - y
    # c = x.lo - y.lo < 0
    c = x < y
    hi = -c
    WideUInt{T}(lo, hi)
end
function -{T<:Unsigned}(x::WideUInt{T}, y::WideUInt{T})
    lo = x.lo - y.lo
    # c = x.lo - y.lo < 0
    c = x.lo < y.lo
    hi = x.hi - y.hi - c
    WideUInt{T}(lo, hi)
end

import Base: *, div, rem

lo{T<:Unsigned}(x::T) = x & (T(1) << halfnbits(T) - T(1))
hi{T<:Unsigned}(x::T) = x >>> halfnbits(T)
function widemul{T<:Unsigned}(x::T, y::T)
    r0 = lo(x) * lo(y)
    r1a = hi(x) * lo(y)
    r1b = lo(x) * hi(y)
    r2 = hi(x) * hi(y)
    WideUInt{T}(r0, r2) + wideadd(r1a, r1b) << halfnbits(T)
end
function *{T<:Unsigned}(x::WideUInt{T}, y::WideUInt{T})
    r0 = widemul(x.lo, y.lo)
    r1 = widemul(x.hi, y.lo) + widemul(x.lo, y.hi)
    r0 + r1 << nbits(T)
end

end
