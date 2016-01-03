module WideInts

export WideUInt

using SmallInts

typealias BaseSigned Union{Int8, Int16, Int32, Int64, Int128}
typealias BaseUnsigned Union{UInt8, UInt16, UInt32, UInt64, UInt128}
typealias BaseInteger Union{BaseSigned, BaseUnsigned}

typealias OtherSigned Union{Int1, Int2, Int4, BaseSigned}
typealias OtherUnsigned Union{UInt1, UInt2, UInt4, BaseUnsigned}
typealias OtherInteger Union{OtherSigned, OtherUnsigned}

immutable WideUInt{T<:Unsigned} <: Unsigned
    lo::T
    hi::T
end

nbits{T}(::Type{T}) = 8*sizeof(T)
nbits{T<:Union{UInt1,Int1}}(::Type{T}) = 1
nbits{T<:Union{UInt2,Int2}}(::Type{T}) = 2
nbits{T<:Union{UInt4,Int4}}(::Type{T}) = 4
nbits{T}(::Type{WideUInt{T}}) = 2*nbits(T)

import Base: bin, hex, show
function bin{T}(x::WideUInt{T}, pad::Int=0)
    lopad = nbits(T)
    hipad = pad - lopad
    x.hi == 0 && hipad <= 0 && return bin(x.lo, lopad)
    bin(x.hi, hipad) * bin(x.lo, lopad)
end
function hex{T}(x::WideUInt{T}, pad::Int=0)
    lopad = nbits(T) ÷ 4
    hipad = pad - lopad
    x.hi == 0 && hipad <= 0 && hex(x.lo, lopad)
    hex(x.hi, hipad) * hex(x.lo, lopad)
end
show{T}(io::IO, x::WideUInt{T}) = print(io, "0x", hex(x))

# Creation and type conversions

import Base: typemin, typemax, rem, convert, promote_rule

mask{T<:Unsigned}(::Type{T}) = ~T(0)
halfnbits{T<:Unsigned}(::Type{T}) = nbits(T) ÷ 2
halfmask{T<:Unsigned}(::Type{T}) = ~T(0) >>> halfnbits(T)

typemin{T<:Unsigned}(::Type{WideUInt{T}}) = WideUInt{T}(0, 0)
typemax{T<:Unsigned}(::Type{WideUInt{T}}) = WideUInt{T}(typemax(T), typemax(T))

# regular to wide
rem{T<:OtherUnsigned}(x::T, ::Type{WideUInt{T}}) = WideUInt{T}(x, 0)
rem{R<:Unsigned}(x::Bool, ::Type{WideUInt{R}}) = WideUInt{R}(x, 0)
@inline function rem{T<:OtherInteger, R<:Unsigned}(x::T, ::Type{WideUInt{R}})
    WideUInt{R}(x % R, (x >> nbits(R)) % R)
end
convert{T<:OtherUnsigned}(::Type{WideUInt{T}}, x::T) = WideUInt{T}(x, 0)
convert{R<:Unsigned}(::Type{WideUInt{R}}, x::Bool) = WideUInt{R}(x, 0)
function convert{R<:Unsigned, T<:OtherInteger}(::Type{WideUInt{R}}, x::T)
    x < 0 && throw(InexactError())
    typemax(T) <= typemax(R) && return WideUInt{R}(x, 0)
    lo = x % R
    hi = (x >> nbits(R)) % R
    x >> 2*nbits(R) != 0 && throw(InexactError())
    WideUInt{R}(lo, hi)
end

# wide to wide
rem{T<:Unsigned}(x::WideUInt{T}, ::Type{WideUInt{T}}) = x
@inline function rem{T<:Unsigned, R<:Unsigned}(x::WideUInt{T}, ::Type{WideUInt{R}})
    if nbits(R) < nbits(T)
        lo = x.lo % R
        hi = (x.lo >> nbits(R) | x.hi << (nbits(T) - nbits(R))) % R
    else
        lo = x.lo % R | x.hi % R << nbits(T)
        hi = x.hi % R >> (nbits(R) - nbits(T))
    end
    WideUInt{R}(lo, hi)
end
convert{T<:OtherUnsigned}(::Type{WideUInt{T}}, x::WideUInt{T}) = x
function convert{R<:Unsigned, T<:Unsigned}(::Type{WideUInt{R}}, x::WideUInt{T})
    if nbits(R) < nbits(T)
        lo = x.lo % R
        hi = (x.lo >> nbits(R) | x.hi << (nbits(T) - nbits(R))) % R
        if 2*nbits(R) >= nbits(T)
            x.hi >> (2*nbits(R) - nbits(T)) != 0 && throw(InexactError())
        else
            (x.hi != 0) | (x.lo >> 2*nbits(T) != 0) && throw(InexactError())
        end
    else
        lo = x.lo % R | x.hi % R << nbits(T)
        hi = x.hi % R >> (nbits(R) - nbits(T))
    end
    WideUInt{R}(lo, hi)
end

# wide to regular
rem{T<:OtherUnsigned}(x::WideUInt{T}, ::Type{T}) = x.lo
rem{T<:Unsigned}(x::WideUInt{T}, ::Type{Bool}) = x.lo % Bool
function rem{T<:Unsigned, R<:OtherInteger}(x::WideUInt{T}, ::Type{R})
    x.lo % R | x.hi % R << nbits(T)
end
function convert{T<:OtherUnsigned}(::Type{T}, x::WideUInt{T})
    x.hi != 0 && throw(InexactError())
    x.lo
end
function convert{T<:Unsigned}(::Type{Bool}, x::WideUInt{T})
    (x.lo > 1) | (x.hi > 0) && throw(InexactError())
    x.lo % Bool
end
function convert{R<:OtherInteger, T<:Unsigned}(::Type{R}, x::WideUInt{T})
    if (x.lo > typemax(R)) | (x.hi > typemax(R) >> nbits(T))
        throw(InexactError())
    end
    x.lo % R | x.hi % R << nbits(T)
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
@inline function <<{T<:Unsigned}(x::WideUInt{T}, y::Int)
    y >= nbits(T) && return WideUInt{T}(0, x.lo << (y - nbits(T)))
    WideUInt{T}(x.lo << y, x.hi << y | x.lo >> (nbits(T) - y))
end
@inline function >>{T<:Unsigned}(x::WideUInt{T}, y::Int)
    y >= nbits(T) && return WideUInt{T}(x.hi >> (y - nbits(T)), 0)
    WideUInt{T}(x.lo >> y | x.hi << (nbits(T) - y), x.hi >> y)
end
@inline >>>{T<:Unsigned}(x::WideUInt{T}, y::Int) = >>(x, y)

import Base: &, |, $
(&){T<:Unsigned}(x::WideUInt{T}, y::WideUInt{T}) =
    WideUInt{T}(x.lo & y.lo, x.hi & y.hi)
(|){T<:Unsigned}(x::WideUInt{T}, y::WideUInt{T}) =
    WideUInt{T}(x.lo | y.lo, x.hi | y.hi)
($){T<:Unsigned}(x::WideUInt{T}, y::WideUInt{T}) =
    WideUInt{T}(x.lo $ y.lo, x.hi $ y.hi)

# Comparisons

import Base: <, <=
@inline function <={T<:Unsigned}(x::WideUInt{T}, y::WideUInt{T})
    x.hi < y.hi && return true
    x.hi > y.hi && return false
    x.lo <= y.lo
end
@inline function <{T<:Unsigned}(x::WideUInt{T}, y::WideUInt{T})
    x.hi < y.hi && return true
    x.hi > y.hi && return false
    x.lo < y.lo
end
import Base: >, >=
@inline >{T<:Unsigned}(x::WideUInt{T}, y::WideUInt{T}) = y < x
@inline >={T<:Unsigned}(x::WideUInt{T}, y::WideUInt{T}) = y <= x

# Arithmetic operations

const have_base_overflow = try Base.add_overflow; true catch e false end
if have_base_overflow
    import Base: neg_overflow, abs_overflow,
        add_overflow, sub_overflow, mul_overflow, div_overflow, rem_overflow
else
    neg_overflow{T<:Unsigned}(x::T) = x != 0
    abs_overflow{T<:Unsigned}(x::T) = false
    # c = x + y > typemax(T)
    @inline add_overflow{T<:Unsigned}(x::T, y::T) = x > ~y
    # c = x - y < 0
    @inline sub_overflow{T<:Unsigned}(x::T, y::T) = x < y
    mul_overflow{T<:Unsigned}(x::T, y::T) = y!=0 && x>div(typemax(T), y)
    div_overflow{T<:Unsigned}(x::T, y::T) = false
    rem_overflow{T<:Unsigned}(x::T, y::T) = false
end

import Base: +, -, abs

+{T<:Unsigned}(x::WideUInt{T}) = x
function -{T<:Unsigned}(x::WideUInt{T})
    lo = -x.lo
    c = neg_overflow(x.lo)
    hi = -x.hi - c
    WideUInt{T}(lo, hi)
end
abs{T<:Unsigned}(x::WideUInt{T}) = x

import Base: +, -
@inline function wideadd{T<:Unsigned}(x::T, y::T)
    lo = x + y
    c = add_overflow(x, y)
    hi = c
    WideUInt{T}(lo, hi)
end
@inline function +{T<:Unsigned}(x::WideUInt{T}, y::WideUInt{T})
    lo = x.lo + y.lo
    c = add_overflow(x.lo, y.lo)
    hi = x.hi + y.hi + c
    WideUInt{T}(lo, hi)
end
function widesub{T<:Unsigned}(x::T, y::T)
    lo = x - y
    c = sub_overflow(x, y)
    hi = -c
    WideUInt{T}(lo, hi)
end
@inline function -{T<:Unsigned}(x::WideUInt{T}, y::WideUInt{T})
    lo = x.lo - y.lo
    c = sub_overflow(x.lo, y.lo)
    hi = x.hi - y.hi - c
    WideUInt{T}(lo, hi)
end

import Base: *, divrem, div, rem, fldmod, fld, mod

halftype(::Type{UInt2}) = UInt1
halftype(::Type{UInt4}) = UInt2
halftype(::Type{UInt8}) = UInt4
halftype(::Type{UInt16}) = UInt8
halftype(::Type{UInt32}) = UInt16
halftype(::Type{UInt64}) = UInt32
halftype(::Type{UInt128}) = UInt64
halftype{T<:Unsigned}(::Type{WideUInt{T}}) = T

doubletype(::Type{UInt1}) = UInt2
doubletype(::Type{UInt2}) = UInt4
doubletype(::Type{UInt4}) = UInt8
doubletype(::Type{UInt8}) = UInt16
doubletype(::Type{UInt16}) = UInt32
doubletype(::Type{UInt32}) = UInt64
doubletype(::Type{UInt64}) = UInt128

@inline function dmul{T<:OtherUnsigned}(x::T, y::T)
    DT = doubletype(T)
    WT = WideUInt{T}
    ((x%DT) * (y%DT)) % WT
end

function split{T<:OtherUnsigned}(x::T)
    HT = halftype(T)
    WideUInt{HT}(x)
end
split{T<:Unsigned}(x::WideUInt{T}) = x

@inline function dmul1{T<:Unsigned, HT<:Unsigned}(x::T, y::HT)
    @assert HT == halftype(T)
    x2 = split(x)
    rlo = dmul(x2.lo, y)
    rhi = dmul(x2.hi, y)
    WT = WideUInt{T}
    rlo%WT + (rhi%WT) << nbits(HT)
end
@inline function dmul{T<:Unsigned}(x::WideUInt{T}, y::WideUInt{T})
    WT = WideUInt{T}
    WWT = WideUInt{WT}
    y2 = split(y)
    rlo = dmul1(x, y2.lo)
    rhi = dmul1(x, y2.hi)
    rlo%WWT + (rhi%WWT) << nbits(T)
end

function *{T<:Unsigned}(x::WideUInt{T}, y::WideUInt{T})
    r = dmul(x, y)
    # r.hi != 0 && throw(OverflowError())
    r.lo
end

function divrem{T<:Unsigned}(x::WideUInt{T}, y::WideUInt{T})
    y == 0 && throw(DivideError())
    # Initial choice
    WT = WideUInt{T}
    q, r = WT(0), x
    @assert x == q * y + r
    while r >= y
        rprev = r
        # Find leading one bit in r
        ilogb_r = nbits(WT) - leading_zeros(r) - 1
        if ilogb_r < nbits(T)
            # Both high elements are zero
            @assert y.hi == 0
            t,u = divrem(r.lo, y.lo)
            q += t
            r = WT(u)
            break
        end
        r1 = rem(r >> (ilogb_r - nbits(T) + 1), T)
        y1 = rem((y - WT(1)) >> (ilogb_r - nbits(T) + 1), T) + T(1)   # round up
        if y1 == 0
            # overflow; r and y must be almost equal
            t = T(1)
        else
            t = div(r1, y1)
            if t == 0
                # underflow; r and y must be almost equal
                t = T(1)
            end
        end
        @assert t > 0
        @assert r >= WT(t) * y
        q += WT(t)
        r -= WT(t) * y
        # Check variant and invariant
        @assert r < rprev
        @assert x == q * y + r
    end
    @assert x == q * y + r
    @assert r < y
    q, r
end

div{T<:Unsigned}(x::WideUInt{T}, y::WideUInt{T}) = divrem(x, y)[1]
rem{T<:Unsigned}(x::WideUInt{T}, y::WideUInt{T}) = divrem(x, y)[2]

fldmod{T<:Unsigned}(x::WideUInt{T}, y::WideUInt{T}) = divrem(x, y)
fld{T<:Unsigned}(x::WideUInt{T}, y::WideUInt{T}) = fldmod(x, y)[1]
mod{T<:Unsigned}(x::WideUInt{T}, y::WideUInt{T}) = fldmod(x, y)[2]

end
