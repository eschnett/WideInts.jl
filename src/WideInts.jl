module WideInts

export WideUInt, WideInt

using SmallInts

typealias BaseUnsigned Union{UInt8, UInt16, UInt32, UInt64, UInt128}
typealias BaseSigned Union{Int8, Int16, Int32, Int64, Int128}
typealias BaseInteger Union{BaseSigned, BaseUnsigned}

typealias OtherUnsigned Union{UInt1, UInt2, UInt4, BaseUnsigned}
typealias OtherSigned Union{Int1, Int2, Int4, BaseSigned}
typealias OtherInteger Union{OtherSigned, OtherUnsigned}

immutable WideUInt{T<:Unsigned} <: Unsigned
    lo::T
    hi::T
end
immutable WideInt{S<:Signed, T<:Unsigned} <: Signed
    lo::T
    hi::S
end

nbits{T}(::Type{T}) = 8*sizeof(T)
nbits{T<:Union{UInt1,Int1}}(::Type{T}) = 1
nbits{T<:Union{UInt2,Int2}}(::Type{T}) = 2
nbits{T<:Union{UInt4,Int4}}(::Type{T}) = 4
nbits{T}(::Type{WideUInt{T}}) = 2*nbits(T)
nbits{S,T}(::Type{WideInt{S,T}}) = nbits(S)+nbits(T)

bitshift(x,n) = ifelse(n>=0, x<<n, x>>n)

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
function bin{S,T}(x::WideInt{S,T}, pad::Int=0)
    x<0 && return "-" * bin(-x, pad)
    lopad = nbits(T)
    hipad = pad - lopad
    x.hi == 0 && hipad <= 0 && return bin(x.lo, lopad)
    bin(x.hi, hipad) * bin(x.lo, lopad)
end
function hex{S,T}(x::WideInt{S,T}, pad::Int=0)
    x<0 && return "-" * hex(-x, pad)
    lopad = nbits(T) ÷ 4
    hipad = pad - lopad
    x.hi == 0 && hipad <= 0 && hex(x.lo, lopad)
    hex(x.hi, hipad) * hex(x.lo, lopad)
end
show{S,T}(io::IO, x::WideInt{S,T}) = print(io, "0x", hex(x))

# Creation and type conversions

import Base: typemin, typemax, rem, convert, promote_rule

mask{T}(::Type{T}) = ~T(0)
halfnbits{T}(::Type{T}) = nbits(T) ÷ 2
halfmask{T}(::Type{T}) = ~T(0) >>> halfnbits(T)

typemin{T}(::Type{WideUInt{T}}) = WideUInt{T}(0, 0)
typemax{T}(::Type{WideUInt{T}}) = WideUInt{T}(typemax(T), typemax(T))
typemin{S,T}(::Type{WideInt{S,T}}) = WideInt{S,T}(typemin(S), typemin(T))
typemax{S,T}(::Type{WideInt{S,T}}) = WideInt{S,T}(typemax(S), typemax(T))

# regular to wide
rem{T<:OtherUnsigned}(x::T, ::Type{WideUInt{T}}) = WideUInt{T}(x, 0)
rem{R<:Unsigned}(x::Bool, ::Type{WideUInt{R}}) = WideUInt{R}(x, 0)
@inline function rem{T<:OtherInteger, R<:Unsigned}(x::T, ::Type{WideUInt{R}})
    WideUInt{R}(x % R, (x >> nbits(R)) % R)
end

rem{S<:OtherSigned, T<:OtherUnsigned}(x::T, ::Type{WideInt{S,T}}) =
    WideInt{S,T}(x, 0)
rem{S<:Signed, R<:Unsigned}(x::Bool, ::Type{WideInt{S,R}}) = WideInt{S,R}(x, 0)
@inline function rem{T<:OtherInteger, S<:Signed, R<:Unsigned}(
        x::T, ::Type{WideInt{S,R}})
    WideInt{S,R}(x % R, (x >> nbits(R)) % S)
end

convert{T<:OtherUnsigned}(::Type{WideUInt{T}}, x::T) = WideUInt{T}(x, 0)
convert{R<:Unsigned}(::Type{WideUInt{R}}, x::Bool) = WideUInt{R}(x, 0)
function convert{R<:Unsigned, T<:OtherInteger}(::Type{WideUInt{R}}, x::T)
    x >> 2*nbits(R) != 0 && throw(InexactError())
    rem(x, WideUInt{R})
end

convert{S<:OtherSigned, T<:OtherUnsigned}(::Type{WideInt{S,T}}, x::T) =
    WideInt{S,T}(x, 0)
convert{S<:Signed, R<:Unsigned}(::Type{WideInt{S,R}}, x::Bool) =
    WideInt{S,R}(x, 0)
function convert{S<:Signed, R<:Unsigned, T<:OtherInteger}(
        ::Type{WideInt{S,R}}, x::T)
    x >> (nbits(R)+nbits(S)) != -signbit(x) && throw(InexactError())
    rem(x, WideInt{S,R})
end

# wide to wide
rem{T<:Unsigned}(x::WideUInt{T}, ::Type{WideUInt{T}}) = x
@inline function rem{T<:Unsigned, R<:Unsigned}(
        x::WideUInt{T}, ::Type{WideUInt{R}})
    lo = x.lo % R | x.hi % R << nbits(T)
    hi = (x.lo >> nbits(R) | bitshift(x.hi, nbits(T) - nbits(R))) % R
    WideUInt{R}(lo, hi)
end

rem{S<:Signed, T<:Unsigned}(x::WideInt{S,T}, ::Type{WideInt{S,T}}) = x
@inline function rem{S<:Signed, T<:Unsigned, R<:Signed, U<:Unsigned}(
        x::WideInt{S,T}, ::Type{WideInt{R,U}})
    lo = x.lo % U | x.hi % U << nbits(T)
    hi = (x.lo >> nbits(U) | bitshift(x.hi, nbits(T) - nbits(U))) % R
    WideInt{R,U}(lo, hi)
end

convert{T<:OtherUnsigned}(::Type{WideUInt{T}}, x::WideUInt{T}) = x
function convert{R<:Unsigned, T<:Unsigned}(::Type{WideUInt{R}}, x::WideUInt{T})
    if 2*nbits(R) >= nbits(T)
        x.hi >> (2*nbits(R) - nbits(T)) != 0 && throw(InexactError())
    else
        (x.hi != 0) | (x.lo >> 2*nbits(T) != 0) && throw(InexactError())
    end
    rem(x, WideUInt{R})
end

function convert{S<:OtherSigned, T<:OtherUnsigned}(
        ::Type{WideInt{S,T}}, x::WideInt{S,T})
    x
end
function convert{S<:Signed, R<:Unsigned, T<:Signed, U<:Unsigned}(
        ::Type{WideInt{S,R}}, x::WideInt{T,U})
    r = rem(x, WideInt{S,R})
    if nbits(S)+nbits(R) >= nbits(T)
        x.hi >> (2*nbits(R) - nbits(T)) != -signbit(r) &&
            throw(InexactError())
    else
        (x.hi != -signbit(r)) | (x.lo >> (nbits(T)+nbits(U)) != -signbit(r)) &&
            throw(InexactError())
    end
    rem(x, WideInt{S,R})
end

# wide to regular
rem{T<:OtherUnsigned}(x::WideUInt{T}, ::Type{T}) = x.lo
rem{T<:Unsigned}(x::WideUInt{T}, ::Type{Bool}) = x.lo % Bool
function rem{T<:Unsigned, R<:OtherInteger}(x::WideUInt{T}, ::Type{R})
    x.lo % R | x.hi % R << nbits(T)
end

rem{S<:OtherSigned, T<:OtherUnsigned}(x::WideInt{S,T}, ::Type{T}) = x.lo
rem{S<:Signed, T<:Unsigned}(x::WideInt{S,T}, ::Type{Bool}) = x.lo % Bool
function rem{S<:Signed, T<:Unsigned, R<:OtherInteger}(
        x::WideInt{S,T}, ::Type{R})
    x.lo % R | x.hi % R << nbits(T)
end

function convert{T<:OtherUnsigned}(::Type{T}, x::WideUInt{T})
    x.hi != 0 && throw(InexactError())
    rem(x, T)
end
function convert{T<:Unsigned}(::Type{Bool}, x::WideUInt{T})
    (x.lo > 1) | (x.hi > 0) && throw(InexactError())
    rem(x, Bool)
end
function convert{R<:OtherInteger, T<:Unsigned}(::Type{R}, x::WideUInt{T})
    if (x.lo > typemax(R)) | (x.hi > typemax(R) >> nbits(T))
        throw(InexactError())
    end
    rem(x, R)
end

function convert{S<:OtherSigned, T<:OtherUnsigned}(::Type{T}, x::WideInt{S,T})
    x.hi != -signbit(x.lo) && throw(InexactError())
    rem(x, T)
end
function convert{S<:Signed, T<:Unsigned}(::Type{Bool}, x::WideInt{S,T})
    (x.lo & ~T(1) != 0) | (x.hi != 0) && throw(InexactError())
    rem(x, Bool)
end
function convert{R<:OtherInteger, S<:Signed, T<:Unsigned}(
        ::Type{R}, x::WideInt{S,T})
    if !((typemin(R) <= x.lo <= typemax(R)) &
            (typemin(R) >> nbits(T) <= x.hi <= typemax(R) >> nbits(T)))
        throw(InexactError())
    end
    rem(x, R)
end

promote_rule{T<:Unsigned, U<:Unsigned}(::Type{WideUInt{T}},
        ::Type{WideUInt{U}}) =
    WideUInt{promote_type(T, U)}
promote_rule{T<:Unsigned, U<:Unsigned}(::Type{WideUInt{T}}, ::Type{U}) =
    WideUInt{promote_type(T, U)}

promote_rule{S<:Signed, R<:Unsigned, T<:Signed, U<:Unsigned}(
        ::Type{WideInt{S,R}}, ::Type{WideInt{T,U}}) =
    WideInt{promote_type(S, T), promote_type(R, U)}
promote_rule{S<:Signed, R<:Unsigned, T<:Signed}(
        ::Type{WideInt{S,R}}, ::Type{T}) =
    WideInt{promote_type(S,T), promote_type(R,typeof(unsigned(T(0))))}

# Bitwise operations

import Base: count_ones, leading_zeros, trailing_zeros

function count_ones{T}(x::WideUInt{T})
    count_ones(x.lo) + count_ones(x.hi)
end
function leading_zeros{T}(x::WideUInt{T})
    x.hi==0 && return nbits(T) + leading_zeros(x.lo)
    leading_zeros(x.hi)
end
function trailing_zeros{T}(x::WideUInt{T})
    x.lo==0 && return nbits(T) + trailing_zeros(x.hi)
    trailing_zeros(x.lo)
end

function count_ones{S,T}(x::WideInt{S,T})
    count_ones(x.lo) + count_ones(x.hi)
end
function leading_zeros{S,T}(x::WideInt{S,T})
    x.hi==0 && return nbits(S) + leading_zeros(x.lo)
    leading_zeros(x.hi)
end
function trailing_zeros{S,T}(x::WideInt{S,T})
    x.lo==0 && return nbits(T) + trailing_zeros(x.hi)
    trailing_zeros(x.lo)
end

import Base: ~

~{T}(x::WideUInt{T}) = WideUInt{T}(~x.lo, ~x.hi)
~{S,T}(x::WideInt{S,T}) = WideInt{S,T}(~x.lo, ~x.hi)

import Base: <<, >>, >>>

@inline function <<{T}(x::WideUInt{T}, y::Int)
    y >= nbits(T) && return WideUInt{T}(0, x.lo << (y - nbits(T)))
    WideUInt{T}(x.lo << y, x.hi << y | x.lo >> (nbits(T) - y))
end
@inline function >>{T}(x::WideUInt{T}, y::Int)
    y >= nbits(T) && return WideUInt{T}(x.hi >> (y - nbits(T)), 0)
    WideUInt{T}(x.lo >> y | x.hi << (nbits(T) - y), x.hi >> y)
end
@inline >>>{T}(x::WideUInt{T}, y::Int) = >>(x, y)

@inline function <<{S,T}(x::WideInt{S,T}, y::Int)
    y >= nbits(T) && return WideInt{S,T}(0, x.lo << (y - nbits(T)))
    WideInt{S,T}(x.lo << y, x.hi << y | x.lo >> (nbits(T) - y))
end
@inline function >>{S,T}(x::WideInt{S,T}, y::Int)
    y >= nbits(S) && return WideInt{S,T}(x.hi >> (y - nbits(T)), signbit(x.hi))
    WideInt{S,T}(x.lo >> y | x.hi << (nbits(T) - y), x.hi >> y)
end
@inline function >>>{S,T}(x::WideInt{S,T}, y::Int)
    y >= nbits(S) && return WideInt{S,T}(x.hi >> (y - nbits(T)), signbit(x.hi))
    WideInt{S,T}(x.lo >> y | x.hi << (nbits(T) - y), x.hi >>> y)
end

import Base: &, |, $

(&){T}(x::WideUInt{T}, y::WideUInt{T}) =
    WideUInt{T}(x.lo & y.lo, x.hi & y.hi)
(|){T}(x::WideUInt{T}, y::WideUInt{T}) =
    WideUInt{T}(x.lo | y.lo, x.hi | y.hi)
($){T}(x::WideUInt{T}, y::WideUInt{T}) =
    WideUInt{T}(x.lo $ y.lo, x.hi $ y.hi)

(&){S,T}(x::WideInt{S,T}, y::WideInt{S,T}) =
    WideInt{S,T}(x.lo & y.lo, x.hi & y.hi)
(|){S,T}(x::WideInt{S,T}, y::WideInt{S,T}) =
    WideInt{S,T}(x.lo | y.lo, x.hi | y.hi)
($){S,T}(x::WideInt{S,T}, y::WideInt{S,T}) =
    WideInt{S,T}(x.lo $ y.lo, x.hi $ y.hi)

# Comparisons

import Base: <

@inline function <{T}(x::WideUInt{T}, y::WideUInt{T})
    x.hi < y.hi && return true
    x.hi > y.hi && return false
    x.lo < y.lo
end

@inline function <{S,T}(x::WideInt{S,T}, y::WideInt{S,T})
    x.hi < y.hi && return true
    x.hi > y.hi && return false
    x.lo < y.lo
end

import Base: >, <=, >=

@inline >{T}(x::WideUInt{T}, y::WideUInt{T}) = y < x
@inline <={T}(x::WideUInt{T}, y::WideUInt{T}) = !(x > y)
@inline >={T}(x::WideUInt{T}, y::WideUInt{T}) = !(x < y)

@inline >{S,T}(x::WideInt{S,T}, y::WideInt{S,T}) = y < x
@inline <={S,T}(x::WideInt{S,T}, y::WideInt{S,T}) = !(x > y)
@inline >={S,T}(x::WideInt{S,T}, y::WideInt{S,T}) = !(x < y)

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

import Base: +, -, abs, signbit

+{T}(x::WideUInt{T}) = x
function -{T}(x::WideUInt{T})
    lo = -x.lo
    c = neg_overflow(x.lo)
    hi = -x.hi - c
    WideUInt{T}(lo, hi)
end
abs{T}(x::WideUInt{T}) = x
signbit{T}(x::WideUInt{T}) = false

+{S,T}(x::WideInt{S,T}) = x
function -{S,T}(x::WideInt{S,T})
    lo = -x.lo
    c = neg_overflow(x.lo)
    hi = -x.hi - c
    WideInt{S,T}(lo, hi)
end
function abs{S,T}(x::WideInt{S,T})
    s = -signbit(x) % WideInt{S,T}
    x $ s - s
end
signbit{S,T}(x::WideInt{S,T}) = signbit(x.hi)

import Base: +, -

@inline function wideadd{T<:Unsigned}(x::T, y::T)
    lo = x + y
    c = add_overflow(x, y)
    hi = c
    WideUInt{T}(lo, hi)
end
function widesub{T<:Unsigned}(x::T, y::T)
    lo = x - y
    c = sub_overflow(x, y)
    hi = -c
    WideUInt{T}(lo, hi)
end

@inline function +{T}(x::WideUInt{T}, y::WideUInt{T})
    lo = x.lo + y.lo
    c = add_overflow(x.lo, y.lo)
    hi = x.hi + y.hi + c
    WideUInt{T}(lo, hi)
end
@inline function -{T}(x::WideUInt{T}, y::WideUInt{T})
    lo = x.lo - y.lo
    c = sub_overflow(x.lo, y.lo)
    hi = x.hi - y.hi - c
    WideUInt{T}(lo, hi)
end

@inline function +{S,T}(x::WideInt{S,T}, y::WideInt{S,T})
    lo = x.lo + y.lo
    c = add_overflow(x.lo, y.lo)
    hi = x.hi + y.hi + c
    WideInt{S,T}(lo, hi)
end
@inline function -{S,T}(x::WideInt{S,T}, y::WideInt{S,T})
    lo = x.lo - y.lo
    c = sub_overflow(x.lo, y.lo)
    hi = x.hi - y.hi - c
    WideInt{S,T}(lo, hi)
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
