module WideInts

export WideUInt

typealias BaseSigned Union{Int8, Int16, Int32, Int64, Int128}
typealias BaseUnsigned Union{UInt8, UInt16, UInt32, UInt64, UInt128}
typealias BaseInteger Union{BaseSigned, BaseUnsigned}

export UInt4
immutable UInt4 <: Unsigned
    val::UInt8
end

import Base: typemin, typemax, rem, convert, promote_rule
typemin(::Type{UInt4}) = UInt4(0x00)
typemax(::Type{UInt4}) = UInt4(0x0f)
rem(x::UInt4, ::Type{UInt4}) = x
rem(x::UInt4, ::Type{Bool}) = rem(x.val, Bool)
rem{T<:BaseInteger}(x::UInt4, ::Type{T}) = rem(x.val, T)
rem(x::Bool, ::Type{UInt4}) = UInt4(rem(x, UInt8))
rem{T<:BaseInteger}(x::T, ::Type{UInt4}) = UInt4(rem(x, UInt8) & 0x0f)
convert(::Type{UInt4}, x::UInt4) = x
convert(::Type{UInt4}, x::Bool) = UInt4(convert(UInt8, x))
function convert{T<:BaseInteger}(::Type{UInt4}, x::T)
    if (x<0) | (x>0x0f) throw(InexactError()) end
    rem(x, UInt4)
end
convert(::Type{Bool}, x::UInt4) = convert(Bool, x.val)
convert{T<:BaseInteger}(::Type{T}, x::UInt4) = convert(T, x.val)
promote_rule{T<:BaseUnsigned}(::Type{UInt4}, ::Type{T}) = T

import Base: leading_zeros, leading_ones, trailing_zeros, trailing_ones
leading_zeros(x::UInt4) = leading_zeros((x.val << 4) | 0x0f)
leading_ones(x::UInt4) = leading_ones(x.val << 4)
trailing_zeros(x::UInt4) = trailing_zeros(x.val | 0xf0)
trailing_ones(x::UInt4) = trailing_ones(x.val)
import Base: ~, &, |, $
~(x::UInt4) = UInt4(~x.val & 0x0f)
(&)(x::UInt4, y::UInt4) = UInt4(x.val & y.val)
(|)(x::UInt4, y::UInt4) = UInt4(x.val | y.val)
($)(x::UInt4, y::UInt4) = UInt4(x.val $ y.val)

import Base: <<, >>, >>>
<<(x::UInt4, y::Int) = UInt4((x.val << y) & 0x0f)
>>(x::UInt4, y::Int) = UInt4(x.val >> y)
>>>(x::UInt4, y::Int) = >>(x, y)

import Base: <, <=
<=(x::UInt4, y::UInt4) = x.val <= y.val
<(x::UInt4, y::UInt4) = x.val < y.val

import Base: +, -, abs, *, div, rem, fld, mod, cld
+(x::UInt4) = x
-(x::UInt4) = rem(-x.val, UInt4)
abs(x::UInt4) = x
+(x::UInt4, y::UInt4) = rem(x.val + y.val, UInt4)
-(x::UInt4, y::UInt4) = rem(x.val - y.val, UInt4)
*(x::UInt4, y::UInt4) = rem(x.val * y.val, UInt4)
div(x::UInt4, y::UInt4) = rem(div(x.val, y.val), UInt4)
rem(x::UInt4, y::UInt4) = rem(rem(x.val, y.val), UInt4)
fld(x::UInt4, y::UInt4) = rem(fld(x.val, y.val), UInt4)
mod(x::UInt4, y::UInt4) = rem(mod(x.val, y.val), UInt4)
cld(x::UInt4, y::UInt4) = rem(cld(x.val, y.val), UInt4)

# typealias OtherUnsigned Union{subtypes(Unsigned)...}
# typealias OtherInteger
#     Union{setdiff(subtypes(Integer), [Unsigned])..., OtherUnsigned}
typealias OtherSigned Union{BaseSigned}
typealias OtherUnsigned Union{UInt4, BaseUnsigned}
typealias OtherInteger Union{OtherSigned, OtherUnsigned}

immutable WideUInt{T<:Unsigned} <: Unsigned
    lo::T
    hi::T
end

# Creation and type conversions

import Base: typemin, typemax, rem, convert, promote_rule

nbits{T<:Unsigned}(::Type{T}) = 8*sizeof(T)
mask{T<:Unsigned}(::Type{T}) = ~T(0)
halfnbits{T<:Unsigned}(::Type{T}) = 4*sizeof(T)
halfmask{T<:Unsigned}(::Type{T}) = ~T(0) >>> halfnbits(T)

typemin{T<:Unsigned}(::Type{WideUInt{T}}) = WideUInt{T}(0, 0)
typemax{T<:Unsigned}(::Type{WideUInt{T}}) = WideUInt{T}(typemax(T), typemax(T))

# regular to wide
rem{T<:OtherUnsigned}(x::T, ::Type{WideUInt{T}}) = WideUInt{T}(x, 0)
rem{R<:Unsigned}(x::Bool, ::Type{WideUInt{R}}) = WideUInt{R}(x, 0)
function rem{T<:OtherInteger, R<:Unsigned}(x::T, ::Type{WideUInt{R}})
    info("rem T=$T R=$R x=$(hex(x))")
    r = WideUInt{R}(x % R, (x >> nbits(R)) % R)
    #info("rem T=$T R=$R x=$(hex(x)) r=$(hex(r))")
    r
end
convert{T<:OtherUnsigned}(::Type{WideUInt{T}}, x::T) = WideUInt{T}(x, 0)
convert{R<:Unsigned}(::Type{WideUInt{R}}, x::Bool) = WideUInt{R}(x, 0)
function convert{R<:Unsigned, T<:OtherInteger}(::Type{WideUInt{R}}, x::T)
    x < 0 && throw(InexactError())
    typemax(T) <= typemax(R) && return WideUInt{R}(x, 0)
    lo = x % R
    hi = (x >> nbits(R)) % R
    info("conv T=$T R=$R x=$(hex(x)) lo=$(hex(lo)) hi=$(hex(hi))")
    x >> 2*nbits(R) != 0 && throw(InexactError())
    WideUInt{R}(lo, hi)
end

# wide to wide
rem{T<:Unsigned}(x::WideUInt{T}, ::Type{WideUInt{T}}) = x
function rem{T<:Unsigned, R<:Unsigned}(x::WideUInt{T}, ::Type{WideUInt{R}})
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
function <={T<:Unsigned}(x::WideUInt{T}, y::WideUInt{T})
    x.hi < y.hi && return true
    x.hi > y.hi && return false
    x.lo <= y.lo
end
function <{T<:Unsigned}(x::WideUInt{T}, y::WideUInt{T})
    x.hi < y.hi && return true
    x.hi > y.hi && return false
    x.lo < y.lo
end

# Arithmetic operations

const have_base_overflow = try Base.add_overflow; true catch e false end
if have_base_overflow
    import Base: neg_overflow, abs_overflow,
        add_overflow, sub_overflow, mul_overflow, div_overflow, rem_overflow
else
    neg_overflow{T<:Unsigned}(x::T) = x != 0
    abs_overflow{T<:Unsigned}(x::T) = false
    # c = x + y > typemax(T)
    add_overflow{T<:Unsigned}(x::T, y::T) = x > ~y
    # c = x - y < 0
    sub_overflow{T<:Unsigned}(x::T, y::T) = x < y
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
function +{T<:Unsigned}(x::WideUInt{T}, y::WideUInt{T})
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
function -{T<:Unsigned}(x::WideUInt{T}, y::WideUInt{T})
    lo = x.lo - y.lo
    c = sub_overflow(x.lo, y.lo)
    hi = x.hi - y.hi - c
    WideUInt{T}(lo, hi)
end

################################################################################

#=
immutable HalfWideUInt{T} <: Unsigned
    lo::T
    hi::T
end

function is_normalized{T<:Unsigned}(x::HalfWideUInt{T})
    (lo >> halfnbits(T) == 0) & (hi >> halfnbits(T) == 0)
end

function normalize{T<:Unsigned}(x::HalfWideUInt{T})
    lo = x.lo & halfmask(T)
    hi = x.hi + lo >> halfnbits(T)
    @assert hi >> halfnbits(T) == 0
    HalfWideUInt{T}(lo, hi)
end


function rem{T<:OtherUnsigned}(x::Type{T}, ::Type{HalfWideUInt{T}})
    lo = x & halfmask(T)
    hi = x >> halfnbits(T)
    HalfWideUInt{T}(lo, hi)
end

convert{T<:OtherUnsigned}(::Type{HalfWideUInt{T}}, x::T) =
    rem(x, HalfWideUInt{T})


@inline function <<{T<:Unsigned}(x::HalfWideUInt{T}, y::Int)
    y >= halfnbits(T) && return HalfWideUInt{T}(0, x.lo << (y - halfnbits(T)))
    lo = (x.lo << y) & halfmask(T)
    hi = (x.hi << y) & halfmask(T) | x.lo >> (halfnbits(T) - y)
    HalfWideUInt{T}(lo, hi)
end
@inline function >>{T<:Unsigned}(x::HalfWideUInt{T}, y::Int)
    y >= halfnbits(T) && return HalfWideUInt{T}(x.hi >> (y - halfnbits(T)), 0)
    lo = x.lo >> y | (x.hi << (nbits(T) - y)) & halfmask(T)
    hi = x.hi >> y
    WideUInt{T}(lo, hi)
end
@inline >>>{T<:Unsigned}(x::WideUInt{T}, y::Int) = >>(x, y)


+{T<:Unsigned}(x::HalfWideUInt{T}) = x
-{T<:Unsigned}(x::HalfWideUInt{T}) = normalize(HalfWideUInt{T}(-x.lo, -y.hi))

+{T<:Unsigned}(x::HalfWideUInt{T}, y::HalfWideUInt{T}) =
    normalize(HalfWideUInt{T}(x.lo + y.lo, x.hi + y.hi))
-{T<:Unsigned}(x::HalfWideUInt{T}, y::HalfWideUInt{T}) =
    normalize(HalfWideUInt{T}(x.lo - y.lo, x.hi - y.hi))
=#

################################################################################

import Base: *, div, rem

halftype(::Type{UInt16}) = UInt8
halftype(::Type{UInt32}) = UInt16
halftype(::Type{UInt64}) = UInt32
halftype(::Type{UInt128}) = UInt64
halftype{T<:Unsigned}(::Type{WideUInt{T}}) = T

doubletype(::Type{UInt8}) = UInt16
doubletype(::Type{UInt16}) = UInt32
doubletype(::Type{UInt32}) = UInt64
doubletype(::Type{UInt64}) = UInt128

function dmul{T<:OtherUnsigned}(x::T, y::T)
    DT = doubletype(T)
    WT = WideUInt{T}
    WT(DT(x) * DT(y))
end

function split{T<:OtherUnsigned}(x::T)
    HT = halftype(T)
    WideUInt{HT}(x)
end
split{T<:Unsigned}(x::WideUInt{T}) = x

function dmul1{T<:Unsigned, HT<:Unsigned}(x::T, y::HT)
    @assert HT == halftype(T)
    x2 = split(x)
    rlo = dmul(x2.lo, y)
    rhi = dmul(x2.hi, y)
    WT = WideUInt{T}
    WT(rlo) + WT(rhi) << nbits(HT)
end
function dmul{T<:Unsigned}(x::WideUInt{T}, y::WideUInt{T})
    WT = WideUInt{T}
    WWT = WideUInt{WT}
    y2 = split(y)
    rlo = dmul1(x, y2.lo)
    rhi = dmul1(x, y2.hi)
    WWT(rlo) + WWT(rhi) << nbits(T)
end

function *{T<:Unsigned}(x::WideUInt{T}, y::WideUInt{T})
    r = dmul(x, y)
    r.hi != 0 && throw(OverflowError())
    r.lo
end

#=
#TODO: maybe remove this
function ddiv{T<:OtherInteger, HT<:OtherInteger}(x::T, y::HT)
    @assert HT == halftype(T)
    q, r = divrem(x, T(y))
    q, r % HT
end
ddiv{T<:OtherInteger}(x::T, y::T) = divrem(x, y)
=#

function ddiv1{T<:Unsigned, HT<:Unsigned}(x::WideUInt{T}, y::HT)
    info("ddiv1 x=$(hex(x))::WideUInt{$T} y=$(hex(y))::$HT")
    @assert HT == halftype(T)
    y == 0 && throw(DivideError())
    WT = WideUInt{T}
    nb = nbits(HT)
    # info("    T(0)=$(hex(T(0)))")
    q, r = WT(0), x
    info("    q=$(hex(q)) r=$(hex(r))")
    @assert x == q * y + r
    Base.@nexprs 3 step -> begin
        off = 3 - step
        t, _ = ddiv((r >> off*nb) % T, y % T)
        q += WT(t) << off*nb
        r -= dmul1(t, y) << off*nb
        @assert x == q * y + r
        @assert r >> off*nb < y
    end
    q, r % HT
end

#=
function ddiv{T<:Unsigned}(x::WideUInt{T}, y::WideUInt{T})
    y == 0 && throw(DivideError())
    HT = halftype(T)
    WT = WideUInt{T}
    nb = nbits(HT)
    q, r = WT(0), x
    @assert x == q * y + r
    while r >= y
        rprev = r
        # Find suitable offset for y
        yoffset = max(0, nbits(WT) - leading_zeros(y) - nbits(HT))
        @assert (y >> yoffset) <= typemax(HT)
        yhi = (y >> yoffset) % HT
        # Take a step
        t, _ = ddiv1(r >> yoffset, yhi)
        if t * y > r
            # Underflow
            t -= 1
        end
        @assert t * y <= r
        q += t
        r -= t * y
        # Check variant and invariant
        @assert r < rpref
        @assert x == q * y + r
    end
    q, r
end
=#

function div{T<:Unsigned}(x::WideUInt{T}, y::WideUInt{T})
    y == 0 && throw(DivideError())
    WT = WideUInt{T}
    # nb = nbits(T)
    ldz_x = leading_zeros(x)
    ldz_y = leading_zeros(y)
    x = x << ldz_x
    y = y << ldz_y
    q, r = WT(0), x
    # ldz_q = 0
    ldz_r = ldz_x
    @assert x >> ldz_x == q * (y >> ldz_y) + (r >> ldz_r)
    while r >> ldz_r >= y >> ldz_y
        rprev = r
        # Perform approximate division
        if y.hi == typemax(T)
            t = y.lo
        else
            t = div(x.hi, y.hi + T(1))
        end
        ldz_t = ldz_x - ldz_y
        q += t >> ldz_t
        r -= t * y
        # Check variant and invariant
        @assert r < rpref
        @assert x == q * y + r
    end
    q, r
end

end
