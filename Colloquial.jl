@use "." => Units abbr scaler Dimension litre

abstract type Counting <: Dimension end

struct Piece <: Counting value::Int end
const piece = Piece
scaler(::Type{Piece}) = 1
abbr(::Type{Piece}) = "pce"

struct Box{c} <: Counting value::Int end
scaler(::Type{Box{c}}) where c = c.value * scaler(typeof(c))
abbr(::Type{Box{c}}) where c = "box($(c.value)$(abbr(typeof(c))))"
Base.promote_rule(::Type{Box{a}}, ::Type{Box{b}}) where {a,b} = Piece
Base.promote_rule(::Type{<:Box}, ::Type{Piece}) = Piece

struct Carton{c} <: Counting value::Int end
scaler(::Type{Carton{c}}) where c = c.value * scaler(typeof(c))
abbr(::Type{Carton{c}}) where c = "carton($(c.value)$(abbr(typeof(c))))"
Base.promote_rule(::Type{Carton{a}}, ::Type{Carton{b}}) where {a,b} = Piece
Base.promote_rule(::Type{<:Carton}, ::Type{Piece}) = Piece
Base.promote_rule(::Type{<:Carton}, ::Type{<:Box}) = Piece

struct Pallet{c} <: Counting value::Int end
scaler(::Type{Pallet{c}}) where c = c.value * scaler(typeof(c))
abbr(::Type{Pallet{c}}) where c = "pallet($(c.value)$(abbr(typeof(c))))"
Base.promote_rule(::Type{Pallet{a}}, ::Type{Pallet{b}}) where {a,b} = Piece
Base.promote_rule(::Type{<:Pallet}, ::Type{Piece}) = Piece
Base.promote_rule(::Type{<:Pallet}, ::Type{<:Box}) = Piece
Base.promote_rule(::Type{<:Pallet}, ::Type{<:Carton}) = Piece

"A volume unit parameterized by litres. e.g. 3Bucket(15) == 45litre"
struct Bucket{litres} end
Base.:*(n::Real, ::Type{Bucket{v}}) where v = (n * v)litre
