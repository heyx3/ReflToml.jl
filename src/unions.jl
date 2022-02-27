"Gets a tuple of the types in a Union."
@inline union_types(T) = (T, )
@inline function union_types(u::Union)
    # Supposedly, 'u.a' is the first type and 'u.b' is a Union of the other types.
    # However, sometimes a is the Union, and b is the type.
    # So I pass both sides through 'union_types()' to be sure.
    (union_types(u.a)..., union_types(u.b)...)
end


"
When deserializing a union of types, this determines the order that the types are tried in.
Lower-priority values are tried first.
"
union_ordering(T::Type)::Float64 =
    # If this is an AbstractType, give it a specific priority.
    # Otherwise, unknown/user-made types are given the lowest priority.
    (StructTypes.StructType(T) isa StructTypes.AbstractType) ?
        union_ordering(Val(:abstract)) :
        +Inf
#
union_ordering(::Type{<:Enum}) = 0.0
union_ordering(::Type{<:Integer}) = 1.0
union_ordering(::Type{<:AbstractFloat}) = 2.0
union_ordering(::Type{<:Dates.TimeType}) = 3.0
union_ordering(::Type{Symbol}) = 4.0
union_ordering(::Type{<:AbstractString}) = 5.0
union_ordering(::Val{:abstract}) = 6.0
#TODO: Make the dictionary and vector priorities dynamic based on the priority of their input types
union_ordering(::Type{<:AbstractDict}) = 7.0
union_ordering(::Type{<:AbstractVector}) = 8.0
const MAX_BUILTIN_UNION_PRIORITY = union_ordering(Vector)


"Organizes the types in a union by their `union_ordering()`."
@inline union_parse_order(U::Union) = sort(collect(union_types(U)), by=union_ordering)
union_parse_order(T::Type) = (T, )
#NOTE: originally I used TupleTools.sort(), but it breaks
#    for unions containing DataTypes and UnionAll's, because
#    those are two different types of things, so the tuple isn't homogenous.

"
Tries parsing a type from a union, returning `Nothing` if it failed
    or `Some{T}` if it succeeded.
By default, simply performs the conversion and catches any exception that happens.
"
function union_try_parse(converter, toml_data, union_type)::Union{Nothing, Some}
    try
        return Some(converter(toml_data, union_type))
    catch e
        return nothing
    end
end
# Optimize the try-parse for simple types.
union_try_parse(_, x::T, ::Type{T2}) where {T2, T<:T2} = Some(x)
union_try_parse(_, f::Real, N::Type{<:Real}) =
    (hasmethod(N, tuple(typeof(f))) && (f >= typemin(N)) && (f <= typemax(N))) ?
        Some(N(f)) :
        nothing
union_try_parse(_, f::Real, I::Type{<:Integer}) =
    (isinteger(f) && (f >= typemin(I)) && (f <= typemax(I)) && hasmethod(I, tuple(typeof(f)))) ?
        Some(I(f)) :
        nothing
union_try_parse(_, s::AbstractString, N::Type{<:Real}) = let parsed = tryparse(N, s)
    return isnothing(parsed) ? nothing : Some(parsed)
end
union_try_parse(_, s::AbstractString, I::Type{<:Integer}) = let parsed = tryparse(Float64, s)
    return (!isnothing(parsed) && isinteger(parsed)) ? Some(I(parsed)) : nothing
end
union_try_parse(_, x, ::Type{Symbol}) =
    hasmethod(Symbol, tuple(typeof(x))) ?
        Some(Symbol(x)) :
        nothing
union_try_parse(c, x::AbstractString, ::Type{Nothing}) =
    (x == c.null_string) ?
        Some(nothing) :
        nothing