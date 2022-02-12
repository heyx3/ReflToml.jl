"Describes whether we're serializing (write) or deserializing (read)."
@enum ConverterModes CM_Write CM_Read

println("#TODO: Handle `missing` like `nothing`")
println("#TODO: Handle Sets")
println("#TODO: Handle rationals (as string or as Float64?)")
println("#TODO: Mark specific struct fields as being serialized/deserialized with their literal type, not their declared type, to handle massive unions like `Contiguous{T}`")
println("#TODO: support 'kwargs' thing from StructTypes")


"""
Serializer/deserializer settings.
This type is a functor, converting data into TOML-friendly primitives, or vice-versa.

The functor signature for serializing is `my_converter(x[, field_type_of_x])::TomlType`.
The functor signature for deserializing is `my_converter(x::TomlType, T)::T`.

The settings are:
 * `{ConverterMode}` : The type parameter determines whether to
        serialize (`CM_Write`) or deserialize (`CM_Read`).
 * `null_string` (default is `"null"`) : allows for reading and writing `nothing` values
        using a special string to represent them. Disable by setting the param to `nothing`.
        Note that null values can also be represented in some circumstances
            by simply excluding the field.
 * `write_null_fields` (default is false) : if true, then whenever a struct's field
        has the value `nothing` and thus could have been omitted from serialization,
        this writer will still explicitly write it as the `null_string` value.
        Enabling this feature and disabling `null_string` will throw an error.
 * `default_struct_type` (default is `Struct()`) : if a struct type hasn't been assigned a
        `StructTypes.StructType`, this one is given to it.
        Set to `nothing` to disable this feature, throwing an error when
        a struct doesn't have a `StructType`.
 * `enums_can_be_ints` (default is true) : if true, enums can be read from integer values.
        Otherwise, they have the default behavior for StructTypes, only parsable from strings.
 * `bools_can_be_strings` (default is true) : if true, bools can be read from strings like "T" or "false".
        Note that bools can always be serialized *as* strings, regardless of this setting.
 * `numbers_can_be_strings` (default is true) : if true, numbers can be parsed from strings,
        using `Base.parse(T, x)`. This also enables serialization of `NaN`, `Inf`, and `-Inf`,
        which normally aren't allowed in TOML.
"""
Base.@kwdef struct Converter{ConverterMode}
    null_string::Union{Nothing, AbstractString} = "null"
    write_null_fields::Bool = false
    default_struct_type::Union{Nothing, StructTypes.StructType} = StructTypes.UnorderedStruct()
    enums_can_be_ints::Bool = true
    bools_can_be_strings::Bool = true
    numbers_can_be_strings::Bool = true
end


#####################
#     Interface     #
#####################

"
For serialization of a `StructTypes.ArrayType`, this provides the serialized type of each element.
By default, for any `AbstractArray{T}, it'll return `T`,
    and for tuples, it'll return each element's expected type.
For other types, or if the element type if `Any`, it'll fall back to each value's exact type.

For deserialization, follow the `StructTypes.ArrayType` documentation.
"
array_el_type(a::AbstractVector, index::Int, element) = let et = eltype(a)
    if et == Any
        typeof(element)
    else
        et
    end
end
array_el_type(@specialize(t::Tuple), index::Int, element) = let et = typeof(t).parameters[index]
    if et == Any
        typeof(element)
    else
        et
    end
end
array_el_type(::AbstractArray{T, N}, index::Int, element) where {T, N} = error(
    "WOML currently doesn't support multi-dimensional arrays."
)

"
For serialization/deserialization of a `StructTypes.DictType`,
    this provides the type of its elements.
Default behavior is to try calling `eltype()`, expecting to get a `Pair`.
If that doesn't work, or the value-type is `Any`, then it will be replaced
    with each value's exact type.
"
dict_value_type(d, key, value) = let et = eltype(d)
    if (et <: Pair) && (et.types[2] != Any)
        return et.types[2]
    else
        return typeof(value)
    end
end


###################
#     Writing     #
###################

# If the type is already TOML-friendly, pass it through.
# Special exceptions are the special float values "NaN", "+Inf", and "-Inf".
(c::Converter{CM_Write})(x::TomlType) = begin
    if x isa AbstractFloat
        if isinf(x)
            if c.numbers_can_be_strings
                return (x > 0) ? "+Inf" : "-Inf"
            else
                error("Can't convert an infinite float to TOML, unless `numbers_can_be_strings` is enabled")
            end
        elseif isnan(x)
            if c.numbers_can_be_strings
                return "NaN"
            else
                error("Cant convert a NaN to TOML, unless `numbers_can_be_strings` is enabled")
            end
        else
            return x
        end
    else
        return x
    end
end

# In most cases, check the type of the value and dispatch based on StructType.
(c::Converter{CM_Write})(x) = c(x, typeof(x))
(c::Converter{CM_Write})(x, field_type::Union) = begin
    # Normally, we can just write the precise value of the field.
    # However, if one of the union types is an abstract type,
    #    we need to make sure it gets serialized with type information.
    for T in union_parse_order(field_type)
        if (StructTypes.StructType(T) isa StructTypes.AbstractType) && (x isa T)
            return c(x, T)
        end
    end

    return c(x, typeof(x))
end
(c::Converter{CM_Write})(x, field_type) = let field_stype = StructTypes.StructType(field_type)
    # If the field is an abstract type, then write it as an abstract value.
    if field_stype isa StructTypes.AbstractType
        c(field_stype, field_type, x)
    # Otherwise, write the field as its exact value.
    else
        c(StructTypes.StructType(x), field_type, x)
    end
end

(c::Converter{CM_Write})(::StructTypes.NullType, _, x) =
    if isnothing(c.null_string)
        error("There is no way to write a `nothing` value, as the `null_string` parameter wasn't set")
    else
        c.null_string
    end
(c::Converter{CM_Write})(::StructTypes.BoolType, _, x) = Bool(x)
(c::Converter{CM_Write})(::StructTypes.NumberType, field_type, x) = c(StructTypes.construct(StructTypes.numbertype(field_type), x))
(c::Converter{CM_Write})(::StructTypes.StringType, _, x) = string(x)
(c::Converter{CM_Write})(::StructTypes.ArrayType, field_type, x) = let vec = [ ]
    for (i::Int, el) in enumerate(x)
        push!(vec, c(el, array_el_type(x, i, el)))
    end
    vec
end
(c::Converter{CM_Write})(::StructTypes.DictType, _, x) = Dict(
    let K=typeof(k), V=typeof(v)
        out_key = string(c(StructTypes.StructType(K), K, k))
        out_val = c(StructTypes.StructType(V), V, v)
        out_key => out_val
    end for (k, v) in StructTypes.keyvaluepairs(x)
)
(c::Converter{CM_Write})(::StructTypes.NoStructType, field_type, x) =
    if isnothing(c.default_struct_type)
        error("No StructType defined for serializing ", typeof(x),
              " (or maybe its original field type)")
    else
        c(c.default_struct_type, field_type, x)
    end
(c::Converter{CM_Write})(::StructTypes.AbstractType, field_type, x) = begin
    subtype_data::NamedTuple = StructTypes.subtypes(field_type)
    subtype_strings::ConstVector{Symbol} = keys(subtype_data)
    subtype_types::ConstVector{Type} = values(subtype_data)

    subtype_idx::Union{Int, Nothing} = findfirst(t -> (x isa t), subtype_types)

    if isnothing(subtype_idx)
        error("Object type ", typeof(x),
              " is not listed as a `StructTypes.subtypes()` of ", field_type)
    else
        return Dict(
            "type" => subtype_strings[subtype_idx],
            "value" => c(x)
        )
    end
end
(c::Converter{CM_Write})(::StructTypes.CustomStruct, _, x) = c(StructTypes..lower(x))
(c::Converter{CM_Write})(::Union{StructTypes.Mutable, StructTypes.Struct}, _, x) = begin
    renamed_fields::NTuple = StructTypes.names(typeof(x))
    renamed_julia_fields::NTuple = map(t -> t[1], renamed_fields)
    renamed_output_fields::NTuple = map(t -> t[2], renamed_fields)

    fields_exclude = StructTypes.excludes(typeof(x))

    # There are a number of reasons why a field's specific value might not be written.
    fields_omitempty = StructTypes.omitempties(typeof(x))
    @inline hide_value(field_name::Symbol, field_value)::Bool = (
        (!c.write_null_fields && isnothing(field_value)) ||  # It's null, and nulls should be omitted
        (
            ((fields_omitempty === true) || (field_name in fields_omitempty)) && # Empties should be omitted, and...
            (hasmethod(iterate, tuple(typeof(field_value))) && isempty(field_value)) #...this value is empty.
        )
    )

    output = Dict{String, Any}()
    for field_name::Symbol in fieldnames(typeof(x))
        # Check if the field should be excluded.
        if !(field_name in fields_exclude)
            field_value = getproperty(x, field_name)
            # Check if the value shouldn't be written.
            if !hide_value(field_name, field_value)
                field_type = fieldtype(typeof(x), field_name)
                # Change the serialized name, if requested.
                new_name_idx = findfirst(x -> x==field_name, renamed_julia_fields)
                if !isnothing(new_name_idx)
                    field_name = renamed_output_fields[new_name_idx]
                end
                # Finally, output the field's value into the dictionary for serialization.
                output[string(field_name)] = c(field_value, field_type)
            end
        end
    end
    return output
end


###################
#     Reading     #
###################

# A catch-all for logic that doesn't fit into Julia's overload resolution order.
(c::Converter{CM_Read})(x::TIn, ::Type{TOut}) where {TIn, TOut} = begin
    # For Union types, try each element of the union individually.
    if TOut isa Union
        for T in union_parse_order(TOut)
            try_val = union_try_parse(c, x, T)
            if try_val isa Some
                @assert(try_val isa Some{<:T},
                        "WOML.union_try_parse() somehow turned a $T into a $(typeof(something(try_val))): $try_val")
                return something(try_val)
            end
        end
        error("Unable to parse a ", typeof(x),
              " into any of the following types: ", union_types(TOut),
              ". The precise value was: ", x)
    # If the output type is non-specific, try to aggressively parse the incoming data.
    elseif TOut == Any
        if x isa AbstractString
            if c.numbers_can_be_strings
                f = tryparse(Float64, x)
                if !isnothing(f)
                    return (isinteger(f) && (f <= typemax(Int)) && (f >= typemin(Int))) ?
                            Int(f) :
                            f
                end
            end

            if c.bools_can_be_strings
                if x == "true"
                    return true
                elseif x == "false"
                    return false
                end
                # Don't try the more expansive options like "f", or "True",
                #    as it feels a bit too eager.
            end

            if x == c.null_string
                return nothing
            end
        elseif x isa AbstractDict
            return c(x, Dict{Any, Any})
        elseif x isa AbstractVector
            return c(x, Vector{Any})
        end
        return x
    elseif TOut <: Union{Dict{Any, Any}, Dict{<:AbstractString, Any}}
        K = eltype(TOut).parameters[1]
        return Dict( (c(k, K) => c(v, Any)) for (k, v) in x )
    elseif TOut == Vector{Any}
        return map(e -> c(e, Any), x) 
    # If the data already satisfies the desired output type, pass it through.
    elseif TIn <: TOut
        return x
    # If the desired type is a Symbol, construct it.
    elseif TOut == Symbol
        return StructTypes.construct(Symbol, x)
    end

    # If nothing is special about this case, dispatch based on the desired type's `StructType`.
    return c(x, TOut, StructTypes.StructType(TOut))
end


# Support deserializing a number from a string.
(c::Converter{CM_Read})(x::AbstractString, T::Type{<:Real}) = begin
    if !c.numbers_can_be_strings
        # Was the data supposed to be a number, or was it generic?
        if T <: Real
            error("Can't parse a ", T, " from a ", typeof(x),
                  " unless `numbers_can_be_strings` is enabled")
        else
            return x
        end
    else
        # Make the parsed type concrete.
        if T in (Real, AbstractFloat)
            T = Float64
        elseif T in (Integer, Signed)
            T = Int64
        elseif T == Unsigned
            T = UInt64
        end
        return parse(T, x)
    end
end

# Support deserializing an enum from an int.
(c::Converter{CM_Read})(x::Number, T::Type{<:Enum{I}}) where {I<:Integer} = begin
    if c.enums_can_be_ints && isinteger(x)
        return T(convert(I, x))
    else
        # Pass through to the usual behavior (almost certainly throwing an error).
        return c(x, T, StructTypes.StructType(T))
    end
end

# Support deserializing a Bool from a string.
(c::Converter{CM_Read})(x::AbstractString, ::Type{Bool}) = begin
    x = lowercase(x)
    if c.bools_can_be_strings && (x in ("t", "true"))
        true
    elseif c.bools_can_be_strings && (x in ("f", "false"))
        false
    else
        # Fall through to the usual behavior (almost certainly throwing an error).
        c(x, Bool, StructTypes.StructType(Bool))
    end
end

# Support reading the null-string.
(c::Converter{CM_Read})(x::AbstractString, ::Type{Nothing}) = begin
    if !isnothing(c.null_string) && (x == c.null_string)
        nothing
    else
        # Fall through to the usual behavior (almost certainly throwing an error).
        c(x, Nothing, StructTypes.StructType(Nothing))
    end
end

(c::Converter{CM_Read})(x, T, ::StructTypes.NullType) = error(
    "Cannot convert a ", typeof(x), " to `Nothing`"
)
(c::Converter{CM_Read})(x, T, ::StructTypes.BoolType) = StructTypes.construct(T, x)
(c::Converter{CM_Read})(x, T, ::StructTypes.NumberType) = StructTypes.construct(T, convert(StructTypes.numbertype(T), x))
(c::Converter{CM_Read})(x, T, ::StructTypes.StringType) = StructTypes.construct(T, x)
(c::Converter{CM_Read})(x::AbstractVector, T::Type{<:Tuple}, ::StructTypes.ArrayType) = begin
    if length(x) != length(T.parameters)
        error("Can't parse a ", length(T.parameters), "-tuple from an array of ",
              length(x), " elements")
    else
        return tuple((c(e, T.parameters[i]) for (i, e) in enumerate(x))...)::T
    end
end
(c::Converter{CM_Read})(x::AbstractVector, T, ::StructTypes.ArrayType) = begin
    # Process each element to be as close to the desired type as possible.
    # Even if the element type is Any, we'd like to aggresively parse things
    #    like numbers/bools as the user configured.
    TElement = (Base.IteratorEltype(T) isa Base.HasEltype) ?
                   eltype(T) :
                   Any
    x = map(i -> c(i, TElement), x)

    # Call through to the constructor if necessary.
    return (x isa T) ? x : StructTypes.construct(T, x)
end
(c::Converter{CM_Read})(x::AbstractDict, T::Type, ::StructTypes.DictType) = StructTypes.construct(T, x)
(c::Converter{CM_Read})(x::AbstractDict, T::Type{<:AbstractDict{K, V}}, ::StructTypes.DictType) where {K, V} = begin
    # Convert the incoming key type to the desired type.
    # Do the same for values.
    return T(c(k, K) => c(v, V) for (k, v) in x)
end
(c::Converter{CM_Read})(x, T::Type, ::StructTypes.NoStructType) =
    if isnothing(c.default_struct_type)
        error("No StructType defined for deserializing ", typeof(x))
    else
        c(x, T, c.default_struct_type)
    end
(c::Converter{CM_Read})(x::AbstractDict, T::Type, ::StructTypes.AbstractType) = begin
    if (length(x) != 2) || !haskey(x, "type") || !haskey(x, "value")
        error("For the AbstractType '", T, "', expected a dictionary",
              " with the fields 'type' and 'value'")
    end
    subtype_string = x["type"]
    subtype_data = x["value"]

    subtypes::NamedTuple = StructTypes.subtypes(field_type)
    subtype_strings::ConstVector{Symbol} = keys(subtypes)
    subtype_types::ConstVector{Type} = values(subtypes)

    subtype_idx::Union{Int, Nothing} = findfirst(n -> (n == subtype_string), subtype_strings)
    if isnothing(subtype_idx)
        error("Serialized type '", subtype_string,
              "' is not a known type of ", T, ": ", subtype_strings)
    end

    return c(subtype_data, subtype_types[subtype_idx])
end
(c::Converter{CM_Read})(x, T::Type, ::StructTypes.CustomStruct) = StructTypes.construct(
    T,
    # If asked to "lower" the incoming value, then do so.
    let in_type = StructTypes.lowertype(T)
        if in_type == Any
            if x isa AbstractDict
                x
            else
                error("CustomStruct type '", T, "' didn't implement lowertype(),",
                      " so it expects a dictionary for deserialization, but it got a ",
                      typeof(x))
            end
        else
            c(x, in_type)
        end
    end
)
using InteractiveUtils
(c::Converter{CM_Read})(x::AbstractDict{<:AbstractString, <:Any}, T::Type, ::Union{StructTypes.Mutable, StructTypes.Struct}) = begin
    renamed_fields::NTuple = StructTypes.names(T)
    renamed_julia_fields::NTuple = map(t -> t[1], renamed_fields)
    renamed_toml_fields::NTuple = map(t -> t[2], renamed_fields)
    function get_field_name(toml_name::Symbol)
        i = findfirst(n -> (n == toml_name), renamed_toml_fields)
        return isnothing(i) ? toml_name : renamed_julia_fields[i]
    end

    fields_exclude = StructTypes.excludes(T)

    fields = Dict{Symbol, Any}()
    for toml_name in keys(x)
        field_name = get_field_name(Symbol(toml_name))
        if !in(field_name, fields_exclude)
            fields[field_name] = c(x[toml_name], fieldtype(T, field_name))
        end
    end

    fields_to_set = setdiff(fieldnames(T), fields_exclude)
    field_values = Iterators.map(f -> get(fields, f, nothing), fields_to_set)
    return T(field_values...)
end