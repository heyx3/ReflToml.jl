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
For deserialization, follow the `StructTypes.ArrayType` documentation.

The first three parameters are the collection, index, and specific element being serialized.
The last parameter is the desired output type; in many cases it's equal to `typeof(data)`,
    but the user might want to, for example, serialize a list of enums as a `Vector{Int}`.
This type should be prioritized over the data's actual type.

By default, this will usually return `eltype(desired_type)`,
    or for tuples, the desired type of the specific element.
If that type is `Any`, then it will try something similar with the collection's actual type.
If that type is also `Any`, then it will return the exact type of the element.
"
function array_el_type(data, index::Int, element, desired_type)
    if (desired_type <: Tuple) && (desired_type.parameters[index] != Any)
        return desired_type.parameters[index]
    elseif eltype(desired_type) != Any
        return eltype(desired_type)
    elseif (data isa Tuple) && (typeof(data).parameters[index] != Any)
        return typeof(data).parameters[index]
    elseif eltype(data) != Any
        return eltype(data)
    else
        return typeof(element)
    end
end

"
For serialization of a `StructTypes.DictType`,
    this provides the type of its elements.

The first three parameters are the dictionary, key, and specific value being serialized.
The last parameter is the desired output type;
    in many cases it's equal to `typeof(data)`, but the user might want to, for example,
    serialize a dictionary of enum values as a dictionary of Int values.
The desired type should be prioritized over the data's actual type.

Default behavior is to check `eltype(desired_type)`, expecting to get some `Pair{K, V}`,
    and return that `V`.
If the element type isn't a `Pair`, or the element type is `Any`,
    then the actual dictionary's value-type is used instead.
If that also fails, then the value's exact type will be returned.
"
function dict_value_type(dict, key, value, desired_type)
    if (eltype(desired_type) <: Pair) && (eltype(desired_type).types[2] != Any)
        return eltype(desired_type).types[2]
    elseif (eltype(dict) <: Pair) && (eltype(dict).types[2] != Any)
        return eltype(dict).types[2]
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

# Get the StructType of the data and dispatch based on that.
# If it has a partcular expected type (e.x. its declared type in a struct),
#    look out for some special cases, like abstract types.
(c::Converter{CM_Write})(x) = c(x, StructTypes.StructType(x), typeof(x))
(c::Converter{CM_Write})(x, expected_type) = begin
    # Get the type of X according to its declaration
    #    (meaning the 'T' in 'x::T', or in 'Vector{T}', or in 'Dict{T, V}', etc).
    # Note that it could be a union of types.
    types::Tuple = union_types(expected_type)
    x_type_idx = findfirst(t -> x isa t, types)

    # If the input data isn't any of the declared types
    #    (e.x. it's an enum and the declared type is Int),
    #    try writing it as each type.
    if isnothing(x_type_idx)
        for T in union_parse_order(expected_type)
            try
                return c(x, StructTypes.StructType(T), expected_type)
            catch e
                # Left here in case debugging is needed.
                #@warn "Parse failed: $T" exception=(e, catch_backtrace())
            end
        end
        error("Couldn't write data of type ", typeof(x), " as a ", expected_type)
    else
        T = types[x_type_idx]
        return c(x, StructTypes.StructType(T), expected_type)
    end
end

(c::Converter{CM_Write})(x, ::StructTypes.NullType, _) =
    if isnothing(c.null_string)
        error("There is no way to write a `nothing` value, as the `null_string` parameter wasn't set")
    else
        c.null_string
    end
(c::Converter{CM_Write})(x, ::StructTypes.BoolType, _) = Bool(x)
(c::Converter{CM_Write})(x, ::StructTypes.NumberType, _) = c(StructTypes.construct(StructTypes.numbertype(typeof(x)), x))
# Special behavioir for enums: they should be written as ints, not Float64.
(c::Converter{CM_Write})(x::Enum{I}, ::StructTypes.NumberType, _) where {I} = c(StructTypes.construct(I, x))
(c::Converter{CM_Write})(x, ::StructTypes.StringType, _) = string(x)
(c::Converter{CM_Write})(x, ::StructTypes.ArrayType, desired_type) = let vec = [ ]
    for (i::Int, el) in enumerate(x)
        push!(vec, c(el, array_el_type(x, i, el, desired_type)))
    end
    vec
end
(c::Converter{CM_Write})(x, ::StructTypes.DictType, desired_type) = Dict(
    (string(c(k)) => c(v, dict_value_type(x, k, v, desired_type)))
      for (k, v) in StructTypes.keyvaluepairs(x)
)
(c::Converter{CM_Write})(x, ::StructTypes.NoStructType, _) =
    if isnothing(c.default_struct_type)
        error("No StructType defined for serializing ", typeof(x),
              " (or maybe its original field type)")
    else
        c(x, c.default_struct_type)
    end
(c::Converter{CM_Write})(x, ::StructTypes.AbstractType, expected_type) = begin
    subtype_data::NamedTuple = StructTypes.subtypes(expected_type)
    subtype_strings::ConstVector{Symbol} = keys(subtype_data)
    subtype_types::ConstVector{Type} = values(subtype_data)

    subtype_idx::Union{Int, Nothing} = findfirst(t -> (x isa t), subtype_types)

    if isnothing(subtype_idx)
        error("Object type ", typeof(x),
              " is not listed as a `StructTypes.subtypes()` of ", expected_type)
    else
        return Dict(
            "type" => subtype_strings[subtype_idx],
            "value" => c(x)
        )
    end
end
(c::Converter{CM_Write})(x, ::StructTypes.CustomStruct, _) = c(StructTypes.lower(x))
(c::Converter{CM_Write})(x, ::Union{StructTypes.Mutable, StructTypes.Struct}, _) = begin
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