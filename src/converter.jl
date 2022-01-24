"Describes whether we're serializing (write) or deserializing (read)."
@enum ConverterModes CM_Write CM_Read

"""
Serializer/deserializer settings.
This type is a functor, acting as the conversion function required by the `TOML` package
    to make a value serializable.
The functor signature for serializing is `my_converter(x[, field_type_of_x])::TomlType`.
The functor signature for deserializingn is `my_converter(x::TomlType, field_type::Type)`

The settings are:
 * `{ConverterMode}` : The type parameter.
        Pass `CM_Read` for deserialiing, and `CM_Write` for serializing.
 * `null_string` (default is disabled) : a string that corresponds to `nothing`
        if the parsed type can be `nothing`.
 * `write_nulls` (default is false) : if true, null values will be explicitly written
       using the above `null_string` value (throwing an error if it's not set).
 * `default_struct_type` (default is `Struct()`) : if a struct type hasn't been assigned a
        `StructTypes.StructType`, this one is given to it.
        Set to `nothing` to disable this feature, throwing an error when
        a struct doesn't have a `StructType`.
 * `enums_from_ints` (default is true) : if true, enums can be read from integer values.
        Otherwise, they have the default behavior for StructTypes, only parsable from strings.
 * `special_floats_as_strings` (default is true) : if true, the special float values
        Inf, Inf, and NaN can be serialized/deserialized from strings.
"""
Base.@kwdef struct Converter{ConverterMode}
    null_string::Union{Nothing, AbstractString} = nothing
    write_nulls::Bool = false
    default_struct_type::Union{Nothing, StructTypes.StructType} = StructTypes.UnorderedStruct()
    enums_from_ints::Bool = true
    special_floats_as_strings::Bool = true
end


"
If your type is a `StructTypes.ArrayType`, you can override how each element's type is serialized.
For example, you can return something that's a `StructTypes.AbstractType`
    to make it serialize type data with each element.
By default, it'll call `eltype()`, and if that element type is `Any`,
    then it'll use each value's exact type instead.
"
array_el_type(a, index::Int, element) = let et = eltype(a)
    if et == Any
        typeof(element)
    else
        et
    end
end
array_el_type(::AbstractArray{T, N}, _...) where {T, N} = error(
    "WOML currently doesn't support multi-dimensional arrays."
)

"
Gets the desired value type for a `StructTypes.DictType` during deserialization.
Default behavior is to try calling `eltype()`, expecting to get `<:Pair`,
    then using that pair's value-type.
If that doesn't work, or the value-type is `Any`, then it will be replaced
    with each value's exact type.
"
dict_value_type(d, key, value) = let et = eltype(d)
    if (et <: Pair) && (et.types[2] != Any)
        return et.types[2]
    else
        return typeof(value)
    end
#


###################
#     Writing     #
###################

# If the type is already TOML-friendly, pass it through.
(c::Converter{CM_Write})(x::TomlType) = x

# Otherwise, check its type and dispatch based on its StructType.
(c::Converter{CM_Write})(x) = c(x, typeof(x))
(c::Converter{CM_Write})(x, field_type) = let field_stype = StructTypes.StructType(field_type)
    # If the field is an abstract type, then write it as an abstract value.
    if field_stype isa StructTypes.AbstractType
        c(field_stype, field_type, x)
    # Otherwise, write the field as its exact value.
    else
        c(StructTypes.StructType(x), field_type, x)
    end
println("#TODO: writing unions")

(c::Converter{CM_Write})(::StructTypes.NullType, _, x) =
    if isnothing(c.null_string)
        error("There is no way to write a `nothing` value, as the `null_string` parameter wasn't set")
    else
        c.null_string
    end
(c::Converter{CM_Write})(::StructTypes.BoolType, _, x) = Bool(x)
(c::Converter{CM_Write})(::StructTypes.NumberType, _, x) = StructTypes.construct(StructTypes.numbertype(), x)
(c::Converter{CM_Write})(::StructTypes.NumberType, _, x::AbstractFloat) = begin
    x2 = StructTypes.construct(StructTypes.numbertype(), x)
    # If the float value is infinity or NaN, serialize it as a string.
    if c.special_floats_as_strings
        if isinf(x2)
            if x2 < 0
                return "-Inf"
            else
                return "+Inf"
            end
        elseif isnan(x2)
            return "NaN"
        else
            return x2
        end
    elseif isinf(x2) || isnan(x2)
        error("Can't write the value ", typeof(x2), "(", x2, ") in standard TOML,",
              " you have to enable the `special_floats_as_strings` setting")
    else
        return x2
    end
end
(c::Converter{CM_Write})(::StructTypes.StringType, _, x) = string(x)
(c::Converter{CM_Write})(::StructTypes.ArrayType, field_type, x) = let vec = [ ]
    for (i::Int, el) in enumerate(x)
        push!(vec, c(el, array_el_type(x, i, el)))
    end
end
(c::Converter{CM_Write})(::StructTypes.DictType, _, x) = Dict(
    (c(k, String, StrucTypes.StringType()) => c(v, dict_value_type(x, k, value)))
       for (k, v) in StructTypes.keyvaluepairs(x)
)
(c::Converter{CM_Write})(::StructType.NoStructType, _, x) =
    if isnothing(c.default_struct_type)
        error("No StructType defined for serializing ", typeof(x),
              " (or maybe its original field type)")
    else
        c(c.default_struct_type, x)
    end
(c::Converter{CM_Write})(::StructTypes.AbstractType, field_type, x) = begin
    subtypes_names::NamedTuple = StructTypes.subtypes(field_type)
    subtypes::NTuple = collect(zip(keys(subtypes_names), values(subtypes_names)))
    subtype_idx::Union{Int, Nothing} = findfirst(s -> (x isa s[2]), subtypes)

    if isnothing(subtype_idx)
        error("Object type ", typeof(x),
              " is not listed as a `StructTypes.subtypes()` of ", field_type)
    else
        return Dict(
            "type" => subtypes[subtype_idx][1],
            "value" => c(x)
        )
    end
end
(c::Converter{CM_Write})(::StructTypes.CustomStruct, _, x) = c(StructTypes.lower(x))
(c::Converter{CM_Write})(::Union{StructTypes.Mutable, StructTypes.Struct}, _, x) = begin
    renamed_fields::NTuple = StructTypes.names(typeof(x))
    renamed_julia_fields::NTuple = map(t -> t[1], renamed_fields)
    renamed_output_fields::NTuple = map(t -> t[2], renamed_fields)

    fields_exclude = StructTypes.excludes(typeof(x))

    fields_omitempty = StructTypes.omitempties(typeof(x))
    @inline should_omit(field_name::Symbol, field_value) =
        ((fields_omitempty === true) || (field_name in fields_omitempty)) &&
        (hasmethod(iterate, tuple(typeof(field_value))) && isempty(field_value))

    output = Dict{String, Any}()
    for field_name::Symbol in fieldnames(typeof(x))
        # Check the exclusion list.
        if !(field_name in fields_exclude)
            field_value = getproperty(x, field_name)
            # Check the "omit empty collections" list.
            if !should_omit(field_name, field_value)
                field_type = fieldtype(typeof(x), field_name)
                # Change the serialized name, if requested.
                new_name_idx = findfirst(x -> x==field_name, renamed_julia_fields)
                if !isnothing(new_name_idx)
                    field_name = renamed_output_fields[new_name_idx]
                end
                # Finally, output the field's value into the dictionary for serialization.
                output[field_name] = c(field_value, field_type)
            end
        end
    end
    return output
end


###################
#     Reading     #
###################

# If deserialized data already satisfies the desired output type, pass it through.
(c::Converter{CM_Read})(x::T, ::Type{TParent}) where {TParent<:TomlType, T<:TParent} = x

# In all other cases, dispatch based on the desired type's `StructType`.
(c::Converter{CM_Read})(x, T::Type) = c(x, T, StructTypes.StructType(T))

(c::Converter{CM_Read})(x::AbstractString, T, ::StructTypes.NullType) =
    if !isnothing(c.null_string)
        if x == c.null_string
            nothing
        else
            error("Expected a null value (\"", c.null_string, "\"), got \"", x, "\"")
        end
    else
        error("Can't convert from a string value to `nothing`",
              " (did you mean to set the `null_string` setting?): \"", x, "\"")
    end
(c::Converter{CM_Read})(x, T, ::StructTypes.NullType) = error(
    "Cannot convert a ", typeof(x), " to `Nothing`"
)
(c::Converter{CM_Read})(x::AbstractString, T, ::StructTypes.BoolType) = c(
    if lowercase(x) in ("t", "true")
        true
    elseif lowercase(x) in ("f", "false")
        false
    else
        error("Cannot convert string to bool: \"", x, "\"")
    end,
    T, StructTypes.BoolType()
)
(c::Converter{CM_Read})(x::Integer, T, ::StructTypes.BoolType) = StructTypes.construct(T, x)
(c::Converter{CM_Read})(x::Number, T, ::StructTypes.NumberType) = StructTypes.construct(T, convert(StructTypes.numbertype(), x))
(c::Converter{CM_Read})(x::Number, T::Type{<:Enum{I}}, ::StructTypes.StringType) where {I<:Integer} =
    if isinteger(x)
        if c.enums_from_ints
            error("Received a number value ", x, " to be deserialized as the enum ", T,
                  " but `enums_from_ints` is disabled")
        else
            return T(convert(I, x))
        end
    else
        error("Received a non-integer value ", x, " to be deserialized as the enum ", T)
    end
(c::Converter{CM_Read})(x, T, ::StructTypes.StringType) = StructTypes.construct(T, x)

println("#TODO: Finish from here")
(c::Converter{CM_Read})(x::AbstractVector, TOutArray::Type{<:Union{AbstractSet{T}, AbstractVector{T}}}, ::StructTypes.ArrayType) where {T} = begin
    converted_elements = map(el -> c(el, T), x)
    return StructTypes.construct(TOutArray, converted_elements)
end
(c::Converter{CM_Read})(x::AbstractDict, TOutDict, ::StructTypes.DictType) = StructTypes.construct(
    TOutDict,
    Dict((c()))
)


println("#TODO: support 'kwargs' thing from StructTypes")