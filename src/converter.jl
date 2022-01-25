"Describes whether we're serializing (write) or deserializing (read)."
@enum ConverterModes CM_Write CM_Read

println("#TODO: Handle `missing` like `nothing`")
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
 * `bools_from_strings` (default is true) : if true, bools can be read from strings like "T" or "false".
        Note that bools can always be serialized *as* strings, regardless of this setting.
 * `special_floats_as_strings` (default is true) : if true, the special float values
        +Inf, -Inf, and NaN can be serialized/deserialized from strings.
"""
Base.@kwdef struct Converter{ConverterMode}
    null_string::Union{Nothing, AbstractString} = "null"
    write_null_fields::Bool = false
    default_struct_type::Union{Nothing, StructTypes.StructType} = StructTypes.UnorderedStruct()
    enums_can_be_ints::Bool = true
    bools_can_be_strings::Bool = true
    special_floats_as_strings::Bool = true
end


#####################
#     Interface     #
#####################

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

println("#TODO: Double-check whether TOML can serialize enums as ints already")
println("#TODO: writing unions")

# In most cases, check the type of the value and dispatch based on StructType.
(c::Converter{CM_Write})(x) = c(x, typeof(x))
(c::Converter{CM_Write})(x, field_type) = let field_stype = StructTypes.StructType(field_type)
    # If the field is an abstract type, then write it as an abstract value.
    if field_stype isa StructTypes.AbstractType
        c(field_stype, field_type, x)
    # Otherwise, write the field as its exact value.
    else
        c(StructTypes.StructType(x), field_type, x)
    end

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
                output[stringn(field_name)] = c(field_value, field_type)
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

println("#TODO: Read unions")

# In all other cases, dispatch based on the desired type's `StructType`.
(c::Converter{CM_Read})(x, T::Type) = c(x, T, StructTypes.StructType(T))

(c::Converter{CM_Read})(x, T, ::StructTypes.NullType) = error(
    "Cannot convert a ", typeof(x), " to `Nothing`"
)
(c::Converter{CM_Read})(x, T, ::StructTypes.BoolType) = StructTypes.construct(T, x)
(c::Converter{CM_Read})(x, T, ::StructTypes.NumberType) = StructTypes.construct(T, convert(StructTypes.numbertype(), x))
(c::Converter{CM_Read})(x, T, ::StructTypes.StringType) = StructTypes.construct(T, x)
println("#TODO: Finish the below")
(c::Converter{CM_Read})(x::AbstractVector, ::Type{TField}, ::StructTypes.ArrayType) where {TField} = begin
    converted_elements = map(el -> c(el, T), x)
    return StructTypes.construct(TOutArray, converted_elements)
end
(c::Converter{CM_Read})(x::AbstractDict, TOutDict, ::StructTypes.DictType) = StructTypes.construct(
    TOutDict,
    Dict((c()))
)