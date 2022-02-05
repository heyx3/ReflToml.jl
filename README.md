# WOML

A wrapper for Julia's built-in TOML package, that can support more data types (`nothing`, `NaN`, `Inf`), and reflection-based serialization using `StructTypes`.

This package is built directly off Julia's TOML module; it essentially just converts data to and from a TOML-friendly form.

The goals of this project are:
  * Make the use of TOML convenient for data serialization/deserialization. JSON has too many annoying problems for my work (most of all, the lack of multiline strings), and Julia doesn't have many other well-supported alternatives, especially which use reflection.
  * Prioritize convenience and simplicity over performance. I'm not a serialization expert, and I have other things I want to work on, so don't expect this package to be tightly-optimized. In fact, it's quite *un*-optimized. For example, there's no specialization or @generated implementation for serializing different structs. On the bright side, compilation times will be small.

## Data types

The following types of data are natively serializable by TOML: `AbstractString`, `Real` (which includes `Bool`), `Dates.TimeType`, and nested arrays/dictionaries of the above. WOML will attempt to turn any other types into one of these. HOWEVER, the following caveats apply:

* The special values `NaN`, `Inf`, and `-Inf` (as well as their 32-bit and 16-bit versions) are not supported in TOML, but they are in WOML by writing them as the string `"NaN"`, `"+Inf"`, or `"-Inf"` respectively.
* Null-like types (i.e. `nothing` and `missing`) are not supported in TOML, but they are in WOML by writing them as a special string, like `"null"`.
* The root object in TOML must be a dictionary (as opposed to JSON, where you can define an array or even a primitive value), but WOML adds special behavior to allow you to do this, by wrapping the data into a dictionary.
* You may not mix array elements which are plain data, and elements which are themselves dictionaries. For example, you can serialize `[ 4, "5", [ :six, 7.0 ] ]`, and `[ Dict(:a=>1), Dict(:b=>2) ]`, but not `[ 4, Dict(:a=>5) ]`. This is because it has 2 entirely different syntaxes for simple arrays vs. dictionary arrays. This error is not actually caught when writing such a problematic array; TOML.jl will simply emit invalid TOML.
* All dictionary keys are converted to strings, so their type data will get lost. For example, the keys `Inf16`, `Inf32`, and `"+Inf"` will all step on each other when converted into TOML.

## Converter

The core of this package is `WOML.Converter`, a functor type that can convert between arbitrary Julia data, and simpler data which TOML.jl can serialize/deserialize. For example, structs can get turned into dictionaries, and symbols into strings.

The converter is created for serialization with `WOML.Converter{CM_Write}(params...)`, or for deserialization with `WOML.Converter{CM_Read}(params...)`.

For example:

```
writer = WOML.Converter{CM_Write}()
reader = WOML.Converter{CM_Read}()

x = [ 4, 5, :six, "seven", [ 8.0, :nine ] ]
y = writer(x)
@test y == [ 4, 5, "six", "seven", [ 8.0, "nine" ] ]

z = reader(y[3], Symbol)
@test z == :six

@enum EE Aa Bb Cc
@test writer(Aa) == "Aa"
@test reader("Bb") == Bb

struct S
  i::Int
end
@test reader(Dict(:i => 30), S) == S(30)
@test reader(Dict("i" => -66), S) == S(-66)
```

There are a number of settings for the converter.
By default, the settings provide quite flexible serialization opportunities.
If you want something closer to normal StructTypes behavior or the real TOML standard,
    you can disable settings as appropriate.

* `null_string::Union{Nothing, AbstractString}`
  * Defaults to `"null"`
  * If set, then `nothing` and `missing` values can be serialized as a special string constant.
  * E.x. `writer.null_string = "MY_NULL"; @test writer(nothing) == "MY_NULL"`. The TOML standard doesn't allow serialization of nulls directly.
* `write_null_fields::Bool`
  * Defaults to `false`
  * *Only used for writing, not reading*
  * If false, then `nothing` or `missing` values are omitted from serialization. If true, then they are written using `null_string` (throwing an error if `null_string` is disabled).
* `default_struct_type::Union{Nothing, StructTypes.StructType}`
  * Defaults to `UnorderedStruct()`
  * If a type `T` doesn't implement `StructTypes.StructType(::Type{T})`, then it's assumed to have this struct-type. Set to `nothing` to force types to explicitly define their type.
* `enums_can_be_ints::Bool`
  * Defaults to `true`
  * *Only used for reading, not writing*
  * Normally, enums are `StructType.StringType()`, meaning they are deserialized from their string representation. However, WOML can also parse them from their integer value.
* `bools_can_be_strings::Bool`
  * Defaults to `true`
  * *Only used for reading, not writing*
  * If true, booleans can be parsed from simple strings like `"T"` or `"false"`. Otherwise, they must come from a proper TOML boolean.
  * E.x. `reader.bools_from_strings=true; @test reader("f", Bool) == false`
* `numbers_can_be_strings::Bool`
  * Defaults to `true`
  * *Only used for reading, not writing*
  * If true, numbers can be parsed from strings, using `Base.parse(T, x)`.
  Otherwise, they must come from a proper TOML number.
  * For special values `NaN`, `+Inf`, and `-Inf`, see the param `special_floats_as_strings`.
* `special_floats_as_strings::Bool`
  * Defaults to `true`
  * If true, the special float values `NaN`, `Inf`, and `-Inf` can be serialized/deserialized as the strings `"NaN"`, `"+Inf"`, and `"-Inf"`. Note that the serialized string is the same regardless of the float's type, e.x. `Inf32` becomes `"+Inf"`. This means you couldn't serialize a dictionary with two keys `Inf32` and `Inf64`. Fortunately, this is a pretty rare edge-case.

## License

Refer to the LICENSE file for the most up-to-date info. But as of the time of this writing, this package is released under the MIT license.