"Gets a tuple of the types in a Union."
@inline union_types(T) = (T, )
@inline union_types(u::Union) = (u.a, union_types(u.b)...)


"A plain value type that is serializable/deserializable by the Julia `TOML` package."
const TomlTypePlain = Union{Real, AbstractString, Dates.TimeType}

"A simple collection that is serializable/deserializable by the Julia `TOML` package."
const TomlType = Union{TomlTypePlain,
                       AbstractVector{<:TomlTypePlain},
                       AbstractDict{<:AbstractString, <:TomlTypePlain}
                      }

#NOTE: originally this was an alias for TOML.Internals.Printer.TOMLValue,
#    but that causes any kind of dictionary/vector to get caught,
#    even if TOML can't actually serialize it.
# Plus we probably don't want to depend on TOML internals like that.