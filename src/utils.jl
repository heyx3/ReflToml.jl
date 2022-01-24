"Gets a tuple of the types in a Union."
@inline union_types(T) = (T, )
@inline union_types(u::Union) = (u.a, union_types(u.b)...)

"A value that is serializable/deserializable by the Julia `TOML` package."
const TomlType = TOML.Internals.Printer.TOMLValue