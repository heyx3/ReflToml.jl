using Test, TOML, StructTypes
using WOML


@testset "Converter Write basic data" begin
    c = WOML.Converter{CM_Write}(
        null_string = "T_NULL",
        numbers_can_be_strings = true
    )

    @test c(5) === 5
    @test c(3.12345) === 3.12345

    s = "hello"
    @test c(s) === s
    @test c(:hello) == s

    @test c([ 4, 5, 6 ]) == [ 4, 5, 6 ]
    @test c(Dict(3=>"hi", 5=>:hey_there)) ==
          Dict("3"=>"hi", "5"=>"hey_there")

    @test c([ 3, 4, [ 1, 2, 3 ], 5, 6, 7 ]) ==
            [ 3, 4, [ 1, 2, 3 ], 5, 6, 7 ]

    @test c(nothing) == "T_NULL"
    @test c(NaN) == "NaN"
    @test c(NaN32) == "NaN"
    @test c(+Inf) == "+Inf"
    @test c(-Inf) == "-Inf"
    @test c(+Inf16) == "+Inf"
    @test c(-Inf16) == "-Inf"
    @test c([ nothing, Inf32, NaN64, -20 ]) ==
          [ "T_NULL", "+Inf", "NaN", -20 ]
    @test c(Dict(
                nothing => 5,
                5 => nothing,

                +Inf32 => 70,
                70 => +Inf32,

                NaN => "hi",
                "hi" => NaN
          )) ==
          Dict(
              "T_NULL" => 5,
              "5" => "T_NULL",

              "+Inf" => 70,
              "70" => "+Inf",

              "NaN" => "hi",
              "hi" => "NaN"
          )

    @test c(Dict(
                7.5 => :abcd,
                :abc => [ "a", "b", "c" ],
                "def" => [
                    [ 1, 2, 3 ],
                    [ 4, 5, 6 ],
                    [ 7, 8, 9 ]
                ],
                "ghi" => [
                    Dict(:j=>1),
                    Dict(:j=>2),
                    Dict(:j=>3)
                ]
            )) ==
            Dict(
                "7.5"=>"abcd",
                "abc" => [ "a", "b", "c" ],
                "def" => [
                    [ 1, 2, 3 ],
                    [ 4, 5, 6 ],
                    [ 7, 8, 9 ]
                ],
                "ghi" => [
                    Dict("j"=>1),
                    Dict("j"=>2),
                    Dict("j"=>3)
                ]
            )
end
@testset "Converter Read basic data" begin
    c = WOML.Converter{CM_Read}(
        null_string = "T_NULL",
        numbers_can_be_strings = true
    )

    @test c(5, Int) === 5
    @test c(5, Float32) === Float32(5)
    @test c(true, Bool) === true

    s = "hello"
    @test c(s, String) === s
    @test c(s, AbstractString) === s
    @test c("hello", String) == s
    @test c("hello", AbstractString) == s

    @test c([ 4, 5, 6 ], Vector{Int}) == [ 4, 5, 6 ]
    @test c([ 4, 5, 6 ], Vector{Integer}) == [ 4, 5, 6 ]
    @test c([ 4, 5, 6 ], Vector{Real}) == [ 4, 5, 6 ]

    @test c(Dict("3"=>"hi", "5"=>"hey_there"), Dict{Int, Symbol}) ==
          Dict(3=>:hi, 5=>:hey_there)

    @test c([ 3, 4, [ 1, 2, 3 ], 5, 6, 7 ], Vector{Any}) ==
            [ 3, 4, [ 1, 2, 3 ], 5, 6, 7 ]

    @test c("T_NULL", Nothing) === nothing
    @test c("NaN", Float32) === NaN32
    @test c("-Inf", Float64) === -Inf64
    @test c("+Inf", Float16) === +Inf16
    @test all(t -> t[1] === t[2],
              zip(c([ "T_NULL", "+Inf", "NaN", -20 ], Vector{Any}),
                  [ nothing, Inf64, NaN64, -20 ]))

    real_d = c(Dict(
        "T_NULL" => 5,
        "5" => "T_NULL",

        "+Inf" => 70,
        "70" => "+Inf",

        "NaN" => 30,
        30 => "NaN"
    ), Dict{Real, Real})
    expected_d = Dict(
        nothing => 5,
        5 => nothing,

        +Inf32 => 70,
        70 => +Inf32,

        NaN => "hi",
        "hi" => NaN
    )
    @test (length(real_d) == length(expected_d)) &&
          (all(pair -> pair in real_d, expected_d))

    @test c(Dict(
                "7.5"=>"abcd",
                "abc" => [ "a", "b", "c" ],
                "def" => [
                    [ 1, 2, 3 ],
                    [ 4, 5, 6 ],
                    [ 7, 8, 9 ]
                ],
                "ghi" => [
                    Dict("j"=>1),
                    Dict("j"=>2),
                    Dict("j"=>3)
                ]
            ), Dict{Any, Any}) ==
          Dict(
              "7.5"=>"abcd",
              "abc" => [ "a", "b", "c" ],
              "def" => [
                  [ 1, 2, 3 ],
                  [ 4, 5, 6 ],
                  [ 7, 8, 9 ]
              ],
              "ghi" => [
                  Dict("j"=>1),
                  Dict("j"=>2),
                  Dict("j"=>3)
              ]
          )
end

@testset "Converter Write struct data" begin
    c = WOML.Converter{CM_Write}(
        default_struct_type = StructTypes.Struct(),
        write_null_fields = false
    )

    struct S
        x
    end
    StructTypes.StructType(::Type{S}) = StructTypes.Struct()

    @test c(S(5)) == Dict("x"=>5)
    @test c(S(:a)) == Dict("x"=>"a")
    @test c(S("1\"2\"3")) == Dict("x"=>"1\"2\"3")

    @test c(S([ 1, 2, 3 ])) == Dict("x" => [ 1, 2, 3 ])
    @test c(S(Dict(:y => NaN))) == Dict("x" => Dict("y" => "NaN"))

    @test c(S(S(-1.5))) == Dict("x" => Dict("x" => -1.5))


    struct SS
        a::Real
        b::S
        c::Union{Nothing, Vector{Int}}
    end
    # Don't explicitly give the struct-type, it should default to `Struct()`

    @test c(SS(true, S(5), [ 4, 5, 6 ])) ==
          Dict("a" => true,
               "b" => Dict("x" => 5),
               "c" => [ 4, 5, 6 ])
    # Note that the null field will be omitted in writing.
    @test c(SS(-Inf16, S(Symbol("x\"y\"z-3")), nothing)) ==
          Dict("a" => "-Inf",
               "b" => Dict("x" => "x\"y\"z-3"))
end

println("#TODO: Converter Read struct data")
println("#TODO: Test non-default converter settings")

false && @testset "Dict{}" begin
    d = ReflToml.read("value=5")
    @test d isa Dict{String, Any}
    @test haskey(d, "value")
    @test d["value"] isa Real
    @test d["value"] == 5
    @test length(d) == 1

    d = ReflToml.read("[A] \n x=4 \n y=7 \n [B] \n x=10 \n y=20 ")
    @test d == Dict(
        "A" => Dict(
            "x" => 4,
            "y" => 7
        ),
        "B" => Dict(
            "x" => 10,
            "y" => 20
        )
    )

    d = ReflToml.read("""
        [A]
        arr = [ 2, 4, 6, "eight" ]
        bool = true

        [A.I]
        inner = 1.234

        [B]
        [[B.arr]]
        x = 10
        y = 11
        [[B.arr]]
        x = 20
        y = 21
    """)
    @test d == Dict(
        "A" => Dict(
            "arr" => [ 2, 4, 6, "eight" ],
            "bool" => true,
            "I" => Dict(
                "inner" => 1.234
            )
        ),
        "B" => Dict(
            "arr" => [
                Dict("x"=>10, "y"=>11),
                Dict("x"=>20, "y"=>21)
            ]
        )
    )
end

false && @testset "Plain struct" begin

end