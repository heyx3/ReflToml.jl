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
    @test c(Dict(:a=>:b, :c=>:d)) == Dict("a"=>"b", "c"=>"d")
end
@testset "Converter Read basic data" begin
    c = WOML.Converter{CM_Read}(
        null_string = "T_NULL",
        numbers_can_be_strings = true,
        bools_can_be_strings = true
    )

    @test c(5, Int) === 5
    @test c(5, Float32) === Float32(5)
    @test c(true, Bool) === true

    s = "hello"
    @test c(s, String) === s
    @test c(s, AbstractString) === s
    @test c("hello", String) == s
    @test c("hello", AbstractString) == s

    @test c("4.5", Any) === 4.5
    @test c("false", Any) === false

    @test c("f", Any) === "f"
    @test c("f", Bool) === false
    @test c("True", Any) === "True"
    @test c("True", Bool) === true

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

    for (actual, expected) in zip(c([ "T_NULL", "+Inf", "NaN", -20 ], Vector{Any}),
                                    Any[ nothing, Inf64, NaN64, -20 ])
        @test actual === expected
    end

    real_d = c(Dict(
        "+Inf" => 70,
        "70" => "+Inf",

        "NaN" => 30,
        30 => "NaN"
    ), Dict{AbstractFloat, AbstractFloat})
    expected_d = Dict(
        +Inf => 70.0,
        70.0 => +Inf,

        NaN => 30.0,
        30.0 => NaN
    )
    @test length(real_d) == length(expected_d)
    for expected_pair in expected_d
        @test haskey(real_d, expected_pair[1])
        @test real_d[expected_pair[1]] === expected_pair[2]
    end

    in_d = Dict(
        "7.5"=>"abcd",
        "true" => [ "a", "b", "c" ],
        "def" => [
            [ 1, 2, 3 ],
            [ 4, 5, 6 ],
            [ 7, 8, 9 ]
        ],
        "false" => [
            Dict("j"=>1),
            Dict("j"=>2),
            Dict("j"=>3)
        ]
    )
    expected_d = Dict(
        7.5=>"abcd",
        true => [ "a", "b", "c" ],
        "def" => [
            [ 1, 2, 3 ],
            [ 4, 5, 6 ],
            [ 7, 8, 9 ]
        ],
        false => [
            Dict("j"=>1),
            Dict("j"=>2),
            Dict("j"=>3)
        ]
    )
    @test c(in_d, Dict{Any, Any}) == expected_d

    @test c(Dict("a"=>"b", "c"=>"d"), Dict{Symbol, Symbol}) ==
          Dict(:a=>:b, :c=>:d)
end

@testset "Converter Write struct data" begin
    c = WOML.Converter{CM_Write}(
        default_struct_type = StructTypes.Struct(),
        write_null_fields = false,
        numbers_can_be_strings = true
    )

    struct Sw
        x
    end
    StructTypes.StructType(::Type{Sw}) = StructTypes.Struct()

    @test c(Sw(5)) == Dict("x"=>5)
    @test c(Sw(:a)) == Dict("x"=>"a")
    @test c(Sw("1\"2\"3")) == Dict("x"=>"1\"2\"3")

    @test c(Sw([ 1, 2, 3 ])) == Dict("x" => [ 1, 2, 3 ])
    @test c(Sw(Dict(:y => NaN))) == Dict("x" => Dict("y" => "NaN"))

    @test c(Sw(Sw(-1.5))) == Dict("x" => Dict("x" => -1.5))


    struct SSw
        a::Real
        b::Sw
        c::Union{Nothing, Vector{Int}}
    end
    # Don't explicitly give the struct-type, it should default to `Struct()`.

    @test c(SSw(true, Sw(5), [ 4, 5, 6 ])) ==
          Dict("a" => true,
               "b" => Dict("x" => 5),
               "c" => [ 4, 5, 6 ])
    # Note that the null field will be omitted in writing.
    @test c(SSw(-Inf16, Sw(Symbol("x\"y\"z-3")), nothing)) ==
          Dict("a" => "-Inf",
               "b" => Dict("x" => "x\"y\"z-3"))
end
@testset "Converter Read struct data" begin
    c = WOML.Converter{CM_Read}(
        null_string = "T_NULL",
        default_struct_type = StructTypes.Struct(),
        numbers_can_be_strings = true,
        bools_can_be_strings = true
    )

    struct Sr
        x
    end
    StructTypes.StructType(::Type{Sr}) = StructTypes.Struct()
    Base.:(==)(a::Sr, b::Sr) = isequal(a.x, b.x)  # To handle things like NaN fields

    @test c(Dict("x"=>5), Sr) === Sr(5)
    @test c(Dict("x"=>"a"), Sr) === Sr("a")
    @test c(Dict("x"=>"1\"2\"3"), Sr) === Sr("1\"2\"3")

    @test c(Dict("x" => [ 1, 2, 3 ]), Sr) == Sr([ 1, 2, 3 ])
    @test c(Dict("x" => Dict("y" => "NaN")), Sr) == Sr(Dict("y" => NaN))

    @test c(Dict("x" => -1.5), Sr) === Sr(-1.5)
    @test c(Dict("x" => "-1.5"), Sr) === Sr(-1.5)


    struct SSr
        a::Real
        b::Sr
        c::Vector{Int}
        d::Nothing
    end
    # Don't explicitly give the struct-type, it should default to `Struct()`.
    Base.:(==)(a::SSr, b::SSr) = all(isequal(getfield(a, n), getfield(b, n))
                                       for n in fieldnames(SSr))

    @test c(Dict("a" => true,
                 "b" => Dict("x" => 5),
                 "c" => [ 4, 5, 6 ],
                 "d" => "T_NULL"),
            SSr) ==
          SSr(true, Sr(5), [ 4, 5, 6 ], nothing)
    # Try again, omitting the field that is null.
    @test c(Dict("a" => "-Inf",
                 "b" => Dict("x" => "x\"y\"z-3"),
                 "c" => [ ]),
            SSr) ==
          SSr(-Inf64, Sr("x\"y\"z-3"), Int[ ], nothing)
end

println("#TODO: Test non-default converter settings")
println("#TODO: Test abstract types")
println("#TODO: Test custom types")
println("#TODO: Add union tests to all test sets")