using Test, TOML, StructTypes
using WOML


@testset "Converter Write basic data" begin
    c = WOML.Converter{CM_Write}(
        null_string = "T_NULL",
        special_floats_as_strings = true
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

println("#TODO: Non-default converter settings")

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