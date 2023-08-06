(*
A functional Pokédex written in Mathematica. Supports all 898 Pokémon, but not alternate forms (yet). 
Current features:
- Pokédex pagination.
- Search by name w/ autocomplete.
- Pokémon info and image.
- Pokémon base stats with graph.
In-progress: 
- Comment the code better.
- Evolution chain formatting.
- EV/IV calculations.
- Alternate Pokémon forms. 
*)

(* Small/Medium/Large do not work for individual bounds.
Get the size in pixels, and use that instead.
*)
rasterSizes = 
  Rasterize[Graphics[{}, ImageSize -> #], "RasterSize"] & /@ {Tiny, 
    Small, Medium, Large};

(* Fetch data from the Pokémon api. *)

endpoint = "https://pokeapi.co/api/v2/";
pokeFetch[url_] := Enclose[
  ImportString[
   ToString[
    (r |-> ConfirmBy[r, #["StatusCode"] == 200 &]["Body"])@
     URLRead[HTTPRequest[ToString@url]], 
    OutputForm, CharacterEncoding -> "UTF8"
    ], 
   "RawJSON"
   ]
  ]

(* Pokédex 404 page .
   Right now, there is no reason this should be shown .
 *)
pageNotFound[r_String] := Column[{
   TextCell["Oops!", "Subtitle"],
   TextCell[
    "Looks like you've stumbled upon a page that does not exist. \
Maybe you spelled a Pokemon's name wrong or looked up an invalid ID, \
but this may also be an error on our end.", "Text"]
   }]

(* Pokémon evolution chain *)

evolutionView[evo_] := If[! MatchQ[evo, _Failure],
  DynamicModule[{
    chain = evo[["chain"]]
    },
   OpenerView[{
     TextCell["Evolution Chain", Bold],
     TextCell[
      If[Length@#1[["evolves_to"]] > 0,
          
          Join[{Capitalize@#1[["species"]][["name"]]}, #0 /@ #1[[
             "evolves_to"]]], 
          {Capitalize@#1[["species"]][["name"]]}
          ] &@chain //. {
        (
         {a_, b : {_, {_}} ..} :> (StringForm[
            "`` -> (``)",
            a,
            StringRiffle[First@# -> First@Last@# & /@ {b}, " or "]
            ])
         ),
        (
         {a_, {b_, c : {_} ..}} :> (StringForm[
            "`` -> `` -> (``)", 
            a, 
            b, 
            StringRiffle[First@# & /@ {c}, " or "]
            ])
         ),
        (
         {a_, {b_} ..} :> If[
           Length@{b} > 0,
           Print[{b}];
           StringForm[
            "`` -> (``)",
            a,
            StringRiffle[First@# & /@ {b}, ", "]
            ],
           a
           ]
         )
        }
      ]
     }]
   ],
  pageNotFound["Pokédex"]
  ]

(* Pokémon Pokédex page *)

showPokemon[poke_] := If[! MatchQ[poke, _Failure],
  DynamicModule[{
    name = Capitalize@poke[["species"]][["name"]],
    id = poke[["id"]],
    abilities = 
     Capitalize@#[["ability"]][["name"]] & /@ poke[["abilities"]],
    sprite = poke[["sprites"]][["front_default"]],
    types = Capitalize@#[["type"]][["name"]] & /@ poke[["types"]],
    stats = 
     Association[
      Capitalize@#[["stat"]][["name"]] -> #[["base_stat"]] & /@ 
       poke[["stats"]]],
    species = 
     pokeFetch[
      StringForm["``pokemon-species/``", endpoint, poke[["id"]]]]
    },
   template = {{0, 0, #}, {0, #, #}, {0, #, 0}, {#, #, 0}, {#, 0, 
        0}, {#, 0, #}} &@Max[200, Max@Values[stats]];
   Pane[
    Column[{
      Row[{
        Column[{
          (*Button["< Pokédex", DialogReturn[1], Appearance->
          Frameless, BaseStyle->{"Output", Blue}],*)
          
          TextCell[name, "Subtitle"],
          Row[{
            TextCell["Dex #", Bold],
            TextCell[id]
            }],
          Row[{
            TextCell["Types ", Bold],
            TextCell[StringRiffle[types, "/"]]
            }],
          Row[{
            TextCell["Abilities ", Bold],
            
            TextCell[
             StringRiffle[
              If[EvenQ[#], abilities[[#]] <> "\n", abilities[[#]]] & /@
                Range[Length@abilities], "/"]]
            }],
          TextCell["Base Stats", Bold],
          Spacer[0],
          Grid[
           KeyValueMap[{
              TextCell[#1, Bold],
              TextCell [#2]
              } &, stats
            ], Alignment -> Left
           ]
          }, Spacings -> 0],
        Spacer[20],
        Column[{
            Image[Import[sprite], ImageSize -> Tiny],
            Graphics3D[{
              EdgeForm[Dashed],
              Pink,
              Opacity[0.75],
              Polygon[Values[#]],
              (i |-> 
                 Text[Style[Keys[#][[i]], Bold, Black], 
                  template[[i]]]) /@ Range[Length@#]
              },
             BoxRatios -> {1, 1, 1},
             ViewPoint -> {90 Degree, 90 Degree, 90 Degree},
             ImageSize -> Tiny
             ]
            }] &@Association@MapIndexed[(
            {s, i} |-> 
             StringRiffle[
               ToUpperCase@
                  StringTake[#, If[2 < StringLength[#] < 7, 1, 2]] & /@
                 StringSplit[Keys[stats][[First@i]], "-"], 
               ". "] -> (If[s[[#]] != 0, Values[stats][[First@i]], 
                  0] & /@ Range[Length@s])
            ), template]
        }],
      Spacer[20],
      Dynamic@
       evolutionView[pokeFetch[species[["evolution_chain"]][["url"]]]],
      Hyperlink[name <> " @ pokemon.com", 
       "https://www.pokemon.com/us/pokedex/" <> name]
      }], Scrollbars -> {False, True}, AppearanceElements -> None, 
    ImageSize -> {Large, 
      UpTo[Mean[{rasterSizes[[3]][[1]], rasterSizes[[2]][[1]]}]]}
    ]
   ],
  pageNotFound["Pokédex"]
  ]

(* Pokédex search/pagination sidebar *)

allPokemon = 
  Association[#[["name"]] -> #[["url"]] & /@ 
    pokeFetch[StringForm["``pokemon?limit=898&offset=0", endpoint]][[
     "results"]]];
showPokedex[] := DynamicModule[{input = "", offset = 1, id = 1},
  CreateDialog[
   Row[{
     Column[{
       TextCell["Pokédex", "Section"],
       InputField[Dynamic[input], String, ContinuousAction -> False, 
        FieldHint -> "Enter a name", 
        FieldCompletionFunction -> (in |-> (
            l = ToLowerCase@in;
            (If[StringContainsQ[#, l], #, Nothing] & /@ 
              Keys[allPokemon])
            ))],
       Button["\[UpArrow]", offset -= Min[8, offset - 1], 
        BaseStyle -> {Bold, Black}],
       Dynamic[
        If[input != "" && ! MatchQ[#, _Failure], 
           offset = ToExpression[#[["id"]]]]
          &@pokeFetch[StringForm["``pokemon/``", endpoint, input]];
        Dynamic@
         TextCell[StringForm["Showing ``-``", offset, offset + 7], 
          "Output", Bold, Black],
        TrackedSymbols :> {input} 
        ],
       Dynamic[
        Column[
         If[
            # == "Classified",
            
            Button[#, Appearance -> Frameless, 
             BaseStyle -> {"Output", Bold, Blue} , Enabled -> False],
            Button[
             Capitalize@#,
             
             id = 
              ToExpression@Last@StringSplit[allPokemon[[#]], "/"],
             Appearance -> Frameless, 
             BaseStyle -> {"Output", Bold, Blue} 
             ]
            ] & /@ 
          PadRight[
           Keys[allPokemon][[
            offset ;; offset + Min[7, Length@allPokemon - offset]]], 
           8, "Classified"]
         ], TrackedSymbols :> {offset}
        ],
       Button["\[DownArrow]", 
        offset += Min[8, Length@allPokemon - offset - 7], 
        BaseStyle -> {Bold, Black}]
       }, Alignment -> Top],
     Spacer[50],
     Dynamic[
      showPokemon[pokeFetch[StringForm["``pokemon/``", endpoint, id]]],
      TrackedSymbols :> {id}
      ]
     }], WindowSize -> Fit]
  ]

showPokedex[]
