(* ::Package:: *)

BeginPackage["SellerPackage`"]


(* ::Text:: *)
(*Functions and constants declaration, documentation*)


Seller
Nested


(* ::Text:: *)
(*Definitions*)


Begin["Private`"]


(* ::Text:: *)
(*Auxiliary functions*)


findParents[graph: {{_Integer, _Integer}..}, vertex_Integer] := Pick[graph[[All, 1]], graph[[All, 2]], vertex]

findParents[graph: {{_Integer, _Integer}..}, vertices: {__Integer}] := Flatten[findParents[graph, #]&/@vertices]


findAllParents[graph: {{_Integer, _Integer}..}, vertex_Integer] := findAllParents[graph, {vertex}]

findAllParents[graph: {{_Integer, _Integer}..}, vertices: {__Integer}] := Flatten@NestWhileList[findParents[graph, #]&, vertices, # != {}&]


findChilds[graph: {{_Integer, _Integer}..}, vertex_Integer] := Pick[graph[[All, 2]], graph[[All, 1]], vertex]

findChilds[graph: {{_Integer, _Integer}..}, vertices: {__Integer}] := Flatten[findChilds[graph, #]&/@vertices]


findAllChilds[fares: {__Integer}, hierarchy: {{_Integer, _Integer}..}, order_Integer?Positive] := findAllChilds[fares, hierarchy, {order}, order]

findAllChilds[fares: {__Integer}, hierarchy: {{_Integer, _Integer}..}, vertices: {__Integer}, order_Integer] := Flatten@NestWhileList[
  Select[findChilds[hierarchy, #], fares[[#]] == fares[[order]]&]&, 
  vertices,
  # != {}&
]


sellNested[priceClasses_, membershipMatrix_, hierarchy_, order_] := Module[
  {connectedProducts, parents, vertices},
  connectedProducts = Union[Position[Pick[membershipMatrix, membershipMatrix[[All, order]], 1], 1][[All, 2]]];
  parents = findAllParents[Select[hierarchy, ContainsOnly[#, connectedProducts]&], order];
  vertices = Apply[Union, findAllChilds[priceClasses, hierarchy, #]&/@parents];
  ReplacePart[priceClasses, Thread[vertices -> Clip[priceClasses[[vertices]] - 1, {0, 1000}]]]
]


nestedSeller::argx = "Wrong argument";

nestedSeller[
  membershipMatrix_, 
  hierarchy: {{_Integer, _Integer}..}, 
  priceClasses: {__Integer?NonNegative}, 
  orders: {__Integer?Positive}
] /; And[
  ContainsOnly[orders, Range@Length@priceClasses],
  ContainsOnly[Flatten@membershipMatrix, {0, 1}],
  Last@Dimensions@membershipMatrix == Length@priceClasses,
  ContainsOnly[Flatten@hierarchy, Range@Length@priceClasses]
] := FoldList[
  sellNested[#, membershipMatrix, hierarchy, #2]&, 
  priceClasses, 
  orders
]

nestedSeller[membershipMatrix_, hierarchy_, priceClasses_, orders_] := CompoundExpression[
  Message[nestedSeller::argx],
  HoldForm[nestedSeller[priceClasses, orders]]
]


(* ::Text:: *)
(*Mains function*)


nonNestedSeller::argxPriceClasses = "priceClasses is not valid. Must be list of non-nega`tive integers. Got `1`.";

nonNestedSeller::argxOrders = "orders are not valid. Must be list of positive integers between one and `2`. Got `1`.";

nonNestedSeller[priceClasses: {__Integer?NonNegative}, orders: {__Integer?Positive}] /; ContainsOnly[orders, Range@Length@priceClasses] := 
  FoldList[If[#1[[#2]] != 0, ReplacePart[#1, #2 -> #1[[#2]] - 1], #1]&, priceClasses, orders]

nonNestedSeller[priceClasses: {__Integer?NonNegative}, orders_] := CompoundExpression[
  Message[nonNestedSeller::argxOrders, orders, Length@priceClasses],
  HoldForm[nonNestedSeller[priceClasses, orders]]
]

nonNestedSeller[priceClasses_, orders_] := CompoundExpression[
  Message[nonNestedSeller::argxPriceClasses, priceClasses, Length@priceClasses],
  HoldForm[nonNestedSeller[priceClasses, orders]]
]


Seller::argx = "Not enough arguments for nested system. Maybe you want eval Seller[membershipMatrix, hierarchy, priceClasses, orders, Nested \[Rule] True]";

Seller[priceClasses_, orders_, OptionsPattern[{Nested -> False}]] := If[
  OptionValue[Nested], 
  Message[Seller::argx]; 
  HoldForm[Seller[priceClasses, orders]], 
  nonNestedSeller[priceClasses, orders]
]

Seller[membershipMatrix_, hierarchy_, priceClasses_, orders_, OptionsPattern[{Nested -> True}]] := If[
  OptionValue[Nested], 
  nestedSeller[membershipMatrix, hierarchy, priceClasses, orders], 
  nonNestedSeller[priceClasses, orders]
]


End[]


EndPackage[]
