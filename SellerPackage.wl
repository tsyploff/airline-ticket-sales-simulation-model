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


(* ::Text:: *)
(*Main function*)


Seller::argx = "Not implemented error. There is currently no nested sales feature, but we are working on it.";

Seller[priceClasses_, orders_, OptionsPattern[{Nested -> False}]] := If[
  OptionValue[Nested], 
  Message[Seller::argx]; 
  HoldForm[Seller[priceClasses, orders]], 
  nonNestedSeller[priceClasses, orders]
]


End[]


EndPackage[]
