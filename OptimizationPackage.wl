(* ::Package:: *)

BeginPackage["OptimizationPackage`"]


(* ::Text:: *)
(*Functions and constants declaration, documentation*)


FindBookingLimits
Model


(* ::Text:: *)
(*Definitions*)


Begin["Private`"]


Needs["OptimizationValidationPackage`"]


(* ::Text:: *)
(*EMSR functions*)


littleWood[{distribution_?DistributionParameterQ, f1_?Positive}, f2_?Positive] := NSolve[f1(1 - CDF[distribution, b]) == f2,b ][[-1, -1, -1]]


metaFareClass[distributions: {__?DistributionParameterQ}, fares: {__Integer?Positive}] /; And[
  Length[distributions] == Length[fares],
  OrderedQ@Reverse@fares
] := Module[
  {x, vars},
  vars = Array[x, Length@fares];
  {TransformedDistribution[Total@vars,MapThread[Distributed,{vars,distributions}]],
  fares.Normalize[Mean/@distributions, Total]}
]


emsr[distributions: {__?DistributionParameterQ}, fares:{__Integer?Positive}, capacity_Integer?Positive] /; And[
  Length[distributions]==Length[fares],
  OrderedQ@Reverse@fares
] := Append[Floor@Table[littleWood[metaFareClass[Take[distributions, i], Take[fares, i]], fares[[i + 1]]], {i, Length@fares - 1}], capacity]


dlpFindShadowPrices[context_]:=With[
  {
    model = dlpModel@OptimizationValidationPackage`dlpReadContext@context,
    n = context["numberOfProducts"],
    m = context["numberOfFlights"]
  },
  LinearOptimization[model[[1]], model[[2]], "DualMaximizer"][[1, m + 1;;m + n]]
]


pricesUp[prices: {__Integer}] := prices - Min[prices] + 1


protectionLevelsToBookingLimits[protectionLevels_] := protectionLevelsToBookingLimits[protectionLevels, Last@protectionLevels]
protectionLevelsToBookingLimits[protectionLevels_, capacity_] := Prepend[Most[capacity - protectionLevels], capacity]


emsrFindBookingLimitsStep[context_, flightProducts_, shadowPrices_, i_]:=Module[
  {flight, prices, protectionLevels, bookingLimits},
  flight = flightProducts[[i]];
  prices = ReverseSortBy[{flight, pricesUp[shadowPrices[[flight]]]}\[Transpose], Last];
  protectionLevels = Clip[#, {0, Last@#}]&@emsr[context["demand"][[flight]], prices[[All, 2]], context["capacity"][[i]]];
  bookingLimits = protectionLevelsToBookingLimits@protectionLevels;
  ReplacePart[ConstantArray[0, context["numberOfProducts"]], Thread[flight -> bookingLimits]]
]


emsrFindBookingLimits[context_] := Module[
  {flightProducts, shadowPrices},
  shadowPrices = dlpFindShadowPrices@context;
  flightProducts = Pick[Range[context["numberOfProducts"]], #, 1]&/@context["membershipMatrix"];
  Max/@Transpose@Table[emsrFindBookingLimitsStep[context, flightProducts, shadowPrices, i], {i, context["numberOfFlights"]}]
]


(* ::Text:: *)
(*Auxiliary functions*)


dlpRoundModel[context_, lowerBound: {__?NonNegative}, upperBound: {__?NonNegative}] /; And[
  Length@lowerBound == context["numberOfProducts"],
  Length@upperBound == context["numberOfProducts"]
] := Module[
  {lowerBoundConstraint, upperBoundConstraint, capacityConstraint, target = -context["fares"]},
  
  capacityConstraint = {-context["membershipMatrix"], context["capacity"]};
  upperBoundConstraint = {-IdentityMatrix[context["numberOfProducts"]], upperBound};
  lowerBoundConstraint = {IdentityMatrix[context["numberOfProducts"]], -lowerBound};
  
  {target, Join[capacityConstraint, upperBoundConstraint, lowerBoundConstraint, 2]}
]


Clear[dlpModel]
dlpModel[context_] := dlpRoundModel[context, ConstantArray[0, context["numberOfProducts"]], context["demand"]]


dlpFindBookingLimits[context_] := dlpValidResult[context["numberOfProducts"], LinearOptimization@@dlpModel@dlpReadContext@context]


dlpRoundBookingLimits[context_, bookingLimits_] := dlpValidResult[
  context["numberOfProducts"],
  LinearOptimization@@dlpRoundModel[context, Floor@bookingLimits, Ceiling@bookingLimits]
]


rlpExperiment[context_] := dlpFindBookingLimits@ReplacePart[context, "demand" -> Map[RandomVariate, context["demand"]]]


rlpFindBookingLimits[context_] := With[{ctx = rlpReadContext@context}, Mean@Table[rlpExperiment@ctx, ctx["numberOfExperiments"]]]


pnlpFindBookingLimits[context_] := Module[
  {ctx = rlpReadContext@context, x, vars, target, constraints},
  vars = Array[x, context["numberOfProducts"]];
  target = context["fares"].MapThread[Mean@TruncatedDistribution[{-\[Infinity], #}, #2]&, {vars, context["demand"]}];
  constraints = Join[
    Thread[context["membershipMatrix"].vars <= context["capacity"]],
    Thread[vars <= Mean/@context["demand"]],
    Thread[vars >= 0]
  ];
  NMaximize[{target, constraints}, vars][[-1, All, -1]]
]


(* ::Text:: *)
(*Main function*)


FindBookingLimits::option="Wrong model. Could be DLP, PNLP, Round PNLP, RLP or Round RLP. Got `1`";

FindBookingLimits[context_, OptionsPattern[{Model -> "DLP"}]] := With[
  {model = OptionValue[Model]}, 
  Which[
    model == "DLP", dlpFindBookingLimits@context,
    model == "PNLP", pnlpFindBookingLimits@context,
    model == "Round PNLP", dlpRoundBookingLimits[context, pnlpFindBookingLimits@context],
    model == "RLP", rlpFindBookingLimits@context,
    model == "Round RLP", dlpRoundBookingLimits[context, rlpFindBookingLimits@context],
    model == "EMSR", emsrFindBookingLimits@context,
    True, Message[FindBookingLimits::option, model];
    HoldForm@FindBookingLimits[context, Model -> model]
  ]
]


End[]


EndPackage[]
