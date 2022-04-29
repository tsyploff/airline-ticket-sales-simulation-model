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
    True, Message[FindBookingLimits::option, model];
    HoldForm@FindBookingLimits[context, Model -> model]
  ]
]


End[]


EndPackage[]
