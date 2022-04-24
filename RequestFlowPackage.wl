(* ::Package:: *)

BeginPackage["RequestFlowPackage`"]


(* ::Text:: *)
(*Functions and constants declaration, documentation*)


RequestFlow::usage = "Gives the request flow from one to numberOfProducts with time of receipt between 0 and saleHorizon using your time dist and requests count dist";


(* ::Text:: *)
(*Definitions*)


Begin["Private`"]


(* ::Text:: *)
(*Auxiliary functions*)


readNumberOfRequests::argx = "Wrong numberOfRequests argument. Could be list of integers or distributions, or multidimensial distribution. If distribution, each sample must be the list of nonnegative inetegers size `1`";

readNumberOfRequests[numberOfProducts_, numberOfRequests: {__Integer?NonNegative}] /; numberOfProducts == Length@numberOfRequests := numberOfRequests

readNumberOfRequests[numberOfProducts_, numberOfRequests_List] /; AllTrue[numberOfRequests, DistributionParameterQ] := readNumberOfRequests[numberOfProducts, RandomVariate/@numberOfRequests]

readNumberOfRequests[numberOfProducts_, numberOfRequests_?DistributionParameterQ] := readNumberOfRequests[numberOfProducts, RandomVariate@numberOfRequests]

readNumberOfRequests[numberOfProducts_, numberOfRequests_] := CompoundExpression[
  Message[readNumberOfRequests::argx, numberOfProducts],
  HoldForm@readNumberOfRequests[numberOfProducts, numberOfRequests]
]


validRequestTimeDistributionsQ[numberOfProducts_, requestTimeDistributions_List] := And[
  AllTrue[requestTimeDistributions, DistributionParameterQ],
  numberOfProducts == Length@requestTimeDistributions
]


validRequestFlowQ[numberOfProducts_, saleHorizon_, requestFlow: {{_, _}..}] := And[
  ContainsOnly[requestFlow[[All, 2]], Range@numberOfProducts],
  AllTrue[requestFlow[[All, 1]], IntervalMemberQ[Interval[{0, saleHorizon}], #]&]
]


writeRequestFlow::argx = "Wrong requestTimeDistributions. Each sample must be member of [0, `1`] interval.";

writeRequestFlow[numberOfProducts_, saleHorizon_, requestFlow_] /; validRequestFlowQ[numberOfProducts, saleHorizon, requestFlow] := requestFlow

writeRequestFlow[numberOfProducts_, saleHorizon_, requestFlow_] := CompoundExpression[
  Message[writeRequestFlow::argx, saleHorizon],
  HoldForm@writeRequestFlow[numberOfProducts, saleHorizon, requestFlow]
]


(* ::Text:: *)
(*Main functionals*)


RequestFlow[numberOfRequests_, requestTimeDistributions_] := SortBy[First]@Flatten[MapIndexed[Thread[{#, First@#2}]&, MapThread[RandomVariate, {requestTimeDistributions, numberOfRequests}]], 1]

RequestFlow[numberOfProducts_Integer?Positive, saleHorizon_Integer?Positive, numberOfRequests_, requestTimeDistributions_] /; validRequestTimeDistributionsQ[
  numberOfProducts, 
  requestTimeDistributions
] := writeRequestFlow[numberOfProducts, saleHorizon, RequestFlow[readNumberOfRequests[numberOfProducts, numberOfRequests], requestTimeDistributions]]


End[]


EndPackage[]
