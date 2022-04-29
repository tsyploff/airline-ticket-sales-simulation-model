(* ::Package:: *)

BeginPackage["OptimizationValidationPackage`"]


(* ::Text:: *)
(*Functions and constants declaration, documentation*)


dlpValidateNumberOfProducts
dlpValidateNumberOfFlights
dlpValidateMembershipMatrix
dlpValidateFares
dlpValidateDemand
dlpValidateCapacity
dlpReadContext
dlpValidResult
rlpValidateDemand
rlpValidateNumberOfExperiments
rlpReadContext


(* ::Text:: *)
(*Definitions*)


Begin["Private`"]


dlpValidateNumberOfProducts::argx = "numberOfProducts is not valid. The number must be a positive integer. Got `1`";

dlpValidateNumberOfProducts[numberOfProducts_Integer?Positive] := numberOfProducts

dlpValidateNumberOfProducts[numberOfProducts_] := CompoundExpression[
  Message[dlpValidateNumberOfProducts::argx, numberOfProducts], 
  HoldForm[dlpValidateNumberOfProducts[numberOfProducts]]
]


dlpValidateNumberOfFlights::argx = "numberOfFlights is not valid. The number must be a positive integer. Got `1`";

dlpValidateNumberOfFlights[numberOfFlights_Integer?Positive] := numberOfFlights

dlpValidateNumberOfFlights[numberOfFlights_] := CompoundExpression[
  Message[dlpValidateNumberOfFlights::argx, numberOfFlights],
  HoldForm[dlpValidateNumberOfFlights[numberOfFlights]]
]


dlpValidateMembershipMatrix::argx = "membershipMatrix is not valid. Must be matrix of size `1`\[Times]`2` of 1 and 0. Got `3`";

dlpValidateMembershipMatrix[numberOfProducts_, numberOfFlights_, membershipMatrix_] /; And[
  Dimensions@membershipMatrix == {numberOfFlights, numberOfProducts},
  ContainsOnly[Flatten@membershipMatrix, {0, 1}]
] := membershipMatrix

dlpValidateMembershipMatrix[numberOfProducts_, numberOfFlights_, membershipMatrix_] := CompoundExpression[
  Message[dlpValidateMembershipMatrix::argx, numberOfFlights, numberOfProducts, membershipMatrix],
  HoldForm[dlpValidateMembershipMatrix[numberOfProducts, numberOfFlights, membershipMatrix]]
]


dlpValidateFares::argx = "fares is not valid. Must be list of positive integers of length `1`. Got `2`";

dlpValidateFares[numberOfProducts_, fares: {__Integer?Positive}] /; Length@fares == numberOfProducts := fares

dlpValidateFares[numberOfProducts_, fares_] := CompoundExpression[
  Message[dlpValidateFares::argx, numberOfProducts, fares],
  HoldForm[dlpValidateFares[numberOfProducts, fares]]
]


dlpValidateDemand::argx = "demand is not valid. Must be list of positive integers or a list of distributions of length `1` or a multivariate distribution . Got `2`";

dlpValidateDemand[numberOfProducts_, demand: {__?Positive}] /; Length@demand == numberOfProducts := demand

dlpValidateDemand[numberOfProducts_, demand: {__?DistributionParameterQ}] := dlpValidateDemand[numberOfProducts, Mean/@demand]

dlpValidateDemand[numberOfProducts_, demand_?DistributionParameterQ] := dlpValidateDemand[numberOfProducts, Mean@demand]

dlpValidateDemand[numberOfProducts_, demand_] := CompoundExpression[
  Message[dlpValidateDemand::argx, numberOfProducts, demand],
  HoldForm[dlpValidateDemand[numberOfProducts, demand]]
]


dlpValidateCapacity::argx = "capacity is not valid. Must be list of positive integers of length `1`. Got `2`";

dlpValidateCapacity[numberOfFlights_, capacity: {__Integer?Positive}] /; Length@capacity == numberOfFlights := capacity

dlpValidateCapacity[numberOfFlights_, capacity_] := CompoundExpression[
  Message[dlpValidateCapacity::argx, numberOfFlights, capacity],
  HoldForm[dlpValidateCapacity[numberOfFlights, capacity]]
]


dlpReadContext[context_] /; ContainsAll[Keys@context, {"numberOfProducts", "numberOfFlights", "membershipMatrix", "fares", "demand", "capacity"}] := <|
  "numberOfProducts" -> dlpValidateNumberOfProducts@context["numberOfProducts"],
  "numberOfFlights" -> dlpValidateNumberOfFlights@context["numberOfFlights"],
  "membershipMatrix" -> dlpValidateMembershipMatrix[context["numberOfProducts"], context["numberOfFlights"], context["membershipMatrix"]], 
  "fares" -> dlpValidateFares[context["numberOfProducts"], context["fares"]],
  "demand" -> dlpValidateDemand[context["numberOfProducts"], context["demand"]],
  "capacity" -> dlpValidateCapacity[context["numberOfFlights"], context["capacity"]]
|>


dlpValidResult::argx = "bookingLimits is not valid. Must be list of positive integers of length `1`. Got `2`";

dlpValidResult[numberOfProducts_, bookingLimits: {__?NonNegative}] /; Length@bookingLimits == numberOfProducts := bookingLimits

dlpValidResult[numberOfProducts_, bookingLimits_] := CompoundExpression[
  Message[dlpValidResult::argx, numberOfProducts, bookingLimits],
  HoldForm[dlpValidResult[numberOfProducts, bookingLimits]]
]


rlpValidateDemand::argx = "demand is not valid. Must be list of distributions of length `1` or a multivariate distribution . Got `2`";

rlpValidateDemand[numberOfProducts_, demand: {__?DistributionParameterQ}] /; Length@demand == numberOfProducts := demand

rlpValidateDemand[numberOfProducts_, demand_] := CompoundExpression[
  Message[rlpValidateDemand::argx, numberOfProducts, demand],
  HoldForm[rlpValidateDemand[numberOfProducts, demand]]
]


rlpValidateNumberOfExperiments::argx = "numberOfExperiments is not valid. The number must be a positive integer. Got `1`";

rlpValidateNumberOfExperiments[numberOfExperiments_Integer?Positive] := numberOfExperiments

rlpValidateNumberOfExperiments[numberOfExperiments_] := CompoundExpression[
  Message[dlpValidateNumberOfProducts::argx, numberOfExperiments], 
  HoldForm[dlpValidateNumberOfProducts[numberOfExperiments]]
]


rlpReadContext[context_] /; ContainsAll[Keys@context, {"numberOfProducts", "numberOfFlights", "membershipMatrix", "fares", "demand", "capacity", "numberOfExperiments"}] := <|
  "numberOfProducts" -> dlpValidateNumberOfProducts@context["numberOfProducts"],
  "numberOfFlights" -> dlpValidateNumberOfFlights@context["numberOfFlights"],
  "membershipMatrix" -> dlpValidateMembershipMatrix[context["numberOfProducts"], context["numberOfFlights"], context["membershipMatrix"]],
  "fares" -> dlpValidateFares[context["numberOfProducts"], context["fares"]],
  "demand" -> rlpValidateDemand[context["numberOfProducts"], context["demand"]],
  "capacity" -> dlpValidateCapacity[context["numberOfFlights"], context["capacity"]],
  "numberOfExperiments" -> rlpValidateNumberOfExperiments@context["numberOfExperiments"]
|>


End[]


EndPackage[]
