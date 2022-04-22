(* ::Package:: *)

(* ::Text:: *)
(*\:041f\:0440\:043e\:043f\:0438\:0441\:044b\:0432\:0430\:0435\:043c \:043d\:0430\:0438\:043c\:0435\:043d\:043e\:0432\:0430\:043d\:0438\:0435 \:043f\:0430\:043a\:0435\:0442\:0430*)


BeginPackage["RequestFlowPackage`"]


(* ::Text:: *)
(*\:0417\:0430\:043f\:043e\:043b\:043d\:044f\:0435\:043c \:0434\:043e\:043a\:0443\:043c\:0435\:043d\:0442\:0430\:0446\:0438\:044e*)


RequestFlow::usage="";


(* ::Text:: *)
(*\:041e\:0442\:043a\:0440\:044b\:0432\:0430\:0435\:043c \:0432\:0441\:043f\:043e\:043c\:043e\:0433\:0430\:0442\:0435\:043b\:044c\:043d\:044b\:0439 \:043f\:0440\:0438\:0432\:0430\:0442\:043d\:044b\:0439 \:043a\:043e\:043d\:0442\:0435\:043a\:0441\:0442 \:0434\:043b\:044f \:043e\:043f\:0438\:0441\:0430\:043d\:0438\:044f \:0444\:0443\:043d\:043a\:0446\:0438\:0438 \:0438 \:0432\:0441\:043f\:043e\:043c\:043e\:0433\:0430\:0442\:0435\:043b\:044c\:043d\:044b\:0445 \:0444\:0443\:043d\:043a\:0446\:0438\:0439*)


Begin["Private`"]


(* ::Text:: *)
(*\:041e\:043f\:0440\:0435\:0434\:0435\:043b\:044f\:0435\:043c \:0432\:0441\:043f\:043e\:043c\:043e\:0433\:0430\:0442\:0435\:043b\:044c\:043d\:044b\:0435 \:0444\:0443\:043d\:043a\:0446\:0438\:0438*)


Clear[readNumberOfRequests]
readNumberOfRequests::argx="Wrong numberOfRequests argument. Could be list of integers or distributions, or multidimensial distribution. If distribution, each sample must be the list of nonnegative inetegers size `1`";

readNumberOfRequests[numberOfProducts_,numberOfRequests:{__Integer?NonNegative}]/;numberOfProducts==Length@numberOfRequests:=numberOfRequests

readNumberOfRequests[numberOfProducts_,numberOfRequests_List]/;AllTrue[numberOfRequests,DistributionParameterQ]:=readNumberOfRequests[numberOfProducts,RandomVariate/@numberOfRequests]

readNumberOfRequests[numberOfProducts_,numberOfRequests_?DistributionParameterQ]:=readNumberOfRequests[numberOfProducts,RandomVariate@numberOfRequests]

readNumberOfRequests[numberOfProducts_,numberOfRequests_]:=CompoundExpression[Message[readNumberOfRequests::argx,numberOfProducts],HoldForm@readNumberOfRequests[numberOfProducts,numberOfRequests]]


Clear[requestFlowValidResult]
requestFlowValidResult[numberOfProducts_,saleHorizon_][result:{{_,_}..}]:=And[ContainsOnly[result[[All,2]],Range@numberOfProducts],AllTrue[result[[All,1]],IntervalMemberQ[Interval[{0,saleHorizon}],#]&]]


(* ::Text:: *)
(*\:041e\:043f\:0440\:0435\:0434\:0435\:043b\:044f\:0435\:043c \:043e\:0441\:043d\:043e\:0432\:043d\:0443\:044e \:0444\:0443\:043d\:043a\:0446\:0438\:044e \:043f\:0430\:043a\:0435\:0442\:0430*)


Clear[RequestFlow]
RequestFlow::argx="Wrong requestTimeDistributions. Each sample must be member of [0, `1`] interval.";
RequestFlow[numberOfProducts_Integer?Positive,saleHorizon_Integer?Positive,numberOfRequests_,requestTimeDistributions_List]/;And[
AllTrue[requestTimeDistributions,DistributionParameterQ],
numberOfProducts==Length@requestTimeDistributions
]:=Module[
{requestsCount=readNumberOfRequests[numberOfProducts,numberOfRequests],requestFlow},

requestFlow="your code here";

If[
requestFlowValidResult[numberOfProducts,saleHorizon][requestFlow],
requestFlow,
Message[RequestFlow::argx,saleHorizon];
HoldForm@RequestFlow[numberOfProducts,saleHorizon,numberOfRequests,requestTimeDistributions]
]
]


End[]


EndPackage[]
