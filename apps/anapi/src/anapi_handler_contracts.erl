-module(anapi_handler_contracts).

-include_lib("dmsl/include/dmsl_payment_processing_thrift.hrl").
-include_lib("dmsl/include/dmsl_domain_thrift.hrl").

-behaviour(anapi_handler).
-export([process_request/3]).
-import(anapi_handler_utils, [general_error/2]).

-spec process_request(
    OperationID :: anapi_handler:operation_id(),
    Req         :: anapi_handler:request_data(),
    Context     :: anapi_handler:processing_context()
) ->
    {ok | error, anapi_handler:response() | noimpl}.

process_request('GetContracts', _Req, Context) ->
    Party = anapi_utils:unwrap(anapi_handler_utils:get_my_party(Context)),
    {ok, {200, #{}, decode_contracts_map(Party#domain_Party.contracts, Party#domain_Party.contractors)}};

process_request('GetContractByID', Req, Context) ->
    ContractID = maps:get('contractID', Req),
    Party = anapi_utils:unwrap(anapi_handler_utils:get_my_party(Context)),
    case genlib_map:get(ContractID, Party#domain_Party.contracts) of
        undefined ->
            {ok, general_error(404, <<"Contract not found">>)};
        Contract ->
            {ok, {200, #{}, decode_contract(Contract, Party#domain_Party.contractors)}}
    end;

process_request('GetContractAdjustments', Req, Context) ->
    case anapi_handler_utils:get_contract_by_id(maps:get('contractID', Req), Context) of
        {ok, #domain_Contract{adjustments = Adjustments}} ->
            Resp = [decode_contract_adjustment(A) || A <- Adjustments],
            {ok, {200, #{}, Resp}};
        {exception, #payproc_ContractNotFound{}} ->
            {ok, general_error(404, <<"Contract not found">>)}
    end;

process_request('GetContractAdjustmentByID', Req, Context) ->
    case anapi_handler_utils:get_contract_by_id(maps:get('contractID', Req), Context) of
        {ok, #domain_Contract{adjustments = Adjustments}} ->
            AdjustmentID = maps:get('adjustmentID', Req),
            case lists:keyfind(AdjustmentID, #domain_ContractAdjustment.id, Adjustments) of
                #domain_ContractAdjustment{} = A ->
                    {ok, {200, #{}, decode_contract_adjustment(A)}};
                false ->
                    {ok, general_error(404, <<"Adjustment not found">>)}
            end;
        {exception, #payproc_ContractNotFound{}} ->
            {ok, general_error(404, <<"Contract not found">>)}
    end;

%%

process_request(_OperationID, _Req, _Context) ->
    {error, noimpl}.

decode_contracts_map(Contracts, Contractors) ->
    anapi_handler_decoder_utils:decode_map(Contracts, fun(C) -> decode_contract(C, Contractors) end).

decode_contract(Contract, Contractors) ->
    anapi_handler_utils:merge_and_compact(#{
        <<"id"                  >> => Contract#domain_Contract.id,
        <<"createdAt"           >> => Contract#domain_Contract.created_at,
        <<"contractor"          >> => anapi_handler_decoder_party:decode_contractor(
            get_contractor(Contract, Contractors)
        ),
        <<"paymentInstitutionID">> =>
            anapi_handler_decoder_party:decode_payment_institution_ref(Contract#domain_Contract.payment_institution),
        <<"validSince"          >> => Contract#domain_Contract.valid_since,
        <<"validUntil"          >> => Contract#domain_Contract.valid_until,
        <<"legalAgreement"      >> => anapi_handler_decoder_utils:decode_optional(
            Contract#domain_Contract.legal_agreement,
            fun anapi_handler_decoder_party:decode_legal_agreement/1
        ),
        <<"reportingPreferences">> => anapi_handler_decoder_utils:decode_optional(
            Contract#domain_Contract.report_preferences,
            fun anapi_handler_decoder_party:decode_reporting_preferences/1
        )
    }, decode_contract_status(Contract#domain_Contract.status)).

decode_contract_status({active, _}) ->
    #{
        <<"status">> => <<"active">>
    };

decode_contract_status({terminated, #domain_ContractTerminated{terminated_at = TerminatedAt}}) ->
    #{
        <<"status">> => <<"terminated">>,
        <<"terminatedAt">> => TerminatedAt
    }.

get_contractor(#domain_Contract{contractor = Contractor}, _) when Contractor =/= undefined ->
    Contractor;
get_contractor(#domain_Contract{contractor_id = ContractorID}, Contractors) ->
    #domain_PartyContractor{
        contractor = Contractor
    } = maps:get(ContractorID, Contractors),
    Contractor.

decode_contract_adjustment(ContractAdjustment) ->
    genlib_map:compact(#{
        <<"id"        >> => ContractAdjustment#domain_ContractAdjustment.id,
        <<"createdAt" >> => ContractAdjustment#domain_ContractAdjustment.created_at,
        <<"validSince">> => ContractAdjustment#domain_ContractAdjustment.valid_since,
        <<"validUntil">> => ContractAdjustment#domain_ContractAdjustment.valid_until
    }).
