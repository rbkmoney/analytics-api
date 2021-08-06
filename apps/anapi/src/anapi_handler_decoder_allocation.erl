-module(anapi_handler_decoder_allocation).

-include_lib("damsel/include/dmsl_domain_thrift.hrl").

-export([decode/1]).

-type allocation() :: dmsl_domain_thrift:'Allocation'().
-type decode_data() :: _.

-spec decode(allocation() | undefined) -> decode_data() | undefined.
decode(undefined) ->
    undefined;
decode(#domain_Allocation{transactions = Transactions}) ->
    [decode_transaction(L) || L <- Transactions].

decode_transaction(Transaction) ->
    Amount = Transaction#domain_AllocationTransaction.amount,
    Map0 = anapi_handler_utils:merge_and_compact(
        #{
            <<"target">> => decode_target(Transaction#domain_AllocationTransaction.target)
        },
        decode_body(Transaction#domain_AllocationTransaction.body, Amount)
    ),
    anapi_handler_utils:merge_and_compact(
        Map0,
        decode_details(Transaction#domain_AllocationTransaction.details)
    ).

decode_target({shop, AllocationShop}) ->
    #{
        <<"allocationTargetType">> => <<"AllocationTargetShop">>,
        <<"shopID">> => AllocationShop#domain_AllocationTransactionTargetShop.shop_id
    }.

decode_details(undefined) ->
    undefined;
decode_details(AllocationDetails) ->
    #{
        <<"cart">> => anapi_handler_decoder_invoicing:decode_invoice_cart(
            AllocationDetails#domain_AllocationTransactionDetails.cart
        )
    }.

decode_body(undefined, TransactionAmount) ->
    #{
        <<"allocationBodyType">> => <<"AllocationBodyAmount">>,
        <<"amount">> => TransactionAmount#domain_Cash.amount,
        <<"currency">> => anapi_handler_decoder_utils:decode_currency(TransactionAmount#domain_Cash.currency)
    };
decode_body(Body, TransactionAmount) ->
    TotalAmount = Body#domain_AllocationTransactionBodyTotal.total,
    FeeAmount = Body#domain_AllocationTransactionBodyTotal.fee_amount,
    FeeTarget = Body#domain_AllocationTransactionBodyTotal.fee_target,
    Fee = Body#domain_AllocationTransactionBodyTotal.fee,
    #{
        <<"allocationBodyType">> => <<"AllocationBodyTotal">>,
        <<"currency">> => anapi_handler_decoder_utils:decode_currency(TotalAmount#domain_Cash.currency),
        <<"total">> => TotalAmount#domain_Cash.amount,
        <<"amount">> => TransactionAmount#domain_Cash.amount,
        <<"fee">> => decode_fee(Fee, FeeTarget, FeeAmount)
    }.

decode_fee(undefined, FeeTarget, FeeAmount) ->
    #{
        <<"allocationFeeType">> => <<"AllocationFeeFixed">>,
        <<"target">> => decode_target(FeeTarget),
        <<"amount">> => FeeAmount#domain_Cash.amount
    };
decode_fee(Fee, FeeTarget, FeeAmount) ->
    #{
        <<"allocationFeeType">> => <<"AllocationFeeShare">>,
        <<"target">> => decode_target(FeeTarget),
        <<"amount">> => FeeAmount#domain_Cash.amount,
        <<"share">> => decode_parts(Fee#domain_AllocationTransactionFeeShare.parts)
    }.

decode_parts(Parts) ->
    #'Rational'{p = P, q = Q} = Parts,
    Exponent = erlang:trunc(math:log10(Q)),
    #{
        <<"m">> => P,
        <<"exp">> => -Exponent
    }.

-ifdef(EUNIT).

-include_lib("eunit/include/eunit.hrl").

-spec test() -> _.

-spec decode_test() -> _.
decode_test() ->
    AllocationCart = #domain_InvoiceCart{
        lines = [
            #domain_InvoiceLine{
                product = <<"info">>,
                quantity = 2,
                price = make_cash(16)
            }
        ]
    },
    Allocation = #domain_Allocation{
        transactions = [
            #domain_AllocationTransaction{
                id = <<"0">>,
                target =
                    {shop, #domain_AllocationTransactionTargetShop{
                        owner_id = <<"partyID1">>,
                        shop_id = <<"shopID1">>
                    }},
                amount = make_cash(32),
                body = #domain_AllocationTransactionBodyTotal{
                    fee_target =
                        {shop, #domain_AllocationTransactionTargetShop{
                            owner_id = <<"partyID2">>,
                            shop_id = <<"shopID2">>
                        }},
                    total = make_cash(16),
                    fee_amount = make_cash(8),
                    fee = #domain_AllocationTransactionFeeShare{
                        parts = make_rational(80, 10)
                    }
                },
                details = #domain_AllocationTransactionDetails{
                    cart = AllocationCart
                }
            }
        ]
    },
    Expected = [
        #{
            <<"target">> => #{
                <<"allocationTargetType">> => <<"AllocationTargetShop">>,
                <<"shopID">> => <<"shopID1">>
            },
            <<"allocationBodyType">> => <<"AllocationBodyTotal">>,
            <<"currency">> => <<"RUB">>,
            <<"total">> => 16,
            <<"amount">> => 32,
            <<"fee">> => #{
                <<"target">> => #{
                    <<"allocationTargetType">> => <<"AllocationTargetShop">>,
                    <<"shopID">> => <<"shopID2">>
                },
                <<"allocationFeeType">> => <<"AllocationFeeShare">>,
                <<"amount">> => 8,
                <<"share">> => decode_parts(make_rational(80, 10))
            },
            <<"cart">> => anapi_handler_decoder_invoicing:decode_invoice_cart(AllocationCart)
        }
    ],
    Result = decode(Allocation),
    ?assertEqual(Expected, Result).

make_cash(Amount) -> #domain_Cash{amount = Amount, currency = #domain_CurrencyRef{symbolic_code = <<"RUB">>}}.
make_rational(P, Q) -> #'Rational'{p = P, q = Q}.

-endif.
