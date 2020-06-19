-module(statement_test).

-include_lib("eunit/include/eunit.hrl").

-include("../src/data.hrl").

setup() -> database:init_database(), ok.

cleanup(_) -> ok.

main_test_() ->
    {inorder,
     {foreach, fun setup/0, fun cleanup/1,
      [fun trivial_test/1]}}.

trivial_test(_) ->
    fun () ->
	    database:put_account(#account{account_number = 0,
					  firstname = <<"John">>,
					  surname = <<"Doe">>, amount = 1000}),
	    database:put_account(#account{account_number = 1,
					  firstname = <<"Jane">>,
					  surname = <<"Doe">>, amount = 1000}),
	    ?assertEqual((database:get_all_transactions(1)), []),
	    database:put_transaction(#transaction{id = 0,
						  timestamp = os:timestamp(),
						  from_acc_nr = 0,
						  to_acc_nr = 1,
						  amount = 1000}),
	    ?assertEqual((length(database:get_all_transactions(0))),
			 1),
	    ?assertEqual((length(database:get_all_transactions(1))),
			 1),

		{ok, John} = database:get_account(0),
		{ok, Jane} = database:get_account(1),
		Transactions = database:get_all_transactions(),
		?assertEqual(business_logic:calculate_balance(John, Transactions), 0),
		?assertEqual(business_logic:calculate_balance(Jane, Transactions), 2000)
    end.
