
-module(web_frontend).
-include("data.hrl").
-export([init/2]).



-spec bin_to_int(binary()) -> integer().
bin_to_int(B) ->
    erlang:list_to_integer(binary:bin_to_list(B)).

bank_statement_form() ->
    << "
<h3> Request bank-statement </h3>
  <form method=\"post\" action=\"/bank-statements/request\">
  <table>
       <tr>
       <td>
         <label for=\"bank_statement_accountnumber\"> Account number </label>
       </td>
       <td>
         <input type=\"text\" id=\"bank_statement_accountnumber\" 
                name=\"bank_statement_accountnumber\" />
       </td>
       </tr>

       <tr>
       <td>
         <label for=\"bank_statement_currency\"> Currency </label>
       </td>
       <td>
         <input type=\"text\" id=\"bank_statement_currency\" name=\"bank_statement_currency\"
                value=\"EUR\" size=3/>
       </td>
       </tr>

       <tr>
       <td>
         <label for=\"bank_statement_format\"> Number & date formatting </label>
       </td>
       <td>
         <select id=\"bank_statement_format\" name=\"bank_statement_format\">
           <option value=\"de\" selected>DE</option>
           <option value=\"en\">EN</option>
         </select>
       </td>
       </tr>
   </table>

  <input type=\"submit\" value=\"Request bank-statement\" />
</form>" >>.


-spec tx_template() -> string().
tx_template() ->
    "<tr>
      <td> ~p </td>
      <td> ~s </td>
      <td> ~s </td>
      <td> ~s </td>
      <td> ~s </td>
    </tr>".

-spec amount_to_string(money(), string(), number_formatter:locale()) -> string().
amount_to_string(Amount, Currency, Format) ->
    {ok, AmountExchanged} = exchange_service:exchange(Currency, Amount),
    AmountFormatted = number_formatter:format(Format, AmountExchanged),
    AmountFormatted++ " " ++ Currency.


%% returns the name of the person associated to the account nr
%% given by account number.
-spec name_by_account_nr(unique_id()) -> string().
name_by_account_nr(AccountNr) ->
    {ok, Account} = database:get_account(AccountNr),
    io_lib:format("~s ~s", [Account#account.firstname, Account#account.surname]).

%% returns the name of the person associated to the account
%% given by account.
-spec name_by_account(#account{}) -> string().
name_by_account(Account) ->
    % {ok, Person}  = database:get_person(Account#account.person_id),
    io_lib:format("~s ~s", [Account#account.firstname, Account#account.surname]).

-spec tx(#transaction{}, string(), number_formatter:locale()) -> string().
tx(Tx, Currency, Format) ->
    Name1 = name_by_account_nr(Tx#transaction.from_acc_nr),
    Name2 = name_by_account_nr(Tx#transaction.to_acc_nr),
    Amount = amount_to_string(Tx#transaction.amount, Currency, Format),
    Date = date_formatter:format(Format, Tx#transaction.timestamp),
    Id = Tx#transaction.id,
    io_lib:format(tx_template(), [Id, Date, Amount, Name1, Name2]).

head_template() ->
    "<p> Name: ~s </p>
     <p> Balance: ~s </p>
     <table>
      <tr>
        <th>ID</th>
        <th>Date</th>
        <th>Amount</th>
        <th>Sender</th>
        <th>Receiver</th>
      </tr> ".

back_button() ->
    "<a href=\"/\">Back </a>".

footer_template() ->
    "</table>" ++ back_button().


-spec head(#account{}, string(), number_formatter:locale()) -> string().
head(Account, Currency, Format) ->
    Amount = amount_to_string(Account#account.amount, Currency, Format),
    Name =  name_by_account(Account),
    io_lib:format(head_template(), [Name, Amount]).

-spec statement(#account{}, list(#transaction{}), string(), number_formatter:locale()) -> string().
statement(Account, Txs, Currency, Format) ->
    % TODO append all txs
    TxsString = lists:foldl(fun(Tx, Acc) -> Acc ++ tx(Tx, Currency, Format) end, "", Txs),
    io_lib:format("~s ~s ~s", [head(Account, Currency, Format), TxsString, footer_template()]).


index() ->
    io_lib:format("~s", [bank_statement_form()]).

%% /bank-statements/request
init(Req, request) ->

    {ok, KeyValuesL, _} = cowboy_req:read_urlencoded_body(Req),

    KeyValues = maps:from_list(KeyValuesL),
    AccountNumber = bin_to_int(maps:get(<<"bank_statement_accountnumber">>, KeyValues)),
    Currency = binary_to_list(maps:get(<<"bank_statement_currency">>, KeyValues)),
    Format = list_to_atom(
               binary_to_list(maps:get(<<"bank_statement_format">>, KeyValues))),

    io:format("Account number ~p~n", [AccountNumber]),
    Ret = database:get_account(AccountNumber),
    KnowsCurrency = exchange_service:knows_currency(Currency),
    case {KnowsCurrency, Ret} of
        {true, {ok, Account}} ->
            Txs = database:get_all_transactions(AccountNumber),
            Body = statement(Account, Txs, Currency, Format),
            Req2 = cowboy_req:reply(200, #{<<"content-type">> => <<"text/html">>},
                                    Body, Req),
            {ok, Req2, []};
        {false, _} ->
            Req2 = cowboy_req:reply(200, #{<<"content-type">> => <<"text/html">>},
                                    "Currency not found.<br/>" ++ back_button(), Req),
            {ok, Req2, []};
        {_, {error, not_found}} ->
            Req2 = cowboy_req:reply(200, #{<<"content-type">> => <<"text/html">>},
                                    "Account not found.<br/>" ++ back_button(), Req),
            {ok, Req2, []}
    end;

%% /index
init(Req0, index) ->
    Req = cowboy_req:reply(200,
                           #{<<"content-type">> => <<"text/html">>},
                           index(),
                           Req0),
    {ok, Req, []}.
