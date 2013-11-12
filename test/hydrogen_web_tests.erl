-module(hydrogen_web_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("hydrogen.hrl").

hydrogen_proplist_test_() ->
    [?_test(url_encode()),
     ?_test(base64_url()),
     ?_test(ip_to_int())].

url_encode() ->
    ?assertEqual("http%3A%2F%2Fwebsite.com%2Fstub%3Fquerystring%3Dspecial+chars%3A+%26%25%2A",
                 hydrogen_web:url_encode(url())).

base64_url() ->
    ?assertEqual(<<"aHR0cDovL3dlYnNpdGUuY29tL3N0dWI_cXVlcnlzdHJpbmc9c3BlY2lhbCBjaGFyczogJiUq">>,
                 hydrogen_web:base64_url_encode(url())),
    ?assertEqual(?TO_B(url()), hydrogen_web:base64_url_decode(
                                   hydrogen_web:base64_url_encode(url()))).

ip_to_int() ->
    ?assertEqual(2130706433, hydrogen_web:ip_to_int("127.0.0.1")).


url() ->
    "http://website.com/stub?querystring=special chars: &%*".
