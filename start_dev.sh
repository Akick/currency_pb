#!/bin/bash
./rebar3 compile
erl -sname currencynode -setcookie fcmSdsfkjkdtTHSdfh -pa include -pa ebin -pa _build/default/lib/*/ebin +pc unicode -config config/sys.config -s currencies
