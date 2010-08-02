#!/bin/sh
cd `dirname $0`
./rebar compile
exec erl -pa apps/*/ebin -boot start_sasl -config rel/overlay/etc/app.config -s reloader -s web -s dispatch_watcher -tempile root '"rel/overlay/site/templates"'
