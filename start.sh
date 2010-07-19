#!/bin/sh
cd `dirname $0`
exec erl -pa apps/*/ebin -boot start_sasl -s reloader -s web
