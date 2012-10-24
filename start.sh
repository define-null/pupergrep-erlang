#!/bin/sh

cd `dirname $0`
echo $PWD
exec erl -name some -pa $PWD/ebin -pa $PWD/deps/*/ebin -boot start_sasl -s loggrep start
