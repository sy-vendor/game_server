cd ../config
werl +P 1024000 -smp +S 1 -name node@127.0.0.1 -setcookie game -boot start_sasl -config game -pa ../ebin -s game start -extra 127.0.0.1 9001 10
pause