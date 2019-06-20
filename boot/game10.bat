cd ../config
start werl +P 1024000 -smp +S 1 -name node10@192.168.1.135 -setcookie game -boot start_sasl -config ../config/elog -pa ../ebin -s main start -extra master 192.168.1.135 9001 10
exit
