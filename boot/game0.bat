cd ../config
start werl +P 1024000 -smp +S 1 -name node0@192.168.1.135 -setcookie game -boot start_sasl -config ../config/elog -pa ../ebin -s main start -extra kfclient 192.168.1.135 9011 0
exit
