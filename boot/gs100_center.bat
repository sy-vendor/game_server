cd ../config
start werl +P 1024000 +K true -hidden -smp +S 1 -name node100@192.168.1.135 -setcookie sy_kf_center -boot start_sasl -config ../config/elog -pa ../ebin -s main start -extra kfcenter 192.168.1.135 9600 100
exit

