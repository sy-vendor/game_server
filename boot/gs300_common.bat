cd ../config
start werl +P 1024000 +K true -hidden -smp +S 1 -name node300@192.168.1.135 -setcookie sy_kf_center -boot start_sasl -config ../config/elog -pa ../ebin -s main start -extra kflogic 192.168.1.135 9800 300
exit
