
:: 开着服务器的情况下双击改脚本可以热更修改过的erl文件（注意下面节点名和cookie的配置对应服务器）

:: 缺点是改了.hrl文件的话是编译不到的。

:: 改了头文件需要热更的话，把下面的h:h() 改成 h:hh()


cd ../ebin
erl -name hot@192.168.1.135 -setcookie game -eval "case net_adm:ping('node10@192.168.1.135') of pang-> io:format(\"No nodes!!!\");_->h:h() end,	halt(1)"
pause