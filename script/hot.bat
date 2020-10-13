cd ../ebin
erl -name hot@127.0.0.1 -setcookie game -eval "case net_adm:ping('node@127.0.0.1') of pang-> io:format(\"No nodes!!!\");_->h:h() end,	halt(1)"
pause