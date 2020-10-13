cd ../config
erl -noshell -name node1_stop1@192.168.2.15 -setcookie game -eval "rpc:cast('node@127.0.0.1', game, stop, [])"
pause