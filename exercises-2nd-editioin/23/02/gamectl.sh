#erl -boot start_sasl -config elog4.config -pa ebin/ 
erl -boot start_sasl -pa ebin/  -s application start sellaprime
