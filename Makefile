compile:
	@ mkdir -p ebin
	@ erlc -o ebin src/httpd*.erl

clean:
	@ rm -rf ebin

run: compile
	@ erl -pa ebin -noshell -s httpd start -s init stop
