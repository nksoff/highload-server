compile:
	@ mkdir -p ebin
	@ erlc -o ebin httpd*.erl

clean:
	@ rm -rf ebin

run: compile
	@ erl -pa ebin
