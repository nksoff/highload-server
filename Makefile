compile:
	@ mkdir -p ebin
	@ erlc -o ebin src/*.erl

clean:
	@ rm -rf ebin/*.beam

run: clean compile
	@ erl -pa ebin -noshell -eval 'application:start(app).'
