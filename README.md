# Simple static erlang webserver

The server accepts only `GET`, `HEAD` requests. If requested file exists, in response you'll get its content. If you requested `/directory` and `/directory/index.html` exists, you'll get file's content, otherwise `403` code is returned.

This server was written to pass hometask in Highload course in [Technopark@mail.ru](https://park.mail.ru). The reqired tests can be found in [init/http-test-suite](https://github.com/init/http-test-suite).

Moreover, I wanter to try erlang out. So, it's my first erlang application using OTP.

## Ho to execute

You should have installed `erl`, `erlc`.

Entry point is `httpd` script which purpose is to pass environment variables to erlang script.
Arguments: `-r /server/document/root` - document path for server (root for / requests), `-c 2` - schedulers online flag for erlang (smth like processor cores).

The server is executed in `80` port, so you need `sudo` to run it.

E.g. if you'd like to run server in `httptest` directory with 4 erlang schedulers:

```
sudo ./httpd -r httptest -c 4
```
