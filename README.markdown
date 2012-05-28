Checkerbot runs on the same host as a number of "serv" instances, finds them through netstat, and monitors their continued existence. It publishes a table of monitored servs on port 2309.

## Building/using

First, install Erlang, then:

    $ make
    $ ./scanner
    .. running ..

Now it's ready to go on port 2309 (might take a while to find the first servs).

## What it does

Opens 2 connections to each listening port on the localhost. One connection types "scores\n" and then waits for a list that looks like a valid scores list and then a disconnect. The second types "checkerbot\n" and expects to get either a line starting with "Hello" or one starting with "$".

If it gets both a "valid" scorechart and a Hello/Full line, it adds the port to the table.

The output on port 2309 looks like this:

    $ nc localhost 2309
    port    status      last_up                    players                         hello
    -----------------------------------------------------------------------------------------------------
    21499   down        {{2012,5,28},{23,21,58}}  (econnrefused)
    2312    up          {{2012,5,28},{23,25,11}}   bar, 1, pat, foo, full, george  Hello checkerbot 1/2.
    2311    up          {{2012,5,28},{23,25,11}}   kel, asdf, a                    Hello Player 1/1.
    5056    up          {{2012,5,28},{23,25,17}}   Gotta, I, You, A, You, We're    Hello my baby, hello my honey!
    7231    up          {{2012,5,28},{23,25,11}}   zzzzzzzzz, zzzzzzz, zzzzzz, zz  Hello Player 2/2.
    3083    down        {{2012,5,28},{23,20,22}}  (econnrefused)
    2310    up          {{2012,5,28},{23,25,11}}   zzzzzzzzzzzz2, zzzzzzzzzzzz, z  Hello Player 3/2.
    2313    up          {{2012,5,28},{23,25,11}}   fd, Hello, sean, testing        Hello Player 1/1.

## Timing

Currently the timing is controlled by the scanner:loop function (in src/scanner.erl).

It scans about every 30 seconds, and only checks for new servs that it hasn't seen before (ie, all netstat listening ports) once every 2 minutes (on average).

