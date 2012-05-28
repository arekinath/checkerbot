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

## Timing

Currently the timing is controlled by the scanner:loop function (in src/scanner.erl).

It scans about every 30 seconds, and only checks for new servs that it hasn't seen before (ie, all netstat listening ports) once every 2 minutes (on average).

