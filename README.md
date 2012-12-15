exodev_inpevt
=============

Linux input event port driver and erlang app

FOR EPX
epx_nif.h
{ key_press, unicode(int)/atom(epx_nif.h), [ modifier (shift/graph/num/cap) ], key-code }
When window opens (mapped).

application:start(inpevt).
{ok, { device, P, _, _, _}} = inpevt:add_device("/dev/input/event17").

inpevt:subscribe(P, self()).
spawn(fun() -> inpevt:subscribe(P, self()), receive X -> io:format("MSG:~p~n", [X]) after 2000 -> io:format("Exit\n", []), exit(normal) end end).
inpevt:unsubscribe(P, self()).

