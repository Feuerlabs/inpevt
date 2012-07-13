exodev_inpevt
=============

Linux input event port driver and erlang app

FOR EPX
epx_nif.h
{ key_press, unicode(int)/atom(epx_nif.h), [ modifier (shift/graph/num/cap) ], key-code }
When window opens (mapped).

fnotify_srv:start(). application:start(inpevt). inpevt:get_devices().

{_, [{_, P, _, _, _, _, _, _, _, _, _, _} | _ ] } = inpevt:get_devices().
inpevt:subscribe(P, self()).
inpevt:unsubscribe(P, self()).
spawn(fun() -> inpevt:subscribe(P, self()), receive X -> X after 2000 -> io:format("Exit\n", []), exit(normal) end end).

rm -rf inpevt/ fnotify/ sbc.tar; tftp -g -r sbc.tar 192.168.0.199; tar xvf sbc.tar
