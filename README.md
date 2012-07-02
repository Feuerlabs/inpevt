exodev_inpevt
=============

Linux input event port driver and erlang app

FOR EPX
epx_nif.h
{ key_press, unicode(int)/atom(epx_nif.h), [ modifier (shift/graph/num/cap) ], key-code }
When window opens (mapped).

fnotify_srv:start().
application:start(inpevt).
{_, [_, {_, P, _, _, _, _, _, _, _, _, _, _} | _ ] } = inpevt:get_devices(key, left).
inpevt:subscribe(P, self()).
