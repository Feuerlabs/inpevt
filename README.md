exodev_inpevt
=============

Linux input event port driver and erlang app

FOR EPX
epx_nif.h
{ key_press, unicode(int)/atom(epx_nif.h), [ modifier (shift/graph/num/cap) ], key-code }
When window opens (mapped).

application:start(inpevt).
{_, P, Cap} = inpevt:open("/dev/input/event0").
inpevt:activate(P).

application:start(inpevt).
{_, P, Cap} = inpevt:open("/dev/input/event11").
inpevt:activate(P).