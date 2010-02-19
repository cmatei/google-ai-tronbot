
all:	MyTronBot

MyTronBot: Map.lisp MyTronBot.lisp
	sbcl --dynamic-space-size 2000 --no-userinit --no-sysinit --noprint --disable-debugger --load MyTronBot.lisp --eval "(save-lisp-and-die \"MyTronBot\" :executable t :toplevel #'my-tron-bot::main)"

send:
	-rm -f ../positron.zip
	zip ../positron.zip Map.lisp MyTronBot.lisp

clean:
	-rm -f MyTronBot MyTronBot.fasl

