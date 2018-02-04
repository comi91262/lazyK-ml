lazyk : lazyk.o
	smlsharp -o lazyk lazyk.smi
lazyk.o : lazyk.sml
	smlsharp -c lazyk.sml

clean :
	rm -f lazyk.o lazyk
