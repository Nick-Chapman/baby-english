
top: output.trace

output.trace: app/*.hs
	stack run > $@
