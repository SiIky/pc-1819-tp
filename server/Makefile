HOSTNAME := localhost

build: ebin/ ebin/gs.app
	erl -make

clean:
	$(RM) -r ebin

ebin/gs.app: src/gs.app
	cp $< -t ebin/

ebin/:
	mkdir -p $@

register:
	echo "register 1	1" | nc -N $(HOSTNAME) 4242
	echo "register 2	2" | nc -N $(HOSTNAME) 4242

.PHONY: build clean register
