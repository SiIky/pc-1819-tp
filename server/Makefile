build: ebin/ ebin/gs.app
	erl -make

clean:
	$(RM) -r ebin

ebin/gs.app: src/gs.app
	cp $< -t ebin/

ebin/:
	mkdir -p $@

.PHONY: build clean
