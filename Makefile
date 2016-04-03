all:
	$(MAKE) -C ./step1

byte:
	$(MAKE) -C ./step1 byte

clean:
	$(MAKE) -C ./step1/ clean

fclean:
	$(MAKE) -C ./step1/ fclean

re:
	$(MAKE) -C ./step1/ re
