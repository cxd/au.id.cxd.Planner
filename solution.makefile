



all:
	@cd Planner; \
	make -f planner.makefile all;
	@cd Test; \
	make -f test.makefile all;


test:
	@cd Test; \
	make -f test.makefile all;


clean:
	rm build/*.dll
