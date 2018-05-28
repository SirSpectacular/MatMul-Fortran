build:
	ifort -std08 -O2 mm1.F90 mm2.F90 mm3.F90 main.F90 -o "main"

time_regular:
	for size in 100 250 500 1000 1500 2000 ; do \
		./main $$size 0 ; \
	done

time_dot:
	for size in 100 250 500 1000 1500 2000 ; do \
		./main $$size 1 ; \
	done

time_cache:
	for size in 100 250 500 1000 1500 2000 ; do \
		./main $$size 2 ; \
	done

time_dot_cache:
	for size in 100 250 500 1000 1500 2000 ; do \
		./main $$size 3 ; \
	done

time_matmul:
	for size in 100 250 500 1000 1500 2000 ; do \
		./main $$size 4 ; \
	done

time_all:
	make time_regular
	make time_dot
	make time_cache
	make time_dot_cache
	make time_matmul

clean:
	-rm -r *.mod *.obj