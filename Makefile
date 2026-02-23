all: vectr


assert.c:
	gsc -:s -o assert.c -module-ref wqy24/assert -c $(GSC-OPTIONS) . wqy24/assert.sld

vectr: assert.c
	gsc -:s -o vectr -exe -ld-options -lsoundio $(GSC-OPTIONS) . assert.c main.scm

clean:
	rm vectr
dev: GSC-OPTIONS+=-debug -warnings
dev: all
