%.run: %.o main.o
	gcc main.o char.o io.o $< -o $@

main.o: main.c
	gcc -c main.c -o main.o -fPIC
	gcc -c char.c -o char.o -fPIC
	gcc -c io.c -o io.o -fPIC

%.o: %.s
	nasm -f elf64 -o $@ $<

%.s: %.rkt
	racket -t compile-file.rkt -m $< > $@

clean:
	rm *.o *.s *.run
