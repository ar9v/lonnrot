%.run: %.o main.o
	gcc main.o $< -o $@

main.o: main.c
	gcc -c main.c -o main.o

%.o: %.s
	nasm -f elf64 -o $@ $<

%.s: %.rkt
	racket -t compile-file.rkt -m $< > $@

clean:
	rm *.o *.s *.run
