%.run: %.o main.o
	echo "FIRST"
	gcc main.o char.o $< -o $@

main.o: main.c
	echo "SECOND"
	gcc -c main.c -o main.o
	gcc -c char.c -o char.o

%.o: %.s
	echo "THIRD"
	nasm -f elf64 -o $@ $<

%.s: %.rkt
	echo "LAST"
	racket -t compile-file.rkt -m $< > $@

clean:
	rm *.o *.s *.run
