TARGET = test
OBJ = $(TARGET).o
SRC = $(TARGET).cpp
CC = gcc
CFLAGS = -g
LDFLAGS = -nostdlib -nostartfiles -static
GLIBCDIR = /home/miha/foss/glibc/build
STARTFILES = $(GLIBCDIR)/csu/crt1.o $(GLIBCDIR)/csu/crti.o `gcc --print-file-name=crtbegin.o`
ENDFILES = `gcc --print-file-name=crtend.o` $(GLIBCDIR)/csu/crtn.o
LIBGROUP = -Wl,--start-group $(GLIBCDIR)/libc.a -lgcc -lgcc_eh -Wl,--end-group
PTHREAD = -Wl,--start-group $(GLIBCDIR)/nptl/libpthread.a -Wl,--end-group

all: $(TARGET) $(TARGET).native

$(TARGET): $(OBJ)
	$(CC) $(LDFLAGS) -o $@ $(STARTFILES) $^ $(PTHREAD) $(LIBGROUP) $(ENDFILES)

$(TARGET).native: $(OBJ)
	$(CC) -pthread -o $@ $^

$(OBJ): $(SRC)
	$(CC) $(CFLAGS) -c $^

clean:
	rm -f *.o *.~ $(TARGET)
