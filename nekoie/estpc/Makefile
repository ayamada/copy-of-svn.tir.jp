# $Id$

.SUFFIXES:

.PHONY: all clean

TARGET = estpc

CC = gcc

CFLAGS := -Wall -O2

CFLAGS += -DHOGE

CFLAGS += -static

LIBS =

all: $(TARGET)

$(TARGET): main.c
	$(CC) $(CFLAGS) -o $(TARGET) main.c $(LIBS)
	strip $(TARGET)

clean:
	rm -f $(TARGET)
