# Dynamic config
HOST := $(shell uname -s)-$(shell uname -m)

ifeq ($(HOST),Linux-i686)
  OK = 1
  CC = gcc
endif

ifeq ($(HOST),Linux-x86_64)
  OK = 1
  CC = gcc -m32
endif

ifeq ($(HOST),Darwin-i386)
  OK = 1
  CC = gcc -m32
endif

ifeq ($(HOST),Darwin-x86_64)
  OK = 1
  CC = gcc -m32
endif

ifeq ($(HOST),Linux-armv6l)
  OK = 1
  CC = gcc
endif

ifndef OK
$(error Can't configure for host type $(HOST))
endif
