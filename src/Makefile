bin_dir = ../bin

COMPILE   = /opt/ghc/8.0.2/bin/ghc
LINK		= /opt/ghc/8.0.2/bin/ghc

.SUFFIXES:
.SUFFIXES: .o .hs

.hs.o:
	$(COMPILE) $(CFLAGS) -c -O $<

all: mucool

MuCool/c_Signals.o: MuCool/Signals.c
	g++ -std=gnu++11 -I./ -o MuCool/c_Signals.o -c MuCool/Signals.c

MuCool/c_Localize.o: MuCool/Localize.c
	g++ -std=gnu++11 -I./ -o MuCool/c_Localize.o -c MuCool/Localize.c
	#g++ -DBUILD_DLL -std=gnu++11 -I./ -o MuCool/c_Localize.o -c MuCool/Localize.c

MuCool/TestLocalization.o: MuCool/TestLocalization.c
	g++ -std=gnu++11 -I./ -o MuCool/TestLocalization.o -c MuCool/TestLocalization.c

MuCool/TestLocalization.exe: MuCool/c_Signals.o MuCool/c_Localize.o MuCool/TestLocalization.o
	g++ -std=gnu++11 -I./ -o MuCool/TestLocalization.exe MuCool/c_Signals.o MuCool/c_Localize.o MuCool/TestLocalization.o 

#mucool: MuCool/Signals.o MuCool/Localize.o MuCool/DAQmx.o MuCool/Time.o MuCool/TestLocalization.exe
mucool: MuCool/Signals.o MuCool/Localize.o MuCool/Time.o MuCool/TestLocalization.exe

liblocalize: mucool
	dllwrap.exe --output-lib=liblocalize.a --dllname=liblocalize.dll --driver-name=g++ MuCool/c_Signals.o MuCool/c_Localize.o

sbld: mucool sbld.hs
	$(COMPILE) --make -threaded -O2 -L./ -lnicaiu sbld
	#$(COMPILE) --make -threaded -O2 -L/c/Windows/System32 -lnicaiu sbld

test: mucool
	$(COMPILE) --make -threaded -O2 TestLocalization

install: all
	cp sbld $(bin_dir)

clean:
	rm -f *.o *.hi MuCool/*.o MuCool/*.hi MuCool/TestLocalization.exe

.PHONY: doc clean all
