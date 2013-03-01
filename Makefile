FSOURCES = vernalR.f dtomdhmR.f dtoymdR.f orbparR.f
FOBJS = $(FSOURCES:.f=.o) 

FLAGS = -O2 -funroll-loops -fPIC 
##FFLAGS = -WSurprising
WARNINGS = -Wall
SHOBJ = equinox.so


all: $(FOBJS) 
	gfortran --shared $(FOBJS) -o $(SHOBJ) 
	cp $(SHOBJ) ~/RLibs/

$(FOBJS): $(FSOURCES)
	gfortran $(WARNINGS) $(FLAGS) -c $(FSOURCES)

clean:
	rm $(SHOBJ) *.o
