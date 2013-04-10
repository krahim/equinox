### note the hard coded directory where the shared object is placed.
##

FSOURCES = vernalR.f dtomdhmR.f dtoymdR.f orbparR.f
FOBJS = $(FSOURCES:.f=.o) 

FLAGS = -O2 -funroll-loops -fPIC 
##FFLAGS = -WSurprising

## the code will give type conversion warnings.
##WARNINGS = -Wall

### if you build this in windows with rtools, you should set equinox.dll
## and perhaps change the location ~/RLibs/
## to a location on the C drive.
SHOBJ = equinox.so


all: $(FOBJS) 
	gfortran --shared $(FOBJS) -o $(SHOBJ) 
	cp $(SHOBJ) ~/RLibs/

$(FOBJS): $(FSOURCES)
	gfortran $(WARNINGS) $(FLAGS) -c $(FSOURCES)

clean:
	rm $(SHOBJ) *.o
