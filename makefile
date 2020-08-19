# I have made changes here that were recommended by my son
# KL April 30, 2019
# changing the executable name to be more consistent with 
#  what is needed for the python codes
#  
LDARGS =
OBJFILES = RinexSNRv2.o librariesSNRv2.o moving_sites.o get_azel.o read_header_20obs.o read_block_gps.o write_to_file.o unixlib.o


%.o: %.f
	gfortran -c $<

#gpsSNR: $(OBJFILES)
#	gfortran $(OBJFILES) -o gpsSNR.e
#	 try the static build - worked on Linux, but not MACOSX
gpsSNR: $(OBJFILES)
	gfortran -static-libgfortran $(OBJFILES) -o gpsSNR.e
clean:
	rm -f *.o *.e
