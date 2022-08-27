# I have made changes here that were recommended by my son
# KL April 30, 2019
# changing the executable name to be more consistent with 
#  what is needed for the python codes
#  
LDARGS =
OBJFILES = RinexSNRv2.o librariesSNRv2.o moving_sites.o get_azel.o read_header_20obs.o read_block_gps.o write_to_file.o unixlib.o
# allows 25 observations
OBJFILES2 = RinexSNRv2_25obs.o librariesSNRv2.o moving_sites.o get_azel.o read_header_25obs.o read_block_gps_25obs.o write_to_file.o unixlib.o


%.o: %.f
	gfortran -c $<

#gpsSNR: $(OBJFILES)
# unfortunately this static executable build does not work on a mac - but it will compile on your machine
gpsSNR_olderversion: $(OBJFILES)
	gfortran     $(OBJFILES) -o gpsSNR.e
#	gfortran  -static-libgfortran -static-libgcc    $(OBJFILES) -o gpsSNR.e

# new version as of 2022 august 27
gpsSNR: $(OBJFILES2)
	gfortran     $(OBJFILES2) -o gpsSNR.e
clean:
	rm -f *.o *.e
