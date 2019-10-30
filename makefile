# I have made changes here that were recommended by my son
# KL April 30, 2019
# changing the executable name to be more consistent with 
#  what is needed for the python codes
LDARGS =
OBJFILES = RinexSNRv2.o librariesSNRv2.o moving_sites.o get_azel.o read_header_20obs.o read_block_gps.o write_to_file.o unixlib.o
OBJFILES_v3 = RinexSNRv2.o librariesSNRv2.o moving_sites.o get_azel.o read_header_20obs_v3.o read_block_gps_v3.o write_to_file.o unixlib.o

.PHONY: gpsSNR
gpsSNR: gpsSNR.e

%.o: %.f
	gfortran -c $<

gpsSNR.e: $(OBJFILES)
	gfortran $(OBJFILES) -o gpsSNR.e

.PHONY: gpsSNR_v3
gpsSNR_v3: gpsSNR_v3.e

%.o: %.f
	gfortran -c $<

gpsSNR_v3.e: $(OBJFILES_v3)
	gfortran $(OBJFILES_v3) -fbacktrace -fbounds-check -o gpsSNR_v3.e

.PHONY: clean
clean:
	rm -f *.o *.e
