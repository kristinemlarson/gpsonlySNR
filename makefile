# I have made changes here that were recommended by my son
LDARGS =
OBJFILES = RinexSNRv2.o librariesSNRv2.o moving_sites.o get_azel.o read_header_20obs.o read_block_gps.o write_to_file.o


%.o: %.f
	gfortran -c $<

RinexSNRv2 : $(OBJFILES)
	gfortran $(OBJFILES) -o RinexSNRv2.e
clean:
	rm -f *.o *.e
