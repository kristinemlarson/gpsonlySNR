August 26, 2022

Allow up to 25 observable types

Allow up to 72 satellites per epoch 


August 17, 2020
Check for really big (> 999 dB-Hz) nonsense SNR values. They are now set to zero because 
they would otherwise overrun the formatted write statement.

September 10, 2019
Adding static compile statement

March 1, 2019
Now allow up to 20 observable types in a RINEX file

February 24, 2018
Fixed a bug in a format statement that made the code crash when there 
were < 5 observables.

Warning - if you want to use the moving site file (i.e. your GPS site is on a 
glacier or a ice sheet), the station name in the RINEX file has to agree
with the name in the knut.txt file. I do not change the case of either.
The knut.txt file is current hardwired into the moving_sites.f file and will
only work for my machine. Please make sure you change the 
path for your machine.

Reminder of the output format:
Column 1 satellite number
Column 2 elevation angle
Column 3 azimuth angle
Column 4 second of the day, GPS time (i.e. no leap seconds)
Column 7 S1
Column 8 S2
Column 9 S5

Reminder on some output choices (elevation angles):
99 - data from 5-30 degrees
77 - all data > 5, L2C satellites only
88 - all data > 5
66 - all data < 30
50 - all data < 10, useful for tall high-rate sites where you only want the lowest elevation data

Look at the shellscript testit to see how to call the code. In short:

gnssSNR.e RINEX output navfile option

If you are unfamiliar with nav files and don't know how to find them,
I encourage you to contact data@unavco.org
You can also try some scripts listed here: https://www.kristinelarson.net/orbits/

Kristine
kristinem.larson@gmail.com

------------------------------------------------
January 15, 2018
I have added the ability to have moving sites, i.e. if you put
epoch (Cartesian) positions and velocities in the file called 
knut.txt, it will use those positions rather than the position 
in the RINEX file, which is usually wrong for glacier sites.
(i.e. UNAVCO uses the same position for all time). If the knut.txt
file does not exist, it does not crash. Please change the location
of the knut.txt file to where you want it to be (I have it defined
for my file system). 

NOTE: the positions/velocities of your receiver do not have to be very good. You are
just trying to get within 100 meters of the correct value. these values
are only used to calculate an elevation angle and an azimuth angle.


January 10, 2018
This is a new version of RinexSNR that reads RINEX version 2.*
files. It strips out SNR data and writes out an external column
file with S1, S2, and S5.  It includes a crude elevation and azimuth
angle usable for GPS-IR. It uses the station receiver coordinates 
in the header for these calculations. Satellite orbits are based
on the broadcast navigation file.

It skips over other GNSS signals. 

Kristine M. Larson


--------------------------------------------------------------------
June 5, 2015

RinexSNRv2 extracts SNR data from RINEX files (version 2.11)
It computes a simple azimuth and elevation angle
for easy reading into other programs, i.e. Matlab.

Output format:
satellite number, elevation angle, azimuth angle, secondsOfday 
east and north multipath reflection points (in meters) for a 
2 meter antenna and S1 S2 values in the original RINEX units, usually db-Hz

The options are easy to see in the code. Option 77 is all data for L2C only satellites.
As new satellites are launched, you need to add their names to the code.
Option 99 is for all satellites, but only below 30 degrees. Option 88 is all
satellites, all data.


I cut off at 5 degrees because I have found the data can have 
large outliers below this value, but you are welcome to modify the 
code for your own interests.

To run:
 
RinexSNRv2.e inputRinex outputSNRfileName navfile program-option

----

1. This version replaces the previous one that had a bug in the 
ephemeris calculation.  In fixing that, I took the opportunity to rewrite the code
in a more modular form. I hope this makes it easier for others to 
understand. 

2. This code ignores everything except GPS data.  If someone is willing
to give me fortran code that will calculate the orbit for a GLONASS
satellite, I am willing to modify it.

3. I do not think this code allows more than 24 satellites at a 
given epoch. I don't think that will be hard to fix, but currently the 
program exits if it finds more than 24 satellites.

4. There is a limit to the number of observables. You can use teqc
to reduce the number of observations if you run into that limitation.

5. I'm sorry but I do not speak PC. I cannot make a PC executable for you.

I would be very grateful for bug reports.

Sincerely
Kristine M. Larson
University of Colorado


January 15, 2018
I have added the ability to have moving sites, i.e. if you put
epoch (Cartesian) positions and velocities in the file called 
knut.txt, it will use those positions rather than the position 
in the RINEX file, which is usually wrong for glacier sites.
(i.e. UNAVCO uses the same position for all time). If the knut.txt
file does not exist, it does not crash. Please change the location
of the knut.txt file to where you want it to be (I have it defined
for my file system). 

NOTE: the positions/velocities of your receiver do not have to be very good. You are
just trying to get within 100 meters of the correct value. these values
are only used to calculate an elevation angle and an azimuth angle.


January 10, 2018
This is a new version of RinexSNR that reads RINEX version 2.*
files. It strips out SNR data and writes out an external column
file with S1, S2, and S5.  It includes a crude elevation and azimuth
angle usable for GPS-IR. It uses the station receiver coordinates 
in the header for these calculations. Satellite orbits are based
on the broadcast navigation file.

It skips over other GNSS signals. 

Kristine M. Larson


--------------------------------------------------------------------
June 5, 2015

RinexSNRv2 extracts SNR data from RINEX files (version 2.11)
It computes a simple azimuth and elevation angle
for easy reading into other programs, i.e. Matlab.

Output format:
satellite number, elevation angle, azimuth angle, secondsOfday 
east and north multipath reflection points (in meters) for a 
2 meter antenna and S1 S2 values in the original RINEX units, usually db-Hz

The options are easy to see in the code. Option 77 is all data for L2C only satellites.
As new satellites are launched, you need to add their names to the code.
Option 99 is for all satellites, but only below 30 degrees. Option 88 is all
satellites, all data.


I cut off at 5 degrees because I have found the data can have 
large outliers below this value, but you are welcome to modify the 
code for your own interests.

To run:
 
RinexSNRv2.e inputRinex outputSNRfileName navfile program-option

----

1. This version replaces the previous one that had a bug in the 
ephemeris calculation.  In fixing that, I took the opportunity to rewrite the code
in a more modular form. I hope this makes it easier for others to 
understand. 

2. This code ignores everything except GPS data.  If someone is willing
to give me fortran code that will calculate the orbit for a GLONASS
satellite, I am willing to modify it.

3. I do not think this code allows more than 24 satellites at a 
given epoch. I don't think that will be hard to fix, but currently the 
program exits if it finds more than 24 satellites.

4. There is a limit to the number of observables. You can use teqc
to reduce the number of observations if you run into that limitation.

5. I'm sorry but I do not speak PC. I cannot make a PC executable for you.

I would be very grateful for bug reports.

Sincerely
Kristine M. Larson
University of Colorado
