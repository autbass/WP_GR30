#!/bin/bash
ls *.raw 
for  DATEI in *.raw; \
	do echo "fft Funktionsaufruf mit ${DATEI}";\
	./fft.exe ${DATEI} fourier_out.dat;\
	echo "set terminal png 
	set output './output/${DATEI//raw/png}' 
	set xlabel 'Frequenz'
	set ylabel 'dBFS' 
	set yrange [-150:0] 
	set xrange [0:12000] 
	plot 'fourier_out.dat' u 1:2 t './sound/${DATEI//.raw}' w l lw .5 
	unset output 
	unset terminal" | gnuplot;\
	echo "${DATEI//raw/png} -> FFT output erstellt";\
done;
rm "fourier_out.dat";
mv ./*.png ./output;
echo "all outputfiles in ./output"







