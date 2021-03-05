set terminal pdf
set output 'bsp3.pdf'
set ytics nomirror 1E-10 1E-1
set y2tics 10E-10 10

unset logscale y
set format y "% h"
set format y2 "10^{%L}"
set autoscale x
set logscale xy2
set yrange [2:3.2]
set y2range[10E-10:2]
set xlabel "n"
set ylabel "Kapital nach n-verkleinerungs Schritten"
set y2label "Abweichung von e nach n Schritten"

plot 'zinsen.dat' u 1:2 axes x1y1 t "Kapital" w lp, '' u 1:3 axes x1y2 t "Abweichung von e" w lp, exp(1) t "e"
set terminal wxt
unset output