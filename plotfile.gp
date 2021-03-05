set terminal pdf
set output 'bsp2.pdf'
set xlabel 'x'
set ylabel 'y'
plot 'result.dat' every 10 u 2:3 title "trajectory" w lp, 'result.dat' every 20 u 2:3:($4/5):($5/5) title "velocity"  w vectors 


