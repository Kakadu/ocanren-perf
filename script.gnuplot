set terminal pdf enhanced font "Monaco,6"
set output 'graph.pdf'

set style data histogram
set style histogram cluster gap 1

set style fill solid border rgb "black"
set auto x
# Comment/uncomment to switch logscale off/on
#set logscale y
set yrange [0.01:*]
plot 'data.gnuplot' using 2:xtic(1) title col \
      , '' using  3:xtic(1) title col \
      , '' using  4:xtic(1) title col \
      , '' using  5:xtic(1) title col \
      , '' using  6:xtic(1) title col \
      , '' using  7:xtic(1) title col \

set terminal pngcairo size 1024,768 enhanced font "Monaco,14"
set output 'graph.png'

replot
