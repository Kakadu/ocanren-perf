import numpy as np
import matplotlib.pyplot as plt

barWidth = 0.25
Latency = {}
Latency["OCanren"] = [541.803622, 72.480947, 567.752202, 502.905198, 838.775028]
Latency["Kotlin"] = [715.061, 105.079, 836.674, 719.670, 1155.466]
Latency["Racket"] = [150.6388732910156, 22.239837646484375, 334.35740966796874, 321.2711120605469, 548.3818664550781]
Allocations = {}
Allocations["OCanren"] = [12, 30, 1, 8, 22]
Allocations["Kotlin"] = [28, 6, 16, 5, 10]
Allocations["Racket"] = [29, 3, 24, 25, 17]
Legend = {}
Legend["OCanren"] = 'OCanren'
Legend["Kotlin"] = 'Kotlin'
Legend["Racket"] = 'Racket'
Pattern = {}
Pattern["OCanren"] = '+'
Pattern["Kotlin"] = '*'
Pattern["Racket"] = 'O'

bars = {}

bars["OCanren"] = np.arange(len(Latency["OCanren"]))
bars["Kotlin"] = [x + barWidth for x in bars["OCanren"]]
bars["Racket"] = [x + barWidth for x in bars["Kotlin"]]

Latency["quines"] = [12, 30, 1, 8, 22]

# Colors picked from 
# https://jfly.uni-koeln.de/color/

def renderLatency(ax):
  colors = {} 
  colors["OCanren"] = "#00B973" #blueish green
  colors["Kotlin"] = '#E69F00'  #orange
  colors["Racket"] = "white"  
  edgecolors = {} 
  edgecolors["OCanren"] = "black"
  edgecolors["Kotlin"] = "black"
  edgecolors["Racket"] = "black"

  for name in ["OCanren", "Kotlin", "Racket"]:
    ax.bar(bars[name], Latency[name], color = colors[name], width = barWidth,
      edgecolor = edgecolors[name], label = Legend[name], hatch=Pattern[name])

fig, axs = plt.subplots(2, 1)
#plt.yscale("log")
fig.set_figwidth(11)
fig.set_figheight(11)
axs[0].set_ylabel('Latency (in ms)')
axs[0].set_xticks([r + barWidth for r in range(len(Latency["OCanren"]))],
        ['3^5', 'log243', '100quines', '15twines', '2thrines'])

renderLatency(axs[0])
# axs[0].bar(br1, Latency["OCanren"], color ='r', width = barWidth,
#         edgecolor ='grey', label = Legend["OCanren"], hatch=Pattern["OCanren"])
# axs[0].bar(br2, Latency["Kotlin"], color ='g', width = barWidth,
#         edgecolor ='grey', label = Legend["Kotlin"], hatch=Pattern["Kotlin"])
# axs[0].bar(br3, Latency["Racket"], color ='b', width = barWidth,
#         edgecolor ='grey', label = Legend["Racket"], hatch=Pattern["Racket"])

axs[1].set_ylabel('allocated')
axs[1].bar(bars["OCanren"], Allocations["OCanren"], color ='r', width = barWidth,
        edgecolor ='grey', label ='IT')
axs[1].bar(bars["Kotlin"], Allocations["Kotlin"], color ='g', width = barWidth,
        edgecolor ='grey', label ='ECE')
axs[1].bar(bars["Racket"], Allocations["Racket"], color ='b', width = barWidth,
        edgecolor ='grey', label ='CSE')

axs[1].set_xticks([], [])
axs[1].invert_yaxis()

fig.tight_layout()
handles, labels = axs[0].get_legend_handles_labels()
# reverse to keep order consistent
axs[0].legend(reversed(handles), reversed(labels), loc='upper right')

plt.savefig('fig2.png')

fig, ax = plt.subplots(layout='constrained')
#fig.set_figwidth(10)
#fig.set_figheight(15)

ax.set_ylabel('Latency (in ms)')
ax.set_xticks([r + barWidth for r in range(len(Latency["OCanren"]))],
        ['3^5', 'log243', '100quines', '15twines', '2thrines'])
ax.set_xticks([r + barWidth for r in range(len(Latency["OCanren"]))],
        ['3^5', 'log243', '100quines', '15twines', '2thrines'])
renderLatency(ax)
handles, labels = ax.get_legend_handles_labels()
# reverse to keep order consistent
ax.legend(handles, labels, loc='upper left', ncol=3)


# ax.bar(bars["OCanren"], Latency["OCanren"], color ='r', width = barWidth,
#         edgecolor ='grey', label = Legend["OCanren"], hatch=Pattern["OCanren"])
# ax.bar(bars["Kotlin"], Latency["Kotlin"], color ='g', width = barWidth,
#         edgecolor ='grey', label = Legend["Kotlin"], hatch=Pattern["Kotlin"])
# ax.bar(bars["Racket"], Latency["Racket"], color ='b', width = barWidth,
#         edgecolor ='grey', label = Legend["Racket"], hatch=Pattern["Racket"])
# plt.yscale("log")
plt.savefig('fig.png', dpi=160)
