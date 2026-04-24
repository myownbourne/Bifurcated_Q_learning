## Reproducing the numerical results and figures

To reproduce the numerical results, first run the corresponding simulation script `Fig_XXX.r`. This script generates the simulation results and saves them as `.csv` files in the working directory.

After obtaining the result files, run the plotting script `PLOT_models.r` to generate the figures reported in the paper. The argument `setting` in `PLOT_models.r` specifies which figure to reproduce. The correspondence between the `setting` parameter and the figures is as follows:

setting        Figure
1             Fig 3 (a) and (c)
1.5           Fig S1
2             Fig S3
22            Fig S5
3             Fig 4 (a),(b),(c),(g)
4             Fig S2
5             Fig S8
7             Fig S4
8             Fig S6
9             Fig S7

The figures for TPR and FPR are generated separately using the script `PLOT_tpr_fpr.r`:

setting       Figure
1             Fig 3 (c)
2             Fig 4 (d)
22            Fig 4 (e)
