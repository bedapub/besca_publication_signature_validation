BESCA revision
===

I help to address the major question #1 of the reviewer 2.

    The expression of BESCA-derived signatures should be examined in bulk RNA datasets from purified cell-types. In particular, blood cell annotations could be easily assessed in the Human Blood Atlas.

We can do two types of analysis to address this comment.

1. We can report the overlapping coefficients between BESCA signatures and BioQC
   signatures, which include tissue- and cell-type-specific gene signatures. We
   expect that similar cell types in bulk and single-cell annotations show high
   correlation. --> Done, producing an overlapping coefficient matrix
   (visualized as heatmap) and a graph showing matching bulk-single-cell
   signatures.
2. We can use data from [Human Protein
   Atlas](https://www.proteinatlas.org/about/download) to visualize the expression
   of BESCA signatures.
