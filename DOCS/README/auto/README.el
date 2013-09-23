(TeX-add-style-hook "README"
 (lambda ()
    (LaTeX-add-bibitems
     "leveque"
     "darulova")
    (LaTeX-add-labels
     "sec:Intro"
     "eqn:ex1"
     "fig:stencil")
    (TeX-add-symbols
     '("verba" 1))
    (TeX-run-style-hooks
     "geometry"
     "margin=1in"
     "mathrsfs"
     "rotating"
     "figuresright"
     "longtable"
     "tikz"
     "pgf"
     "listings"
     "url"
     "setspace"
     "stmaryrd"
     "booktabs"
     "amsthm"
     "amssymb"
     "hyperref"
     "epsfig"
     "graphicx"
     "amsmath"
     ""
     "header")))

