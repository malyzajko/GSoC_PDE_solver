(TeX-add-style-hook "header"
 (lambda ()
    (LaTeX-add-environments
     "prop"
     "thm"
     "lem"
     "dfn"
     "definition"
     "algorithm"
     "example"
     "lemma")
    (TeX-add-symbols
     '("deval" 2)
     '("inteval" 3)
     '("indint" 4)
     '("dive" 1)
     '("curl" 1)
     '("grad" 1)
     '("pdc" 3)
     '("pdd" 2)
     '("pd" 2)
     '("dd" 2)
     '("lhat" 1)
     '("shat" 1)
     '("ketbra" 2)
     '("braket" 2)
     '("ket" 1)
     '("bra" 1)
     '("commute" 2)
     '("uv" 1)
     '("ptrace" 2)
     '("trace" 1)
     '("gv" 1)
     '("multequal" 4)
     '("abs" 1)
     '("avg" 1)
     '("scripty" 1)
     '("lrbraces" 1)
     '("lrbrace" 1)
     '("header" 4)
     "cotan"
     "I"
     "hammy"
     "rhohat"
     "del"
     "dbar"
     "hfield"
     "bfield"
     "efield"
     "econstant"
     "mconstant"
     "oldepsilon"
     "oldphi"
     "vaccent"
     "underdot")
    (TeX-run-style-hooks
     "calligra"
     "mathtools"
     "fancyhdr"
     "enumerate"
     "cancel"
     "multirow"
     "multicol"
     "graphics"
     "amssymb"
     "amsthm"
     "amsmath"
     ""
     "latex2e"
     "art11"
     "article"
     "11pt")))

