(TeX-add-style-hook
 "residuals-fed"
 (lambda ()
   (TeX-add-to-alist 'LaTeX-provided-class-options
                     '(("article" "letter" "12pt")))
   (TeX-add-to-alist 'LaTeX-provided-package-options
                     '(("geometry" "letterpaper" "right=1in" "left=1in" "top=1in" "bottom=1in") ("inputenc" "utf8") ("fontenc" "T1") ("url" "hyphens") ("graphicx" "pdftex") ("helvet" "scaled=.90") ("natbib" "longnamesfirst" "sort") ("todonotes" "colorinlistoftodos" "textsize=small")))
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "path")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "url")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "nolinkurl")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "hyperbaseurl")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "hyperimage")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "href")
   (add-to-list 'LaTeX-verbatim-macros-with-delims-local "path")
   (TeX-run-style-hooks
    "latex2e"
    "article"
    "art12"
    "geometry"
    "setspace"
    "inputenc"
    "fontenc"
    "amsmath"
    "url"
    "graphicx"
    "tikz"
    "mathptmx"
    "helvet"
    "courier"
    "natbib"
    "rotating"
    "caption"
    "dcolumn"
    "arydshln"
    "todonotes"
    "hyperref"
    "bigfoot")
   (TeX-add-symbols
    '("emm" 1)
    "mc")
   (LaTeX-add-labels
    "T:coverage"
    "F:scrn"
    "F:vpcts"
    "ts-eq"
    "alpha-eq")
   (LaTeX-add-bibliographies
    "/home/eric/Dropbox/mydocs/magar"))
 :latex)

