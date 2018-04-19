IATGEN Readme (April 2, 2018)
=============================

iatgen (pronounced "I A T gen") is an R package and Shiny App that
builds and analyzes survey-based IATs (Implicit Association Tests;
Greenwald et al., 1998) following a procedure developed by Carpenter et
al. (2018; preprint available at <https://osf.io/9w38r/>). Specifically,
they developed procedures for configuring Qualtrics (www.qualtrics.com)
such that it can run IATs and then developed Qualtrics-friendly
JavaScript and HTML code to implement a basic IAT in Qualtrics. The R
"iatgen" package was developed as a tool for copying and pasting this
code into a Qualtrics template so that a functional IAT survey can be
built rapidly and without the errors that would come from hand-coding.

Note that iatgen is *not* a software tool for running IATs. All iatgen
IATs are run in and by Qualtrics. Also please note that the IAT code we
use is custom-built HTML and JavaScript that we created (much like other
free IAT programs available; c.f.,
<http://mgto.org/implicit-association-test-iat-experiment-software/>;
<http://www4.ncsu.edu/~awmeade/FreeIAT/FreeIAT.htm>). All of these
programs strive to provide a faithful IAT; our IATs produce results
highly consistnet with published IAT research (as demonstrated by the
Carpenter et al., 2018 preprint). However, the official IAT software
remains either Project Implicit (implicit.harvard.edu) and the
Millisecond Software (www.millisecond.com).

*Please note that iatgen is licensed for non-commercial (i.e., academic)
use only.*

Getting Started
---------------

#### Installation

iatgen can be installed on your computer using the `devtools` package.
You first need to install this package if you do not have it. In
addition, iatgen will use commands from the `stringr` package, so this
should be installed as well.

    install.packages("devtools")
    install.packages("stringr")

Next, iatgen can be installed using the `install_github()` command from
the `devtools` package:

    devtools::install_github("iatgen/iatgen")

#### Loading iatgen

iatgen can be loaded as normal with `library()`:

    library(iatgen)

#### Getting Help

That the primary functions in iatgen have built-in help documentation.
For example, detailed information on `writeIATfull()` can be obtained
with `?writeIATfull()`.

#### Shiny App

For users who do not wish to use the

License
-------

<a rel="license" href="http://creativecommons.org/licenses/by-nc/4.0/"><img alt="Creative Commons License" style="border-width:0" src="https://i.creativecommons.org/l/by-nc/4.0/88x31.png" /></a><br />This
work is licensed under a
<a rel="license" href="http://creativecommons.org/licenses/by-nc/4.0/">Creative
Commons Attribution-NonCommercial 4.0 International License</a>.
