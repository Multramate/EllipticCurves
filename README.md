# The Arithmetic of Elliptic Curves

## Information

I completed this project under the supervision of Prof Johannes Nicaise as part of the Undergraduate Research Opportunity Programme (UROP) over the summer of the second year of my undergraduate degree in Imperial College London.

## Abstract

In the field of *algebraic geometry*, *elliptic curves* are deeply studied rich structures with far-fetching computational applications to classical *number theory* and contemporary *cryptography*. It is a fundamental tool in Wiles' proof of *Fermat's last theorem*, as well as the main object of discussion in the *Birch and Swinnerton-Dyer conjecture*, an open problem in number theory deemed worthy of being called one of the *Millennium Prize Problems* by the *Clay Mathematics Institute*. In this project, three of the four most fundamental theorems in the arithmetic of elliptic curves, namely the *Hasse-Weil theorem*, the *Nagell-Lutz theorem*, and the *Mordell-Weil theorem*, are proven in their respective special forms. *Schoof's algorithm* for *counting rational points over Galois fields* will also be briefly discussed, allowing for an application to *integer factorisation* and *primality testing*. An introductory section and a brief appendix on *fields*, *varieties*, *curves*, and *groups* are also included for completion.

## Contents

The following is the table of contents as in the report.

### Preface
Aim, overview, style, notation, and acknowledgements.

### Introduction
The first section introduces elliptic curves while hiding away the definitions and results taken from algebraic geometry.
1. Definition - informal and formal definitions of an elliptic curve
2. Weierstrass equations - explicit equations representing an elliptic curve
3. Group law - notion of an algebraic group defined on an elliptic curve
4. Isogenies - properties of group homomorphisms between elliptic curves

### Elliptic curves over finite fields
The second section discusses elliptic curves over finite fields, which hinges on Hasse's theorem to explain Schoof's algorithm for counting rational points.
1. Hasse's theorem: inseparable isogenies - statement of the Hasse-Weil theorem for elliptic curves (group of rational points can be approximated) and first half of its proof
2. Hasse's theorem: quadratic forms - second half of the proof of Hasse's theorem
3. Riemann hypothesis - statement of and relation to the Riemann hypothesis
4. Schoof's algorithm - statement of Schoof's algorithm for counting rational points
5. Point counting - counting and computing group structure of rational points

### Elliptic curves over the rationals
The third section discusses elliptic curves over the rationals, which can be split into two parts due to the fundamental theorem of finitely generated abelian groups.
1. Nagell-Lutz theorem - statement and proof of the Nagell-Lutz theorem (criteria for group of torsion points)
2. Torsion computation - computing the torsion subgroup by the fundamental theorem
3. Reduction modulo prime - reducing the field and recomputing the torsion subgroup
4. Mordell's theorem: descent - statement of the Mordell-Weil theorem for elliptic curves (group of rational points is finitely generated) and its proof assuming the next two subsections
5. Mordell's theorem: heights - first half of the proof of Mordell's theorem
6. Mordell's theorem: weak Mordell - second half of the proof of Mordell's theorem
7. Rank computation - computing the group rank by the fundamental theorem
8. Birch and Swinnerton-Dyer conjecture - statement of the Birch and Swinnerton-Dyer conjecture

### Applications
The last section touches on some applications to classical arithmetic as well as the basics of contemporary cryptography.
1. Arithmetic - introduction to integer factorisation and primality testing with elliptic curves
2. Cryptography - introduction to cryptography and elliptic curve cryptography

### Preliminaries
Appendix A includes required algebraic preliminaries for the previous sections.
1. Rings and fields - definitions from elementary algebra and basic Galois theory
2. Algebraic varieties - definition of projective varieties and propositions from algebraic geometry
3. Algebraic curves - definition of projective curves and theorems from algebraic curves
4. Groups - definition of finitely generated groups and fundamental theorems from group theory

### Algorithm proofs
Appendix B includes proofs of two less relevant algorithms omitted from the main text.
1. Transformation of a cubic curve into Weierstrass form - proof of an algorithm of Weierstrass equations
2. Group law explicit formulae - proof of an algorithm of the group law

### Code listings
Appendix C includes listings of code in the functional Haskell programming language compiled by the Glasgow Haskell Compiler.
1. Fields.hs - data for fields and prime subfields in the second section
2. WeierstrassEquations.hs - data for Weierstrass equations in the first section
3. GroupLaw.hs - data for the group law in the first section
4. Rationals.hs - algorithms in the third section
5. Applications.hs - algorithms in the fourth section
6. Test.hs - input of computations from previous modules
7. Output.txt - output of computations from previous modules

### References
Bibliography excluding basic algebra knowledge.