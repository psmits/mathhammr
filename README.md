mathhammr
=========

An R package full of useful functions for simulating dice rolling in [Warhammer 40k 8th edition](https://www.games-workshop.com/en-US/Warhammer-40-000).

There are 3 major functions:

- `to_attack()`
- `to_wound()`
- `to_save()`

All of these are powered by the `roll_dice()` function.

`run_examples()` lists the avaliable shiny demos.



What?
-----

Mathhammer is the name used by the 40k community for analyzing the effectiveness of individual units. For a primer, I recommend this [DakkaDakka primer](https://www.dakkadakka.com/wiki/en/Basics_of_Mathhammer). Mathhammer is really just the application of probability and statistics to wargaming -- which is about as nerdy as it gets.


Installation
------------

This package is not on CRAN. To use it, you'll have to install from GitHub.

```
library(devtools)
devtools::install_github("psmits/mathhammr")
```



Shiny Applications
------------------

[This package powers a webapp for rolling dice. It is great for horde armies.](https://psmits.shinyapps.io/dice_roller/)
